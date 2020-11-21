package ADB::Client::Ref;
use strict;
use warnings;

our $VERSION = '1.000';

use Carp;
use Scalar::Util qw(weaken refaddr blessed looks_like_number);
use List::Util qw(first);
use Errno qw(EINPROGRESS EWOULDBLOCK EINTR EAGAIN ECONNRESET ETIMEDOUT
             ECONNREFUSED EACCES EPERM ENETUNREACH EHOSTUNREACH);
use Storable qw(dclone);

use ADB::Client::Events qw(mainloop unloop loop_levels timer immediate);
use ADB::Client::Spawn qw($ADB);
use ADB::Client::Utils
    qw(info caller_info callers dumper display_string
       ip_port_from_addr clocktime_running
       FAILED BAD_ADB ASSERTION INFINITY
       $DEBUG $VERBOSE $QUIET $ADB_HOST $ADB_PORT),
    _prefix => "utils_", qw(addr_info);
use Socket qw(IPPROTO_TCP IPPROTO_UDP SOCK_DGRAM SOCK_STREAM SOL_SOCKET
              SO_ERROR TCP_NODELAY);
use ADB::Client::Command qw(command_check_response
                            PHASE1 PHASE2
                            SPECIAL COMMAND_NAME COMMAND FLAGS PROCESS CODE
                            EXPECT_EOF MAYBE_EOF MAYBE_MORE SERIAL TRANSPORT);
use ADB::Client::Tracker;

use Exporter::Tidy
    other	=> [qw($CALLBACK_DEFAULT
                       $ADB_HOST $ADB_PORT $ADB $ADB_SOCKET $DEBUG $VERBOSE)];

use constant {
};

our @CARP_NOT = qw(ADB::Client ADB::Client::Events);

our $BLOCK_SIZE = int(2**16);

our $CALLBACK_DEFAULT	= \&callback_default;
our $ADB_SOCKET	= undef;
our $TRANSACTION_TIMEOUT = 10;
our $CONNECTION_TIMEOUT = 10;
# How long a Client wait for a Spawn object to return a result
# After this the Client will get its response, but the Spawn object may still
# wait for up to ADB_SPAWN_TIMEOUT
our $SPAWN_TIMEOUT = 10;

use constant {
    FATAL	=> [_fatal	=> SPECIAL, \&ADB::Client::Ref::_fatal_run],
    MARKER	=> [marker	=> SPECIAL, \&_marker],
    CONNECT	=> [_connect	=> SPECIAL, \&_connect_start],
    SPAWN	=> [spawn	=> SPECIAL, \&_connect_start],
    VERSION	=> [version	=> "host:version", -1, EXPECT_EOF, \&process_version],
    KILL	=> [kill	=> "host:kill", 0, EXPECT_EOF],
};

our @COMMANDS;
our @BUILTINS = (
    # command, number of result bytes (-1: read count), expect close
    # See the index in command array constants
    FATAL,
    MARKER,
    CONNECT,
    SPAWN,
    # _close is queued close. Used for make test
    # Shouldn't be useful to a normal user since it does nothing for the
    # commands that autoclose and loses state for the ones that leave the
    # connection open for following commands
    # close() itself is not exposed to the user since it messes up the state
    # if the queue is active. Use post_activate if you want to stall the queue
    [_close		=> SPECIAL, \&_closer],
    [forget		=> SPECIAL, \&_forget],
    [resolve		=> SPECIAL, \&_resolve],
    VERSION,
    KILL,
    [features		=> "host:features", -1, EXPECT_EOF | SERIAL,
     [\&process_features, "filter"]],
    [host_features	=> "host:host-features", -1, EXPECT_EOF,
     [\&process_features, "filter"]],
    [serial		=> "host:get-serialno", -1, EXPECT_EOF | SERIAL],
    [state		=> "host:get-state", -1, EXPECT_EOF | SERIAL],
    [device_path	=> "host:get-devpath", -1, EXPECT_EOF | SERIAL],
    [devices		=> "host:devices", -1, EXPECT_EOF,   \&process_devices],
    [devices_long	=> "host:devices-l", -1, EXPECT_EOF, \&process_devices],
    [devices_track	=> "host:track-devices", -1, MAYBE_MORE, \&process_devices],
    [_transport		=> "host:transport-%s", 0, SERIAL|MAYBE_EOF],
    # Both of these work:
    # host-serial:0715f712da553032:transport-this_does_not_matter
    # host:transport:0715f712da553032
    # Make the second one available. It is less ambiguous for serials with :
    [transport_serial	=> "host:transport:%s", 0, MAYBE_EOF],
    [_tport		=> "host:tport:%s", 8, SERIAL, \&process_tport],
    [remount		=> "remount:", INFINITY, TRANSPORT|EXPECT_EOF],
    [root		=> "root:", INFINITY, TRANSPORT|EXPECT_EOF],
    [unroot		=> "unroot:", INFINITY, TRANSPORT|EXPECT_EOF],
    [connect		=> "host:connect:%s", -1, EXPECT_EOF],
    [disconnect		=> "host:disconnect:%s", -1, EXPECT_EOF],
    # all the wait-for-device variants strictly have a SERIAL version
    # But only host-serial:<serial>:wait-for-<transport> does anything special
    # and it recognizes that serial irrespective of the <transport>
    [_wait		=> "host:wait-for-%s-%s", 0,
     SERIAL|PHASE2|EXPECT_EOF],
    [verity_enable	=> "enable-verity:", 0, TRANSPORT|EXPECT_EOF],
    [verity_disable	=> "disable-verity:", 0, TRANSPORT|EXPECT_EOF],
    [reboot		=> "reboot:%s", 0, TRANSPORT|EXPECT_EOF],
    # The details of these commented out commands were not thoroughly tested
    # [sideload		=> "sideload:%s", 0, TRANSPORT|EXPECT_EOF],
    # [sideload_host	=> "sideload:%s:%s", 0, TRANSPORT|EXPECT_EOF],
    # [reconnect	=> "host:reconnect", 0, SERIAL|EXPECT_EOF],
    # [reconnect_device	=> "reconnect", 0, TRANSPORT|EXPECT_EOF],
    # [reconnect_offline	=> "host:reconnect-offline", 0, EXPECT_EOF],
    # [usb		=> "usb:", 0, TRANSPORT|EXPECT_EOF],
    # [tcpip		=> "tcpip:%s", 0, TRANSPORT|EXPECT_EOF],
    # [jdwp		=> "jdwp", 0, TRANSPORT|EXPECT_EOF],
    [forward_list	=> "host:list-forward", -1, EXPECT_EOF, \&process_forward_list],
    # These return double OKAY with a possible counted value from the second
    [forward		=> "host:forward:%s;%s", INFINITY,
     SERIAL|PHASE2|EXPECT_EOF, \&process_forward],
    [forward_norebind	=> "host:forward:norebind:%s;%s", INFINITY,
     SERIAL|PHASE2|EXPECT_EOF, \&process_forward],
    # host:killforward fails if you don't select a device. Which is silly since
    # the adb server know on which device which forward lives and indeed the
    # kill actually still works even if you give the serial of some other
    # device. So it shouldn't NEED TRANSPORT, but it does.
    [forward_kill	=> "host:killforward:%s", INFINITY, TRANSPORT|EXPECT_EOF],
    [forward_kill_all	=> "host:killforward-all", INFINITY, EXPECT_EOF],
    [reverse_list	=> "reverse:list-forward", 0, TRANSPORT|EXPECT_EOF],
    # [reverse	=> "reverse:forward:%s;%s", 0, TRANSPORT|EXPECT_EOF],
    # [reverse_norebind	=> "reverse:forward:norebind:%s;%s", 0, TRANSPORT|EXPECT_EOF],
    # [reverse_kill	=> "reverse:killforward:%s", 0, TRANSPORT|EXPECT_EOF],
    # [reverse_kill_all	=> "reverse:killforward-all", 0, TRANSPORT|EXPECT_EOF],
    # [mdns_check	=> "host:mdns:check", 0, EXPECT_EOF],
    # [mdns_services	=> "host:mdns:services", 0, EXPECT_EOF],
    # [pair		=> 'host:pair:%2$s:%1$s', 0, EXPECT_EOF],
);

my $objects = 0;

sub objects {
    return $objects;
}

END {
    # $QUIET first for easier code coverage
    info("Still have %d %s objects at program end", $objects, __PACKAGE__) if !$QUIET && $objects;
}

# Notice that the client argument isn't yet blessed at this point
sub new {
    @_ % 2 == 0 || croak "Odd number of arguments";

    my ($class, $client, %arguments) = @_;

    my $model = delete $arguments{model};
    return ref($class)->new($client, model => $model // $class->client, %arguments) if
        ref $class ne "";
    if (defined $model) {
        $model = $model->client_ref || croak "Model without client_ref";
        for my $name (
            qw(blocking adb adb_socket host port reresolve
               addr_info block_size
               connection_timeout transaction_timeout spawn_timeout)) {
            $arguments{$name} //= $model->{$name};
        }
    }
    my $blocking = (delete $arguments{blocking} // 1) ? 1 : 0;
    my $adb  = delete $arguments{adb} // $ADB;
    my $adb_socket = delete $arguments{adb_socket} // $ADB_SOCKET || 0;
    $adb_socket = looks_like_number($adb_socket) ? $adb_socket <=> 0 : 1;
    my $host = delete $arguments{host} // $ADB_HOST;
    my $port = delete $arguments{port} // $ADB_PORT;
    my $reresolve = delete $arguments{reresolve} // INFINITY;
    my $connection_timeout = delete $arguments{connection_timeout} //
        $CONNECTION_TIMEOUT;
    my $transaction_timeout = delete $arguments{transaction_timeout} //
        $TRANSACTION_TIMEOUT;
    my $spawn_timeout = delete $arguments{spawn_timeout} //
        $SPAWN_TIMEOUT;
    my $block_size = delete $arguments{block_size} || $BLOCK_SIZE;
    my $addr_info = delete $arguments{addr_info};

    croak "Unknown argument " . join(", ", keys %arguments) if %arguments;

    my $client_ref = bless {
        client		=> $client,
        blocking	=> $blocking,
        host		=> $host,
        port		=> $port,
        reresolve	=> $reresolve,
        resolve_last	=> 0,
        addr_info	=> undef,
        addr_connected	=> undef,
        connection_timeout	=> $connection_timeout,
        transaction_timeout	=> $transaction_timeout,
        spawn_timeout	=> $spawn_timeout,
        timeout		=> undef,
        adb		=> $adb,
        adb_socket	=> $adb_socket,
        block_size	=> $block_size,
        socket		=> undef,
        expect_eof	=> undef,
        sent		=> 0,
        in		=> "",
        out		=> "",
        # Invariant: !@commands => !active (or active => @commands)
        #            !active => !reading (or reading => active)
        #            active && socket <=> reading || !defined in
        #            !socket => out = ""
        active		=> undef,
        commands	=> [],
        command_retired	=> undef,
        post_activate	=> undef,
        result		=> undef,
        starter		=> undef,
        $DEBUG ? (callers => callers()) : (),
    }, $class;
    ++$objects;
    weaken($client_ref->{client});
    if ($addr_info) {
        $addr_info = dclone($addr_info);
        for my $ai (@$addr_info) {
            delete $ai->{connected};
        }
    } else {
        $addr_info = utils_addr_info($host, $port);
    }
    $client_ref->{addr_info} = $addr_info;
    $client_ref->{resolve_last} = clocktime_running();
    return $client_ref;
}

sub client {
    return shift->{client};
}

sub host {
    return shift->{host};
}

sub port {
    return shift->{port};
}

sub adb_socket {
    return shift->{adb_socket};
}

sub _addr_info {
    return shift->{addr_info};
}

sub addr_info : method {
    return dclone(shift->_addr_info);
}

sub _connection_data {
    return shift->{addr_connected};
}

sub connection_data {
    my $data = shift->_connection_data;
    return ref $data eq "" ? $data : dclone($data);
}

sub blocking {
    return shift->{blocking} if @_ <= 1;
    my $client_ref = shift;
    my $old = $client_ref->{blocking};
    $client_ref->{blocking} = shift ? 1 : 0;
    return $old;
}

sub delete {
    my ($client_ref, $deleted) = @_;

    $client_ref->close;
    $client_ref->{result} = undef;
    @{$client_ref->{commands}} = $deleted ?
        () : ADB::Client::Command->new(COMMAND_REF => FATAL);
    #if (my $client = $client_ref->client) {
    #    $$client = undef;
    #}
}

# This must NEVER return
sub fatal {
    my ($client_ref, $msg) = @_;
    $client_ref->delete;
    confess "Fatal: Assertion: $msg";
}

sub _fatal_run {
    my ($client_ref) = @_;

    # Can be non-deleted if we get here via the command queue
    $client_ref->delete;
    confess "Attempt to restart a dead ADB::Client";
}

sub DESTROY {
    --$objects;
    info("DESTROY @_") if $DEBUG;
    shift->delete(1);
}

sub callback_default {
    croak $_[1] if $_[1];
}

sub connected {
    return shift->{socket} ? 1 : 0;
}

sub callback_blocking {
    croak "Already have a blocking command pending" if
        defined shift->{result};
    my $loop_levels = loop_levels();
    return sub {
        my $client_ref = $ {shift()};
        $client_ref->fatal("No wait pending") if !defined $client_ref->{result};
        $client_ref->fatal("Result already set") if $client_ref->{result};
        $client_ref->fatal("We are not the final command") if @{$client_ref->{commands}};
        $client_ref->{result} = \@_;
        unloop($loop_levels);
    };
}

sub wait : method {
    my ($client_ref) = @_;

    # Wait should be called directly after command queue by callback_blocking
    # So this should not trigger
    $client_ref->fatal("Already have a blocking command pending") if
        defined $client_ref->{result};
    $client_ref->{result} = "";

    eval { mainloop() };
    if ($@) {
        # Died out of mainloop. Could be something completely unrelated
        my $err = $@;
        if (!defined $client_ref->{result}) {
            # Don't do fixups on fatal client_refs
            # (this die could very well be the fatality message)
            die $err if
                @{$client_ref->{commands}} &&
                $client_ref->{commands}[0]{COMMAND_REF} == FATAL;
            $client_ref->fatal("Waiting for command without expecting result after $err");
        }
        if ($client_ref->{result}) {
            # We already had a result. In case of an error we could report it
            # here, but the thing crashing mainloop is probably more important
            $client_ref->fatal("Result while commands after $err") if
                @{$client_ref->{commands}};
            $client_ref->close;
        } else {
            pop @{$client_ref->{commands}} ||
                $client_ref->fatal("No wait result but also no command after $err");
            # Also remove associated autoconnect
            pop @{$client_ref->{commands}} if
                @{$client_ref->{commands}} &&
                $client_ref->{commands}[0]{COMMAND_REF} == CONNECT &&
                !defined $client_ref->{commands}[0]{callback};
            $client_ref->close if !@{$client_ref->{commands}};
        }
        $client_ref->{result} = undef;
        die $err;
    }
    if (@{$client_ref->{commands}}) {
        if (!defined $client_ref->{result}) {
            $client_ref->fatal(
                $client_ref->{commands}[0]{COMMAND_REF} == FATAL ?
                "ADB::Client is dead but something caught the exception" :
                "Waiting for command without expecting result");
        }
        $client_ref->fatal("Command still pending while we have a result") if
            $client_ref->{result};
        $client_ref->{result} = undef;

        # We just fell out of mainloop without setting result
        # This must mean there were non-blocking commands queued before the
        # (final) blocking one, and one of them must have failed leaving no
        # activity for mainloop. (Or something else called unloop)

        # Remove the command we are waiting for since we sort of had an error
        # and we cannot expect the user to properly recover from this
        pop @{$client_ref->{commands}};
        # Also remove associated autoconnect
        pop @{$client_ref->{commands}} if
            @{$client_ref->{commands}} &&
            $client_ref->{commands}[0]{COMMAND_REF} == CONNECT &&
            !defined $client_ref->{commands}[0]{callback};
        # In case something else called unloop
        $client_ref->close if !@{$client_ref->{commands}};
        croak "A previous command in the queue failed";
    }

    my $result = $client_ref->{result} ||
        $client_ref->fatal("Exit mainloop without setting result");
    $client_ref->{result} = undef;
    # If there are no more commands we should also not be active
    $client_ref->fatal("Active during wait") if $client_ref->{active};

    # At last we feel confident that everything is in its proper state
    # croak $result->[0] =~ s{(.*) at .* line \d+\.?\n}{$1}sar if $result->[0];
    $result->[0] =~ /\n\z/ ? die $result->[0] : croak $result->[0] if $result->[0];
    wantarray || return $result->[1];
    # remove the flag indicating success
    shift @$result;
    return @$result;
}

sub commands_add {
    my ($class, $client_class) = @_;

    for my $command_ref (@BUILTINS) {
        $class->command_add($client_class, $command_ref);
    }
}

sub command_add {
    my ($class, $client_class, $command_ref) = @_;

    my $command_name = $command_ref->[COMMAND_NAME] ||
        croak("No COMMAND_NAME");
    $command_ref->[COMMAND] // croak "No COMMAND in command '$command_name'";

    if ($command_ref->[COMMAND] ne SPECIAL) {
        $command_ref->[FLAGS] // croak "No FLAGS in command '$command_name'";
        if ($command_ref->[FLAGS] & SERIAL) {
            my @command_ref = @$command_ref;
            $command_ref[FLAGS] &= ~SERIAL;
            # Instead of host:features we can also do:
            #  host-usb:features
            #  host-local:features
            #  host-serial:52000c4748d6a283:features
            for my $prefix (qw(usb local serial:%s transport-id:%d)) {
                my $ref = [@command_ref];
                $ref->[COMMAND] =~ s/:/-$prefix:/ ||
                    croak "No : in command '$ref->[COMMAND_NAME]'";
                my $suffix = $prefix;
                $suffix =~ s/:.*//; # Change transport-id:%s to transport-id
                $suffix =~ s/.*-//; # Change transport-id to id
                $ref->[COMMAND_NAME] .= "_" . $suffix;
                $class->command_add($client_class, $ref);
            }
            $command_ref = \@command_ref;
        }
        if ($command_ref->[FLAGS] & PHASE2) {
            # We currently have no code to avoid having to fake "success"
            # if we immediately get the whole answer (see process_phase1).
            # Having EXPECT_EOF allows us to delegate this to normal EOF
            # processing
            $command_ref->[FLAGS] & EXPECT_EOF or
                croak "TWHO_PHASE without EXPECT_EOF";
            my $ref = $command_ref;
            $command_ref = [$command_name, $command_ref->[COMMAND], 0,
                            MAYBE_MORE|PHASE1, sub { process_phase1($ref, @_) }];
        }
    }

    push @COMMANDS, $command_ref;
    eval { $client_class->_add_command($#COMMANDS) };
    if ($@) {
        pop @COMMANDS;
        die $@;
    }
}

sub command_get {
    my ($class, $index) = @_;

    my $command_ref = $COMMANDS[$index] ||
        croak "No command at index '$index'";
    # We already checked these in command_add
    my $command_name = $command_ref->[COMMAND_NAME] ||
        die "Assertion: No COMMAND_NAME";
    my $command = $command_ref->[COMMAND] //
        die "Assertion: No COMMAND in command '$command_name'";

    $command !~ /%(?!(?:\d+\$)?[sd])/a ||
        croak "Invalid format in command '$command_name': $command";
    return $command_name, $command =~ tr/%//, $command eq SPECIAL;
}

sub command_simple {
    my ($client_ref, $arguments, $callback, $index, $args) = @_;

    my $command_ref = $COMMANDS[$index] ||
        croak "No command at index '$index'";
    my $command = ADB::Client::Command->new(CALLBACK => $callback);
    $command->{transaction_timeout2} = delete $arguments->{transaction_timeout2}
        if $command_ref->[FLAGS] & PHASE1;

    if (ref $command_ref->[PROCESS] eq "ARRAY") {
        my $keys = $command_ref->[PROCESS];
        # Skip first (the actual processing code)
        for my $key (@$keys[1..$#$keys]) {
            $command->{$key} = delete $arguments->{$key} if exists $arguments->{$key};
        }
    }

    croak "Unknown argument " . join(", ", keys %$arguments) if %$arguments;

    croak "Already have a blocking command pending" if defined $client_ref->{result};

    $command->command_ref($command_ref, @$args);
    push @{$client_ref->{commands}}, $command;
    $client_ref->activate(1);
}

sub special_simple {
    my ($client_ref, $arguments, $callback, $index, $args) = @_;

    croak "Unknown argument " . join(", ", keys %$arguments) if %$arguments;

    my $command_ref = $COMMANDS[$index] ||
        croak "No command at index '$index'";
    my $command = ADB::Client::Command->new(COMMAND_REF	=> $command_ref,
                                            CALLBACK	=> $callback);
    $command->arguments($args);
    push @{$client_ref->{commands}}, $command;
    $client_ref->activate(1);
}
*marker = \&special_simple;
*forget = \&special_simple;
*_close = \&special_simple;

sub _fatal {
    my ($client_ref, $arguments, $callback, $index) = @_;
    croak "Unknown argument " . join(", ", keys %$arguments) if %$arguments;

    my $command_ref = $COMMANDS[$index] ||
        croak "No command at index '$index'";
    croak "Already have a blocking command pending" if defined $client_ref->{result};
    push(@{$client_ref->{commands}},
         ADB::Client::Command->new(COMMAND_REF => $command_ref));
    $client_ref->activate(1);
}

sub resolve {
    my ($client_ref, $arguments, $callback, $index) = @_;

    my %args;

    $args{addr_info} = delete $arguments->{addr_info} if
        exists $arguments->{addr_info};
    $args{host} = delete $arguments->{host} // $ADB_HOST if
        exists $arguments->{host};
    $args{port} = delete $arguments->{port} // $ADB_PORT if
        exists $arguments->{port};
    $client_ref->special_simple($arguments, $callback, $index, \%args);
}

sub _resolve {
    my ($client_ref) = @_;

    my $command = $client_ref->{commands}[0] ||
        $client_ref->fatal("No command");
    my $args = $command->arguments;
    $client_ref->{host} = $args->{host} if defined $args->{host};
    $client_ref->{port} = $args->{port} if defined $args->{port};
    my $addr_info = $args->{addr_info};
    if (!$addr_info) {
        $addr_info = utils_addr_info($client_ref->{host}, $client_ref->{port}, 1);
        if (ref $addr_info ne "ARRAY") {
            $client_ref->error($addr_info);
            return;
        }
    }
    $client_ref->{addr_info} = $addr_info;
    $client_ref->{resolve_last} = clocktime_running();
    $client_ref->success;
}

sub _closer {
    my ($client_ref) = @_;

    my $connected = $client_ref->connected;
    $client_ref->close();
    $client_ref->success($connected);
}

sub _forget {
    my ($client_ref) = @_;

    $client_ref->close();
    $client_ref->{addr_connected} = undef;
    $client_ref->success;
}

sub _connect : method {
    my ($client_ref, $arguments, $callback, $index) = @_;

    my $command_ref = $COMMANDS[$index] ||
        croak "No command at index '$index'";
    my $connector = $client_ref->connector($command_ref, $arguments, $callback);

    croak "Unknown argument " . join(", ", keys %$arguments) if %$arguments;

    croak "Already have a blocking command pending" if defined $client_ref->{result};

    push @{$client_ref->{commands}}, $connector;
    $client_ref->activate(1);
}

sub spawn {
    my ($client_ref, $arguments, $callback, $index) = @_;

    my $command_ref = $COMMANDS[$index] ||
        croak "No command at index '$index'";
    my $connector =
        $client_ref->connector($command_ref, $arguments, $callback, 1);

    croak "Unknown argument " . join(", ", keys %$arguments) if %$arguments;

    croak "Already have a blocking command pending" if defined $client_ref->{result};

    push @{$client_ref->{commands}}, $connector;
    $client_ref->activate(1);
}

sub close : method {
    my ($client_ref) = @_;

    if ($client_ref->{socket}) {
        if ($client_ref->{out} ne "") {
            $client_ref->{socket}->delete_write;
            $client_ref->{out} = "";
        }
        $client_ref->{socket}->delete_read if
            $client_ref->{active} && defined $client_ref->{in};
        $client_ref->{in} = "";
        $client_ref->{sent} = 0;
        $client_ref->{expect_eof} = undef;
        $client_ref->{socket} = undef;
        # Don't update $client_ref->{addr_connected}{connected}
        # We want that to represent the last connection attempt,
        # NOT the current connection state. Check $client_ref->{socket} instead
    }
    $client_ref->{active}  = 0;
    $client_ref->{timeout} = undef;
    $client_ref->{starter} = undef;
}

sub command_retired {
    return shift->{command_retired};
}

sub post_activate {
    my ($client_ref, $activate) = @_;

    defined $activate || croak "Missing post_activate argument";
    my $old = $client_ref->{post_activate} //
        croak "post_activate outside success or error callback";
    $client_ref->{post_activate} = $activate ? 1 : 0;
    return $old;
}

sub error {
    my $client_ref = shift;
    my $err = shift || "Unknown error";

    $client_ref->close();
    local $client_ref->{command_retired} = shift @{$client_ref->{commands}} //
        $client_ref->fatal("error without command");
    local $client_ref->{post_activate} = 0;
    $client_ref->{command_retired}->{CALLBACK}->($client_ref->{client}, $err, @_);
    $client_ref->activate if $client_ref->{post_activate};
    # Make sure not to return anything
    return;
}

# If used inside a calback (toplevel false) nothing after this call should
# change client_ref state, so typically this should be the last thing you do
# Caller is responsible to only call this with active = 0
sub success {
    my $client_ref = shift;

    $client_ref->fatal("Active during success") if $client_ref->{active};

    my $command = shift @{$client_ref->{commands}} ||
        $client_ref->fatal("Success without command");
    local $client_ref->{command_retired} = $command;

    my $result = \@_;
    my $command_ref = $command->{COMMAND_REF};
    if ($command_ref->[PROCESS]) {
        my $process = $command_ref->[PROCESS];
        $process = $process->[0] if ref $process eq "ARRAY";
        # $_[0] as first arguments since the others will typically be ignored
        $result = eval { $process->($_[0], $command_ref->[COMMAND], $client_ref, $result) };
        if ($@) {
            my $err = $@;
            $err =~ s/\s+\z//;
            my $str = display_string($_[0]);
            unshift @{$client_ref->{commands}}, $command;
            $client_ref->error("Assertion: Could not process $command_ref->[COMMAND] output $str: $err");
            return;
        }
        if (ref $result ne "ARRAY") {
            unshift @{$client_ref->{commands}}, $command;
            ref $result eq "" ||
                $client_ref->fatal("Could not process $command_ref->[COMMAND] output: Neither a string nor an ARRAY reference");
            $client_ref->error($result || "Unknown error") if defined $result;
            return;
        }
    }
    local $client_ref->{post_activate} = 1;
    $command->{CALLBACK}->($client_ref->{client}, undef, @$result);
    $client_ref->activate if $client_ref->{post_activate};
    # Make sure not to return anything
    return;
}

# If used inside a calback (toplevel false) nothing after this call should
# change client_ref state, so typically this should be the last thing you do
sub activate {
    my ($client_ref, $top_level) = @_;

    return if $client_ref->{active} || !@{$client_ref->{commands}};

    for (1) {
        my $command_ref = $client_ref->{commands}[0]->command_ref;
        if ($client_ref->{out} ne "") {
            my $response = display_string($client_ref->{out});
            $client_ref->fatal("$response to ADB still pending when starting $command_ref->[COMMAND]");
        }
        if (!defined $client_ref->{in} || $client_ref->{in} ne "") {
            my $response = display_string($client_ref->{in});
            $client_ref->fatal("$response from ADB still pending when starting $command_ref->[COMMAND]");
        }
        if ($command_ref->[COMMAND] eq SPECIAL) {
            # Special

            # First get rid of top_level.
            # It's too annoying to always have to handle that as a special case
            if ($top_level) {
                $client_ref->{timeout} = immediate(sub {
                    $client_ref->{active} ||
                        $client_ref->fatal("Something deactived but left timeout");
                    $client_ref->{timeout} = undef;
                    $client_ref->{in} = "" if $client_ref->{socket};
                    $client_ref->{active} = 0;
                    $client_ref->activate });
                # We don't expect any read here, but it's needed to maintain our
                # invariant active & socket => reader || !defined in
                $client_ref->{in} = undef if $client_ref->{socket};
                last;
            }

            my $code = $command_ref->[CODE] ||
                $client_ref->fatal("No CODE in special command '$command_ref->[COMMAND_NAME]'");
            # $code is supposed to handle {active} by itself
            $code->($client_ref);
            return;
        }
        if (!$client_ref->{socket}) {
            unshift(@{$client_ref->{commands}},
                    $client_ref->connector(CONNECT));
            redo;
        }
        $client_ref->{out} = $client_ref->{commands}[0]->out;
        info("Sending to ADB: " . display_string($client_ref->{out})) if $DEBUG;
        $client_ref->{socket}->add_write(sub { $client_ref->_writer });
        $client_ref->{socket}->add_read(sub { $client_ref->_reader });
        my $addr = $client_ref->{addr_connected} ||
            $client_ref->fatal("Socket without addr_connected");
        $client_ref->{timeout} = timer(
            $addr->{transaction_timeout} // $client_ref->{transaction_timeout},
            sub { $client_ref->_transaction_timed_out });
    }
    $client_ref->{active} = 1;
}

sub _transaction_timed_out {
    my ($client_ref) = @_;

    $client_ref->error("Operation timed out");
}

sub _marker {
    my ($client_ref) = @_;

    info("Sending (not) MARKER") if $DEBUG;
    # This can potentially lead to endless recursion through
    # activate -> success -> activate -> success ...
    # if the callback just keeps on pushing markers
    $client_ref->success;
}

sub connector {
    my ($client_ref, $command_ref, $arguments, $callback, $spawn) = @_;

    my $command = {
        COMMAND_REF	=> $command_ref,
        command_ref	=> $command_ref,
        version_min	=> delete $arguments->{version_min},
        version_max	=> delete $arguments->{version_max},
        version_scan	=> 0,
    };
    if ($spawn) {
        $command->{spawn}		= 1;
        $command->{kill}		= delete $arguments->{kill};
        $command->{adb}		= delete $arguments->{adb} // $client_ref->{adb};
        my $adb_socket		= delete $arguments->{adb_socket} // $client_ref->{adb_socket} || 0;
        $command->{adb_socket}	= looks_like_number($adb_socket) ?
            $adb_socket <=> 0 : 1;
    }

    if (defined $command->{version_min}) {
        $command->{version_min} =~ /^[1-9][0-9]*\z|^0\z/ ||
            croak "Version_min is not a natural number";
        $command->{version_min} < 2**16 ||
            croak "Version_min '$command->{version_min}' out of range";
        $command->{version_scan} ||= 1;
    }

    if (defined $command->{version_max}) {
        $command->{version_max} =~ /^[1-9][0-9]*\z|^0\z/ ||
            croak "Version_max is not a natural number";
        #$command->{version_max} < 2**16 ||
        #    croak "Version_max '$command->{version_max}' out of range";
        $command->{version_scan} ||= 1;
    }

    my $step =
        !$callback ? \&_autoconnect_done :		# autoconnect
        $spawn     ? \&_connect_step :			# spawn()
        $command->{version_scan} ? \&_connect_step :	# connect()
        # No need for complexity if there is no version_min in connect()
        undef;
    return ADB::Client::Command->new(%$command,
                                     CALLBACK => $callback) if !$step;

    $command->{callback} = $callback;
    $command->{step} = $step;
    return ADB::Client::Command->new(%$command,
                                     CALLBACK => \&_connector);
}

sub _connector {
    my $client_ref = shift->client_ref;

    # Hack! This tells the success (and error) method not to call activate
    $client_ref->post_activate(0);

    my $command = $client_ref->{command_retired};
    unshift @{$client_ref->{commands}}, $command;
    my $old_step = $command->{step};
    $command->{step} = \&_connect_step;
    $command->{COMMAND_REF} = $command->{command_ref};
    $old_step->($client_ref, $command, @_);
}

# Restore the real callback and call the success or error method on it
# Called with active = 0 and CONNECT command still on the queue
# The method is responsible for calling activate if needed
sub _connector_final {
    my $client_ref = shift;

    my $command = $client_ref->{commands}[0] ||
        $client_ref->fatal("No command");
    # Restore reresolve
    $client_ref->{reresolve} = $command->{reresolve};
    # Restore original command_ref
    $command->{COMMAND_REF} = $command->{command_ref};
    # Restore original callback
    $command->{CALLBACK} = $command->{callback};
    if ($_[0]) {
        $client_ref->{addr_connected} = $command->{addr_connected};
        $client_ref->error(@_);
    } else {
        shift;
        $client_ref->{addr_connected} = $_[0];
        # this will also call activate
        $client_ref->success(@_);
    }
}

# Called with active = 0
sub _connect_start {
    my ($client_ref) = @_;

    my $command = $client_ref->{commands}[0] ||
        $client_ref->fatal("No command");
    if ($client_ref->{reresolve} && !$client_ref->{socket}) {
        my $now = clocktime_running();
        my $age = $now - $client_ref->{resolve_last};
        if ($age >= $client_ref->{reresolve} || $age < 0) {
            my $addr_info =
                utils_addr_info($client_ref->{host}, $client_ref->{port}, 1);
            if (ref $addr_info ne "ARRAY") {
                $client_ref->error($addr_info);
                return;
            }
            $client_ref->{addr_info} = $addr_info;
            $client_ref->{resolve_last} = $now;
            $client_ref->{addr_connected} = undef;
        }
    }

    if ($command->{step}) {
        # Don't store these for auto-reconnect
        $command->{reresolve} = $client_ref->{reresolve};
        $client_ref->{reresolve} = 0;
        $command->{addr_connected} = $client_ref->{addr_connected};
    }
    $command->{address_i}	= -1;
    $command->{address} = $client_ref->{addr_connected} ?
        [$client_ref->{addr_connected}] : $client_ref->{addr_info};

    if ($client_ref->{socket}) {
        my $addr = $client_ref->{addr_connected} ||
            $client_ref->fatal("socket without addr_connected");

        if ($command->{spawn}) {
            # Maybe instead check getsockname...
            socket(my $udp, $addr->{family}, SOCK_DGRAM, IPPROTO_UDP) ||
                return $client_ref->error("Could not create UDP socket: $^E");
            bind($udp, $addr->{bind_addr0}) ||
                return $client_ref->error("Could not bind to $addr->{bind_ip} ($client_ref->{host}): $^E");
        }

        $command->{address_i} = 0;

        # We have a socket, but maybe the ADB server already closed it and
        # we never noticed since we weren't paying attention
        # (This is called with active == 0, so not selecting for read)
        for (1) {
            my $rc = sysread($client_ref->{socket}, my $buffer, $client_ref->{block_size});
            if ($rc) {
                # Nothing should be happening on the connection since we have no
                # command pending
                my $response = display_string($buffer);
                $client_ref->error("Response without having sent anything: $response");
                return;
            }
            if (defined $rc || $! == ECONNRESET) {
                # We had a socket but the ADB server had already closed it
                # so back to connecting
                $command->{address_i} = -1;
                $client_ref->close;
            } elsif ($! == EAGAIN || $! == EWOULDBLOCK) {
                # We have a socket and it's a good socket
                # don't forget this serendipitous success in the state machine!
                $client_ref->success(dclone($client_ref->{addr_connected}));
                return;
            } elsif ($! == EINTR) {
                redo;
            } else {
                $client_ref->error("Unexpected error reading from adb socket: $^E");
                return;
            }
        }
    }

    $client_ref->_connect_next;
}

# Step through state->{address} one by one trying to make a connection
# Called with active = 0
# Make sure to manage {active} as needed
sub _connect_next {
    my ($client_ref) = @_;

    my $command = $client_ref->{commands}[0] ||
        $client_ref->fatal("No command");

    while (1) {
        my $addr = $command->{address}[++$command->{address_i}];
        if (!$addr) {
            # Did we just finish a version scan ?
            if ($command->{spawn} && $command->{version_scan} > 0) {
                # If all connections have a bad version and we are not going to
                # kill then we are done
                # And $addr->{connected} implies we have a bad version since:
                #   * if version failed => we already returned an error
                #   * if version was OK => we already returned success
                last if !$command->{kill} && !first {!$_->{connected}} @{$command->{address}};
                $command->{version_scan} = -1;
                $command->{address_i} = -1;
                redo;
            }
            last;
        }

        if ($command->{spawn}) {
            if ($command->{version_scan} >= 0) {
                socket(my $udp, $addr->{family}, SOCK_DGRAM, IPPROTO_UDP) ||
                    return $client_ref->error("Could not create UDP socket: $^E");
                bind($udp, $addr->{bind_addr0}) ||
                    return $client_ref->error("Could not bind to $addr->{bind_ip} ($client_ref->{host}): $^E");
            } else {
                # Not the first loop.
                # Don't try to connect to what wasn't connectable
                # in the first loop (for kill)
                if ($addr->{connected}) {
                    $command->{kill} || last;
                } else {
                    $client_ref->_connect_step_spawn;
                    return;
                }
            }
        }

        my $socket;
        do {
            # Make sure CLOEXEC is set
            local $^F = -1;
            if (!socket($socket, $addr->{family}, SOCK_STREAM, IPPROTO_TCP)) {
                $addr->{last_connect_error} = "Socket: $^E";
                next;
            }
        };
        $socket->blocking(0);
        if ($addr->{connect_addr0}) {
            bind($socket, $addr->{connect_addr0}) ||
                return $client_ref->error("Could not bind to @{[ip_port_from_addr($addr->{connect_addr0})]}[0]: $^E");
        }
        if (connect($socket, $addr->{connect_addr})) {
            # SOL_TCP == IPPROTO_TCP
            $socket->setsockopt(IPPROTO_TCP, TCP_NODELAY, 1) //
                warn("Could not set TCP_NODELAY on connecting socket: $^E");
            $client_ref->{socket} = $socket;
            $client_ref->_connected(0) || return;
        } elsif ($! == EINPROGRESS || $! == EWOULDBLOCK) {
            setsockopt($socket, IPPROTO_TCP, TCP_NODELAY, 1) //
                warn("Could not set TCP_NODELAY on connecting socket: $^E");
            $client_ref->{socket} = $socket;
            $addr->{connected} = 0;
            $client_ref->{out} = "Dummy";
            my $callback = sub { $client_ref->_connect_writable };
            $socket->add_write($callback);
            # $socket->add_error($callback);
            $client_ref->{in} = undef;
            $client_ref->{timeout} = timer(
                $addr->{connection_timeout} // $client_ref->{connection_timeout},
                sub { $client_ref->_connection_timeout }
            );
            $client_ref->{active} = 1;
            return;
        } else {
            $client_ref->_connected($!) || return;
        }
    }

    # Final result
    $client_ref->error(
        $command->{first_error} ||
        "Assertion: No failure reason after connection attempts");
}

sub _connect_writable {
    my ($client_ref) = @_;

    # Called with active = 1
    my $command = $client_ref->{commands}[0] ||
        $client_ref->fatal("No command");

    my $addr = $command->{address}[$command->{address_i}];
    $addr->{connected} = undef;
    my $packed = getsockopt($client_ref->{socket}, SOL_SOCKET, SO_ERROR);
    if (!$packed) {
        $addr->{last_connect_error} = "Could not getsockopt(SOL_SOCKET, SO_ERROR): $!";
        $client_ref->error("Assertion: $addr->{last_connect_error}");
        return;
    }
    my $err = unpack("I", $packed);

    # We should get a final result
    if ($err == EINPROGRESS || $err == EWOULDBLOCK) {
        $addr->{last_connect_error} = "Socket writable while connection still in progress";
        $client_ref->error("Assertion: $addr->{last_connect_error}");
        return;
    }

    if ($err) {
        # Convert to dualvar
        $err = $! = $err;
        $client_ref->close;
    } else {
        # Don't just use $client_ref->close since we want to keep $client_ref->{socket}
        local $client_ref->{socket} = $client_ref->{socket};
        $client_ref->close;
    }
    $client_ref->_connected($err) || return;
    $client_ref->_connect_next;
}

sub _connection_timeout {
    my ($client_ref) = @_;

    # Called with active = 1
    my $command = $client_ref->{commands}[0] ||
        $client_ref->fatal("No command");
    my $addr = $command->{address}[$command->{address_i}];
    $addr->{connected} = undef;

    $client_ref->close;
    $! = ETIMEDOUT;
    $client_ref->_connected($!) || return;
    $client_ref->_connect_next;
}

# Result of a connection attempt must allways go through here
# Return 0: we are done
# return 1: continue with _connect_next (try the next ip/port)
# $err is expected to be a dualvar corresponding to $!
sub _connected {
    my ($client_ref, $err) = @_;

    # Called with active = 0
    my $command = $client_ref->{commands}[0] ||
        $client_ref->fatal("No command");
    my $addr = $command->{address}[$command->{address_i}];

    if ($err == 0) {
        # $client_ref->{in} = "";
        # $client_ref->{sent} = 0;
        # $client_ref->{expect_eof} = undef;
        $addr->{connected} = clocktime_running() || 1e-9;
        $client_ref->{addr_connected} = $addr;
        if (defined fileno $addr->{bind_addr}) {
            # Implies $command->{spawn}
            $client_ref->close;
            $client_ref->_connect_step_spawn;
            return 0;
        }
        $client_ref->success(dclone($addr));
        return 0;
    } else {
        $addr->{last_connect_error} = "Connect error: $err";
        my $msg = "ADB server $addr->{connect_ip} port $addr->{connect_port}: $addr->{last_connect_error}";
        if (defined fileno $addr->{bind_addr}) {
            # Implies $command->{spawn}
            # We already did getsockname on the socket during spawn_socket
            $addr->{bind_addr} = getsockname($addr->{bind_addr}) ||
                $client_ref->fatal("Cannot getsockname: $^E");
            delete $command->{spawn};
        }
        if ($err == ECONNREFUSED ||
            $err == ECONNRESET ||
            $err == EACCES ||
            $err == EPERM ||
            $err == ENETUNREACH ||
            $err == EHOSTUNREACH ||
            $err == ETIMEDOUT) {
            # "Normal" connection errors
            # Not sure about ECONNRESET, it's not documented in man 2 connect
            # but I *DO* get it from SOL_SOCKET SO_ERROR. Maybe the connection
            # succeeded but immediately after that the connection got reset ?
            # need to do a packet capture.
            # (Can be reproduced by kill -9 of adbd just before connect)

            if ($command->{spawn} && $command->{version_scan} < 1) {
                # No scan or scan finished
                $client_ref->_connect_step_spawn;
                return 0;
            }
            $command->{first_error} ||= $msg if !$command->{spawn};
            return 1;
        } else {
            $client_ref->error($msg);
            return 0;
        }
    }
}

# Called with active = 0 and CONNECT command still on the queue
# The method is responsible for calling activate if needed
sub _autoconnect_done {
    my $client_ref = shift;
    my $command = shift;

    my $c = shift @{$client_ref->{commands}} ||
        $client_ref->fatal("_autoconnect_done without command");
    $c == $command ||
        $client_ref->fatal("Inconsistent _autoconnect_done");
    if ($_[0]) {
        # this calls error on the NEXT command
        # (the one that triggered this reconnect)
        $client_ref->error(@_);
    } else {
        $client_ref->activate;
    }
}

# Called with active = 0 and CONNECT command still on the queue
# The method is responsible for calling activate if needed
# Error: All connects failed
# Success: we have a connection
sub _connect_step {
    my $client_ref = shift;
    my $command = shift;
    my $err = shift;

    if ($err) {
        # All connects failed
        $client_ref->_connector_final($err, @_);
        return;
    }

    $client_ref->{socket} || $client_ref->fatal("connection without socket");

    my $addr = $command->{address}[$command->{address_i}];
    if ($command->{version_scan} > 0) {
        delete $addr->{version};
        $command->command_ref(VERSION);
        $command->{step} = \&_connect_step_version;
    } elsif ($command->{kill}) {
        $command->command_ref(KILL);
        $command->{step} = \&_connect_step_kill;
    } else {
        # Simply call the original callback
        $client_ref->_connector_final(undef, $addr);
        return;
    }
    # We are called with a just opened connection. Don't reconnect on close
    # $client_ref->close;
    $client_ref->activate;
}

# Called with active = 0 and CONNECT command still on the queue
# The method is responsible for calling activate if needed
# $err implies some form of bad response
# typically this means you connected to a non-ADB server
sub _connect_step_version {
    my ($client_ref, $command, $err, $version) = @_;

    # Version is autoclose
    $client_ref->fatal("Still have socket") if $client_ref->{socket};

    my $addr = $command->{address}[$command->{address_i}];

    if ($err) {
        $addr->{last_connect_error} = $err;
        $command->{first_error} ||= "ADB server $addr->{connect_ip} port $addr->{connect_port}: $err";
        $client_ref->_connector_final($command->{first_error});
        return;
    }
    if (defined $command->{version_min} && $version < $command->{version_min}) {
        $err = "Version '$version' is below '$command->{version_min}'";
    } elsif (defined $command->{version_max} && $version > $command->{version_max}) {
        $err = "Version '$version' is above '$command->{version_max}'";
    } else {
        # Version is in window
        # Simply call the original callback
        $client_ref->_connector_final(undef, $addr);
        return;
    }
    # Version is too low or too high
    $addr->{last_connect_error} = $err;
    $command->{first_error} ||= "ADB server $addr->{connect_ip} port $addr->{connect_port}: $err";
    $client_ref->_connect_next;
}

# Called with active = 0 and CONNECT command still on the queue
# The method is responsible for calling activate if needed
sub _connect_step_kill {
    my ($client_ref, $command, $err) = @_;

    # Kill is autoclose
    $client_ref->fatal("Still have socket") if $client_ref->{socket};

    my $addr = $command->{address}[$command->{address_i}];

    if ($err) {
        $addr->{last_connect_error} = $err;
        $command->{first_error} ||= "ADB server $addr->{connect_ip} port $addr->{connect_port}: $err";
        $client_ref->_connector_final($command->{first_error});
        return;
    }
    $client_ref->_connect_step_spawn;
}

# Called with active = 0 and CONNECT command still on the queue
# The method is responsible for calling activate if needed
sub _connect_step_spawn {
    my ($client_ref) = @_;

    my $command = $client_ref->{commands}[0] ||
        $client_ref->fatal("No command");
    my $addr = $command->{address}[$command->{address_i}];
    my $result = ADB::Client::Spawn->join($client_ref, $addr->{bind_addr}) ||
            $client_ref->fatal("ADB::Client::SpawnRef returns false");

    if (defined fileno $addr->{bind_addr}) {
        # We already did getsockname on the socket during spawn_socket
        $addr->{bind_addr} = getsockname($addr->{bind_addr}) ||
            $client_ref->fatal("Cannot getsockname: $^E");
    }

    if (!blessed($result) || !$result->isa("ADB::Client::SpawnRef")) {
        ref $result eq "" || $client_ref->fatal("ADB::Client::SpawnRef returns invalid reference $result");
        $addr->{last_connect_error} = $result;
        $command->{first_error} ||= "ADB server $addr->{bind_ip} port $addr->{bind_port}: Spawn failed: $result";
        $client_ref->_connector_final($command->{first_error});
        return;
    }
    $client_ref->{starter} = $result;
    $client_ref->{timeout} = timer(
        $addr->{spawn_timeout} // $client_ref->{spawn_timeout},
        sub { $client_ref->_spawn_result("Operation timed out")});
    $client_ref->{active} = 1;
}

sub _spawn_result {
    my ($client_ref, $err, $more) = @_;

    # Clears timeout and sets active = 0
    $client_ref->close;

    my $command = $client_ref->{commands}[0] ||
        $client_ref->fatal("No command");
    my $addr = $command->{address}[$command->{address_i}];

    if ($err) {
        $addr->{last_connect_error} = $err;
        $command->{first_error} ||= "ADB server $addr->{bind_ip} port $addr->{bind_port}: Spawn failed: $err";
        if ($more) {
            # We didn't get OK from --reply-fd. This often means adb can't
            # do this particular bind (e.g. specific IP or IPv6)
            # Try another address/port
            # This is important for the common case of using host "localhost"
            # which resolves as ::1 and 127.0.0.1 but adb
            # (at least as of version 41) cannot do a specific bind to ::1
            # So we need to fail ::1 and fallback to 127.0.0.1
            $client_ref->_connect_next;
        } else {
            $client_ref->_connector_final("ADB server $addr->{bind_ip} port $addr->{bind_port}: Spawn failed: $err");
        }
        return;
    }
    $addr->{pid} = $more;
    # We have no idea what version the new server is
    delete $addr->{version};

    # As a sanity check we now will check the new server
    # $client_ref->_connector_final(undef, $addr);

    delete $command->{spawn};
    delete $command->{kill};
    $command->{address} = [$addr];
    $command->{address_i} = -1;
    $command->{version_scan} = 1 if $command->{version_scan};
    # Whatever happens during this final check is what we will report as error
    $command->{first_error} = undef;
    $client_ref->_connect_next;
}

sub _writer {
    my ($client_ref) = @_;

    my $rc = syswrite($client_ref->{socket}, $client_ref->{out}, $BLOCK_SIZE);
    if ($rc) {
        substr($client_ref->{out}, 0, $rc, "");
        $client_ref->{socket}->delete_write if $client_ref->{out} eq "";
        $client_ref->{sent} += $rc;
        return;
    }
    if (defined $rc) {
        $client_ref->error("Assertion: Length 0 write");
        return;
    }
    return if $! == EAGAIN || $! == EINTR || $! == EWOULDBLOCK;
    $client_ref->error("Unexpected error writing to adb socket: $^E");
}

sub _reader {
    my ($client_ref) = @_;

    my $rc = sysread($client_ref->{socket}, my $buffer, $BLOCK_SIZE);
    if (!$rc) {
        # Handle EOF and error
        if (defined $rc || $! == ECONNRESET) {
            my $command = $client_ref->{commands}[0];
            if ($client_ref->{out} ne "") {
                # Any close while we are still writing is unexpected
                my $out = display_string($client_ref->{out});
                $client_ref->error("Unexpected EOF while still writing $out to adb socket");
                return;
            }
            if ($client_ref->{expect_eof}) {
                # Logic for expect_eof should imply this is never reached
                $command || $client_ref->fatal("expect_eof without command");

                # This shouldn't happen when talking to a proper ADB Nserver
                $client_ref->{in} eq "" ||
                    $client_ref->error("Spurious response bytes: " .
                                       display_string($client_ref->{in}));

                my $str = $client_ref->{expect_eof};
                $client_ref->close();
                $client_ref->success($$str);
            } elsif ($command) {
                my $command_ref = $command->{COMMAND_REF};
                my ($error, $str) = command_check_response($command_ref, $client_ref, 0);
                return $client_ref->error($str) if $error;
                if ($client_ref->{in} ne "") {
                    $str = display_string($client_ref->{in});
                    $client_ref->error("Spurious response bytes: $str");
                    return;
                }
                $client_ref->{sent} = 0;
                if (defined $error) {
                    # Success
                    $client_ref->close;
                    $client_ref->success($str);
                } else {
                    $client_ref->error("Immediate EOF while waiting for response to $command_ref->[COMMAND]");
                }
            } else {
                # We still had the connection open while nothing was happening
                # It's fine that it gets closed
                $client_ref->close();
            }
        } elsif ($! == EAGAIN || $! == EINTR || $! == EWOULDBLOCK) {
        } else {
            # ERROR
            $client_ref->error("Unexpected error reading from adb socket: $^E");
        }
        return;
    }

    my $command = $client_ref->{commands}[0] ||
        $client_ref->fatal("No command");
    my $command_ref = $command->{COMMAND_REF};
    if ($client_ref->{out} ne "" || $client_ref->{sent} == 0) {
        $buffer = display_string($buffer);
        if ($client_ref->{out} eq "") {
            # Implies sent == 0
            $client_ref->error("Response without having sent anything: $buffer");
        } else {
            $client_ref->error("Response while command has not yet completed: $buffer");
        }
        return;
    }

    $client_ref->{in} .= $buffer;
    if ($client_ref->{expect_eof}) {
        my $spurious = display_string($client_ref->{in});
        $client_ref->error("Spurious response bytes $spurious");
        return;
    }

    my ($error, $str) = command_check_response($command_ref, $client_ref, $rc) or return;
    return $client_ref->error($str) if $error;
    if ($client_ref->{in} ne "" && !($command_ref->[FLAGS] & MAYBE_MORE)) {
        $str = display_string($client_ref->{in});
        $client_ref->error("Spurious response bytes: $str");
        return;
    }

    $client_ref->{sent} = 0;
    # Success
    if ($command_ref->[FLAGS] & EXPECT_EOF) {
        # Delay until actual EOF
        # Important for e.g. host:kill which is only finished at the EOF
        $client_ref->{expect_eof} = \$str;
        return;
    }

    # Don't expect any more input
    $client_ref->{socket}->delete_read;
    # Drop transaction timeout
    $client_ref->{timeout} = undef;
    # That should have been the only pending activity
    # (We already checked that out = "")
    $client_ref->{active} = 0;

    $client_ref->success($str);
}

sub process_version {
    my ($version, undef, $client_ref) = @_;

    # Caller will already construct an error message using the input string
    $version =~ m{^[0-9a-fA-F]{4}\z} || die "Not a 4 digit hex number";
    $version = hex $version;
    $client_ref->{addr_connected} ||
        $client_ref->fatal("version without addr_connected");
    $client_ref->{addr_connected}{version} = $version;
    return [$version];
}

sub process_features {
    my ($features, $command_name, $client_ref) = @_;

    my $filter = $client_ref->command_retired->{filter};
    $filter = { map(($_ => 1), @$filter) } if ref $filter eq "ARRAY";

    my %features;
    my @features = split /,/, $features;
    for my $feature (@features) {
        $feature =~ s/\s+\z//;
        $feature =~ s/^\s+//;
        if (!$filter || $filter->{$feature}) {
            # Count feature in case there is more than one
            # (shouldn't happen for real devices)
            ++$features{$feature};
        } else {
            $features{$feature} = 0;
        }
    }
    return [\%features, \@features, $features];
}

sub process_devices {
    my ($devices, $command_name, $client_ref) = @_;

    my $long = $command_name eq "host:devices-l";
    my (@devices, %devices);
  DEVICE:
    while ($devices =~ s{^(\S+)[^\S\n]+(\S+|no device)(?:[^\S\n]+(\S.*\S))?\n}{}a) {
        my ($serial, $state, $description) = ($1, $2, $3);
        # Possible states seem to be:
        # offline, bootloader device host recovery rescue sideload
        # unauthorized authorizing connecting unknown
        # And UsbNoPermissionsShortHelpText() is: no permissions; [<url>])
        # We will fail to parse if that last one can really happen
        die "Multiple devices with serial number $serial" if $devices{$serial};
        push @devices, $serial;
        if ($long) {
            my %description = (state => $state);
            my @description = split(" ", $description);
            for my $description (@description) {
                my ($key, $value) = $description =~ m{^([^:]+):(.*)} or last DEVICE;
                last DEVICE if exists $description{$key};
                $description{$key} = $value;
            }
            $devices{$serial} = \%description;
        } else {
            last if defined $description;
            $devices{$serial} = $state;
        }
    }
    if ($devices ne "") {
        # Get first line
        $devices =~ s{\n.*}{}s;
        $devices = display_string($devices);
        die "Could not parse $devices";
    }
    my @tracker;
    if ($command_name eq "host:track-devices" && $client_ref->isa(__PACKAGE__)) {
        my $socket = $client_ref->{socket} ||
            die "No open socket after '$command_name'";
        # Notice that close doesn't actually close the socket, it just removes
        # the reference. And since we have a copy the socket remains open
        $client_ref->close;
        @tracker = ADB::Client::Tracker->new($socket, $client_ref->command_retired->command_ref, $client_ref->{block_size}, \%devices, $client_ref->{in});
        $client_ref->{in} = "";
    }
    return [\%devices, \@devices, shift, @tracker];
}

sub process_tport {
    return [unpack("q", shift)];
}

sub process_phase1 {
    my ($command_ref_orig, $devices, $command_name, $client_ref, $result) = @_;

    my $command = $client_ref->command_retired;
    $command->{COMMAND_REF} = $command_ref_orig;

    if ($client_ref->{in} ne "" and
        my ($error, $str) =
        command_check_response($command_ref_orig,
                               $client_ref, length $client_ref->{in})) {
        # Oh, we already got the second phase result
        return $str if $error;
        if ($client_ref->{in} ne "") {
            $str = display_string($client_ref->{in});
            return "Spurious response bytes: $str";
        }
        $client_ref->{expect_eof} = \$str;
        # Wait for EOF
    }
    # need to wait for more bytes (typically when the device gets connected)
    $client_ref->{timeout} = timer(
        $command->{transaction_timeout2} // $client_ref->{transaction_timeout},
        sub { $client_ref->_transaction_timed_out });
    $client_ref->{socket}->add_read(sub { $client_ref->_reader });
    $client_ref->{sent} = 1;
    $client_ref->{active} = 1;
    return undef;
}

sub process_forward_list {
    my ($forwards) = @_;

    my %forwards;
    for (split /\n/, $forwards) {
        my ($serial, $from, $to) = /^(\S+)\s+(\S+)\s+(.*\S)\s*\z/ or
            die "Invalid forward line '$_'";
        die "Duplicate from '$from'" if exists $forwards{$from};
        $forwards{$from} = {
            serial => $serial,
            to     => $to,
        };
    }
    return [\%forwards];
}

sub process_forward {
    my ($forward) = @_;

    return [""] if $forward eq "";
    my %data = ( in => "OKAY$forward" );
    my ($error, $str) = ADB::Client::Utils::adb_check_response(\%data, length $forward, -1, EXPECT_EOF) or die "Incomplete response 'OKAY$forward'";
    return $str if $error;
    $data{in} eq "" || die "Still input left";
    return [$str];
}

1;
__END__
# Below is stub documentation for your module. You'd better edit it!

=head1 NAME

ADB::Client::Ref - Perl extension for blah blah blah

=head1 SYNOPSIS

  use ADB::Client::Ref;
  blah blah blah

=head1 DESCRIPTION

Stub documentation for ADB::Client::Ref, created by h2xs. It looks like the
author of the extension was negligent enough to leave the stub
unedited.

Blah blah blah.

=head2 EXPORT

None by default.



=head1 SEE ALSO

Mention other useful documentation such as the documentation of
related modules or operating system documentation (such as man pages
in UNIX), or any relevant external documentation such as RFCs or
standards.

If you have a mailing list set up for your module, mention it here.

If you have a web site set up for your module, mention it here.

=head1 AUTHOR

Ton Hospel, E<lt>ton@E<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2020 by Ton Hospel

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.30.3 or,
at your option, any later version of Perl 5 you may have available.

=cut
