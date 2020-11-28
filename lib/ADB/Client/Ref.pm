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
    qw(info caller_info callers dumper display_string adb_check_response
       ip_port_from_addr clocktime_running realtime adb_file_type_from_mode
       adb_mode_from_file_type
       %errno_adb %errno_native_from_adb
       %adb_mode_from_file_type %adb_file_type_from_mode
       SUCCEEDED FAILED BAD_ADB ASSERTION INFINITY
       ADB_FILE_TYPE_MASK ADB_PERMISSION_MASK EPOCH1970
       $DEBUG $VERBOSE $QUIET $ADB_HOST $ADB_PORT),
    _prefix => "utils_", qw(addr_info);
use Socket qw(IPPROTO_TCP IPPROTO_UDP SOCK_DGRAM SOCK_STREAM SOL_SOCKET
              SO_ERROR TCP_NODELAY);
use ADB::Client::Command qw
    (:flags NR_BYTES SPECIAL COMMAND_NAME COMMAND FLAGS PROCESS CODE);
use ADB::Client::Tracker;

use Exporter::Tidy
    other	=> [qw($CALLBACK_DEFAULT
                       $ADB_HOST $ADB_PORT $ADB $ADB_SOCKET $DEBUG $VERBOSE)];

our @CARP_NOT = qw(ADB::Client ADB::Client::Events ADB::Client::Timer);

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
    FATAL	=> [_fatal	=> SPECIAL, \&_fatal_run],
    # Probably should rename marker to result now that it has been generalized
    MARKER	=> [marker	=> SPECIAL, \&_marker],
    CONNECT	=> [_connect	=> SPECIAL, \&_connect_start],
    SPAWN	=> [spawn	=> SPECIAL, \&_connect_start],
    VERSION	=> [version	=> "host:version", -1, EXPECT_EOF, \&process_version],
    KILL	=> [kill	=> "host:kill", 0, EXPECT_EOF],
    FILE_TYPE_UNKNOWN	=> "UNKNOWN",
    # 92 is android ENOPROTOOPT
    SYNC_ERROR_ADB	=> $errno_adb{92},
    SYNC_ERROR_NATIVE	=> $errno_native_from_adb{92},
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
    # if the queue is active. Use post_action if you want to stall the queue
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
    [devices_track	=> "host:track-devices", -1, MAYBE_MORE,
     \&process_devices],
    [_transport		=> "host:transport-%s", 0, SERIAL|MAYBE_EOF],
    # Both of these work:
    # host-serial:0715f712da553032:transport-this_does_not_matter
    # host:transport:0715f712da553032
    # Make the second one available. It is less ambiguous for serials with :
    [transport_serial	=> "host:transport:%s", 0, MAYBE_EOF],
    [_tport		=> "host:tport:%s", 8, SERIAL, \&process_tport],
    [remount		=> "remount:", INFINITY, TRANSPORT|EXPECT_EOF|ROOT],
    [root		=> "root:", INFINITY, TRANSPORT|EXPECT_EOF],
    [unroot		=> "unroot:", INFINITY, TRANSPORT|EXPECT_EOF|ROOT],
    [connect		=> "host:connect:%s", -1, EXPECT_EOF,
     \&process_connect],
    [disconnect		=> "host:disconnect:%s", -1, EXPECT_EOF],
    # all the wait-for-XXX-YYY variants support a SERIAL flag.
    # But only host-serial:<serial>:wait-for-XXX-YYY and
    # host-transport-id:<tid>:wait-for-XXX-YYY do anything special and they
    # recognize that serial/transport irrespective of the XXX
    [_wait		=> "host:wait-for-%s-%s", 0,
     SERIAL|PHASE2|EXPECT_EOF],
    [verity_enable	=> "enable-verity:", 0, TRANSPORT|EXPECT_EOF|ROOT],
    [verity_disable	=> "disable-verity:", 0, TRANSPORT|EXPECT_EOF|ROOT],
    [reboot		=> "reboot:%s", 0, TRANSPORT|EXPECT_EOF],
    # The details of these commented out commands were not thoroughly tested
    # [sideload		=> "sideload:%s", 0, TRANSPORT|EXPECT_EOF],
    # [sideload_host	=> "sideload:%s:%s", 0, TRANSPORT|EXPECT_EOF],
    # [reconnect	=> "host:reconnect", 0, SERIAL|EXPECT_EOF],
    # [reconnect_device	=> "reconnect", 0, TRANSPORT|EXPECT_EOF],
    # [reconnect_offline	=> "host:reconnect-offline", 0, EXPECT_EOF],
    [usb		=> "usb:", INFINITY, TRANSPORT|EXPECT_EOF],
    [tcpip		=> "tcpip:%d", INFINITY, TRANSPORT|EXPECT_EOF],
    # [jdwp		=> "jdwp", 0, TRANSPORT|EXPECT_EOF],
    [forward_list	=> "host:list-forward", -1, EXPECT_EOF,
     \&process_forward_list],
    # These return double OKAY with a possible counted value from the second
    [forward		=> "host:forward:%s;%s", INFINITY,
     SERIAL|PHASE2|EXPECT_EOF, \&process_forward],
    [forward_norebind	=> "host:forward:norebind:%s;%s", INFINITY,
     SERIAL|PHASE2|EXPECT_EOF, \&process_forward],
    # host:killforward fails if you don't select a device. Which is silly since
    # the adb server knows on which device which forward lives and indeed the
    # kill actually still works even if you give the serial of some other
    # device. So it shouldn't NEED TRANSPORT, but it does.
    [forward_kill	=> "host:killforward:%s", 0,
     TRANSPORT|SERIAL|PHASE2|EXPECT_EOF],
    [forward_kill_all	=> "host:killforward-all", 0, PHASE2|EXPECT_EOF],
    [reverse_list	=> "reverse:list-forward", -1, TRANSPORT|EXPECT_EOF,
     \&process_reverse_list],
    [reverse	=> "reverse:forward:%s;%s", INFINITY,
     TRANSPORT|PHASE2|EXPECT_EOF,
     \&process_forward],
    [reverse_norebind	=> "reverse:forward:norebind:%s;%s", INFINITY,
     TRANSPORT|PHASE2|EXPECT_EOF],
    [reverse_kill	=> "reverse:killforward:%s", 0,
     TRANSPORT|PHASE2|EXPECT_EOF],
    [reverse_kill_all	=> "reverse:killforward-all", 0,
     TRANSPORT|PHASE2|EXPECT_EOF],
    [sync		=> "sync:", 0, TRANSPORT, \&process_sync],
    # [mdns_check	=> "host:mdns:check", 0, EXPECT_EOF],
    # [mdns_services	=> "host:mdns:services", 0, EXPECT_EOF],
    # [pair		=> 'host:pair:%2$s:%1$s', 0, EXPECT_EOF],
    [lstat_v1		=> "STAT", 16, SYNC|UTF8_OUT, \&process_lstat_v1],
    [stat_v2		=> "STA2", 72, SYNC|UTF8_OUT, \&process_lstat_v2],
    [lstat_v2		=> "LST2", 72, SYNC|UTF8_OUT, \&process_lstat_v2],
    [list_v1		=> "LIST", 0, SYNC|UTF8_IN|UTF8_OUT|MAYBE_MORE,
     [\&process_list_v1, qw(recursive self_parent)]],
    # Unimplemented, I have no device supporting list_v2
    # [list_v2		=> "LIS2", 0, SYNC|UTF8_IN|UTF8_OUT|MAYBE_MORE],
    [send_v1		=> "SEND", 8, SYNC|UTF8_OUT|SEND,
     [\&process_send_v1, "raw"]],
    # Unimplemented, I have no device supporting send_v2
    # [send_v2		=> "SND2", 0, SYNC|UTF8_OUT|SEND],
    [recv_v1		=> "RECV", 0, SYNC|UTF8_OUT|MAYBE_MORE|RECV,
     [\&process_recv_v1, "raw", "sink"]],
    # Unimplemented, I have no device supporting recv_v2
    # [recv_v2		=> "RCV2", 0, SYNC|UTF8_OUT|MAYBE_MORE|RECV],
    [quit		=> "QUIT", 0, SYNC|EXPECT_EOF],
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
        reader		=> undef,
        writer		=> undef,
        timeout		=> undef,
        adb		=> $adb,
        adb_socket	=> $adb_socket,
        block_size	=> $block_size,
        socket		=> undef,
        expect_eof	=> undef,
        expect_fail	=> undef,
        read_suspended	=> 0,
        write_suspended	=> 0,
        sent		=> 0,
        in		=> "",
        out		=> "",
        # Invariant: !@commands => !active (or active => @commands)
        #            !active => !reading (or reading => active)
        #            active && socket <=> reading || !defined in
        #            !socket => out = ""
        sync		=> 0,
        active		=> undef,
        commands	=> [],
        command_retired	=> undef,
        post_action	=> undef,
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

sub is_fatal {
    my ($client_ref) = @_;

    return @{$client_ref->{commands}} &&
        $client_ref->{commands}[0]{COMMAND_REF} == FATAL;
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
    my (undef, $arguments) = @_;

    croak "Already have a blocking command pending" if
        defined shift->{result};
    my $loop_levels = loop_levels();
    my $unloop = sub {
        my $client_ref = $ {shift()};
        $client_ref->fatal("No wait pending") if !defined $client_ref->{result};
        $client_ref->fatal("Result already set") if $client_ref->{result};
        $client_ref->fatal("We are not the final command") if @{$client_ref->{commands}} > 1;
        $client_ref->{result} = \@_;
        unloop($loop_levels);
    };
    my $callback = delete $arguments->{callback} // return $unloop;
    return [$unloop, ref $callback eq "ARRAY" ? @$callback : $callback];
}

sub callback_unshift {
    my ($client_ref, $arguments, $callback_new) = @_;

    my $callback = $arguments->{callback};
    $callback = $CALLBACK_DEFAULT if
        !$callback && ($arguments->{blocking} // $client_ref->{blocking});
    $arguments->{callback} = $callback ? [$callback_new, ref $callback eq "ARRAY" ? @$callback : $callback] : $callback_new;
}

sub wait : method {
    my ($client_ref) = @_;

    # Wait should be called directly after command queue by callback_blocking
    # So this should not trigger
    $client_ref->fatal("Already have a blocking command pending") if
        defined $client_ref->{result};
    $client_ref->{result} = "";

    eval { mainloop() };

    # Make sure $client_ref->{result} is reset to undef whatever happens later
    my $result = $client_ref->{result};
    $client_ref->{result} = undef;

    if ($@) {
        # Died out of mainloop. Could be something completely unrelated
        my $err = $@;
        if (!defined $result) {
            # Don't do fixups on fatal client_refs
            # (this die could very well be the fatality message)
            die $err if $client_ref->is_fatal;
            $client_ref->fatal("Waiting for command without expecting result after $err");
        }
        if ($result) {
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
        die $err;
    }
    if (@{$client_ref->{commands}}) {
        if (!defined $result) {
            $client_ref->fatal(
                $client_ref->is_fatal ?
                "ADB::Client is dead but something caught the exception" :
                "Waiting for command without expecting result");
        }
        $client_ref->fatal("Command still pending while we have a result") if
            $result;

        # We just fell out of mainloop without setting result
        # This must mean there were non-blocking commands queued before the
        # (final) blocking one, and one of them must have failed leaving no
        # activity for mainloop. (Or something else called unloop)

        # Remove the command we are waiting for since we sort of had an error
        # and we cannot expect the user to properly recover from this.
        # (even if there is no reason think this is the command that actually
        # caused the error)
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

    $result || $client_ref->fatal("Exit mainloop without setting result");
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

    ref $command_ref eq "ARRAY" ||
        croak "Command '$command_ref' is not an ARRAY reference";
    my @command_ref = @$command_ref;
    my $command_name = $command_ref[COMMAND_NAME] ||
        croak("No COMMAND_NAME");
    $command_ref[COMMAND] // croak "No COMMAND in command '$command_name'";
    if (!utf8::downgrade($command_ref[COMMAND], 1)) {
        my $command = display_string($command_ref[COMMAND]);
        croak "Command cannot be converted to native 8 bit encoding: $command";
    }

    if ($command_ref[COMMAND] ne SPECIAL) {
        $command_ref[FLAGS] // croak "No FLAGS in command '$command_name'";

        my $command = $command_ref[COMMAND];
        if ($command_ref[FLAGS] & SYNC) {
            length $command eq 4 ||
                croak "SYNC command '$command' does not have length 4";
            $command_ref[NR_BYTES] >= 0 ||
                croak "SYNC command '$command' cannot be counted";
            $command_ref[NR_BYTES] < 2**32 ||
                croak "SYNC command '$command' expected response out of range";
            # In check_response() we only have special handling for this case
            # (QUIT should be the only EXPECT_EOF SYNC command)
            croak "SYNC command '$command' execpects byes and EOF" if
                ($command_ref[FLAGS] & EXPECT_EOF) && $command_ref[NR_BYTES];
        }
        croak "Command '$command' has both SEND and RECV" if
            ($command_ref[FLAGS] & SEND) && ($command_ref[FLAGS] & RECV);

        if ($command_ref[FLAGS] & SERIAL) {
            my @command_ref = @$command_ref;
            $command_ref[FLAGS] &= ~SERIAL;
            # Instead of host:features we can also do:
            #  host-usb:features
            #  host-local:features
            #  host-serial:52000c4748d6a283:features
            for my $prefix (qw(usb local serial:%s transport-id:%d)) {
                my $command = $command_ref[COMMAND];
                $command =~ s/:/-$prefix:/ ||
                    croak "No : in command '$command_name'";
                local $command_ref[COMMAND] = $command;
                my $suffix = $prefix;
                $suffix =~ s/:.*//; # Change transport-id:%s to transport-id
                $suffix =~ s/.*-//; # Change transport-id to id
                local $command_ref[COMMAND_NAME] .= "${command_name}_$suffix";
                $class->command_add($client_class, \@command_ref);
            }
        }
        if ($command_ref[FLAGS] & PHASE2) {
            # We currently have no code to avoid having to fake "success"
            # if we immediately get the whole answer (see process_phase1).
            # Having EXPECT_EOF allows us to delegate this to normal EOF
            # processing
            $command_ref[FLAGS] & EXPECT_EOF or
                croak "PHASE2 without EXPECT_EOF";
            my $ref = [@command_ref];
            @command_ref = ($command_name, $command_ref[COMMAND], 0,
                            MAYBE_MORE|PHASE1, sub { process_phase1($ref, @_) });
        }
    }

    push @COMMANDS, \@command_ref;
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
    my $nr_args = $command =~ tr/%//;
    if ($command_ref->[FLAGS] && ($command_ref->[FLAGS] & SYNC)) {
        $nr_args =
            $command_ref->[FLAGS] & SEND ? 2 :
            $command_ref->[NR_BYTES] == 0 && ($command_ref->[FLAGS] & EXPECT_EOF) ?
            0 : 1;
    }

    $command !~ /%(?!(?:\d+\$)?[sd])/a ||
        croak "Invalid format in command '$command_name': $command";
    return $command_name, $nr_args, $command eq SPECIAL;
}

sub command_simple {
    my ($client_ref, $arguments, $callback, $index, $args) = @_;

    croak "Already have a blocking command pending" if
        defined $client_ref->{result};

    my $command_ref = $COMMANDS[$index] ||
        croak "No command at index '$index'";
    my $command = ADB::Client::Command->new(CALLBACK => $callback);
    $command->{transaction_timeout} =
        delete $arguments->{transaction_timeout} //
        $client_ref->{transaction_timeout};
    my $flags = $command_ref->[FLAGS];
    $command->{transaction_timeout2} =
        delete $arguments->{transaction_timeout2} //
        $client_ref->{transaction_timeout} if $flags & PHASE1;

    $command->{on_progress} = delete $arguments->{on_progress} if
        exists $arguments->{on_progress} &&
        ($flags & (SYNC|MAYBE_MORE)) == (SYNC|MAYBE_MORE);

    if (ref $command_ref->[PROCESS] eq "ARRAY") {
        my $keys = $command_ref->[PROCESS];
        # Skip first (the actual processing code)
        for my $key (@$keys[1..$#$keys]) {
            $command->{$key} = delete $arguments->{$key} if exists $arguments->{$key};
        }
    }

    if ($command_ref->[FLAGS] & RECV) {
        $command->{DATA} = "";
        $command->{done} = 0;
    }

    if ($command_ref->[FLAGS] & SEND) {
        $client_ref->{nr_bytes} = 0;
        $command->{mtime} = delete $arguments->{mtime};
        my $mode = delete $arguments->{mode};
        if (!defined $mode) {
            my $perms = delete $arguments->{perms} // 0644;
            my $ftype = delete $arguments->{ftype} // "REG";
            $mode = adb_mode_from_file_type($ftype) | $perms;
        }
        $mode |= adb_mode_from_file_type("REG") unless
            $mode & ADB_FILE_TYPE_MASK;
        $mode == int($mode) || croak "Mode must be an integer";
        $mode == ($mode & (ADB_FILE_TYPE_MASK|ADB_PERMISSION_MASK)) ||
            croak sprintf "Spurious bits in mode %o", $mode;
        $mode |= $adb_mode_from_file_type{REG} unless $mode & ADB_FILE_TYPE_MASK;
        adb_file_type_from_mode($mode);
        $args->[0] .= sprintf(",%d", $mode);
        $command->{source} = $command->{raw} ?
            \&_send_data_raw : \&_send_data;
    }

    if ($command->{source} || $command->{sink}) {
        # Cannot have both
        $client_ref->fatal("Both source and sink") if
            $command->{source} && $command->{sink};

        # As soon as we have bytes >= low_water we do a write. Except when the
        # read side got EOF, then we do a final write with whatever is left
        my $low_water =
            delete $arguments->{low_water} // $client_ref->{block_size};
        $low_water >= 1 || croak "low_water should be at least 1";
        # As soon as we have bytes >= high_water we stop reading
        my $high_water =
            delete $arguments->{high_water} // 3 * $client_ref->{block_size};
        $high_water >= $low_water || croak "high_water must me >= low_water";
        $command->{low_water}  = $low_water;
        $command->{high_water} = $high_water;
        $command->{suspendable} = 0;
    }

    croak "Unknown argument " . join(", ", keys %$arguments) if %$arguments;

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
*forget = \&special_simple;
*_close = \&special_simple;

sub _fatal {
    my ($client_ref, $arguments, $callback, $index) = @_;

    croak "Already have a blocking command pending" if defined $client_ref->{result};
    croak "Unknown argument " . join(", ", keys %$arguments) if %$arguments;

    my $command_ref = $COMMANDS[$index] ||
        croak "No command at index '$index'";
    push(@{$client_ref->{commands}},
         ADB::Client::Command->new(COMMAND_REF => $command_ref));
    $client_ref->activate(1);
}

sub marker {
    my ($client_ref, $arguments, $callback, $index) = @_;

    my $result = delete $arguments->{result} // [];
    ref $result eq "" || ref $result eq "ARRAY" ||
        croak "Invalid result reference $result";
    $client_ref->special_simple($arguments, $callback, $index, $result);
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

    croak "Already have a blocking command pending" if defined $client_ref->{result};

    my $command_ref = $COMMANDS[$index] ||
        croak "No command at index '$index'";
    my $connector = $client_ref->connector($command_ref, $arguments, $callback);

    croak "Unknown argument " . join(", ", keys %$arguments) if %$arguments;

    push @{$client_ref->{commands}}, $connector;
    $client_ref->activate(1);
}

sub spawn {
    my ($client_ref, $arguments, $callback, $index) = @_;

    croak "Already have a blocking command pending" if defined $client_ref->{result};

    my $command_ref = $COMMANDS[$index] ||
        croak "No command at index '$index'";
    my $connector =
        $client_ref->connector($command_ref, $arguments, $callback, 1);

    croak "Unknown argument " . join(", ", keys %$arguments) if %$arguments;

    push @{$client_ref->{commands}}, $connector;
    $client_ref->activate(1);
}

sub close : method {
    my ($client_ref) = @_;

    if ($client_ref->{socket}) {
        if ($client_ref->{out} ne "") {
            $client_ref->{writer} = undef;
            $client_ref->{out} = "";
        }
        $client_ref->{reader} = undef;
        $client_ref->{in} = "";
        $client_ref->{expect_eof} = undef;
        $client_ref->{expect_fail} = undef;
        $client_ref->{socket} = undef;
        # Don't update $client_ref->{addr_connected}{connected}
        # We want that to represent the last connection attempt,
        # NOT the current connection state. Check $client_ref->{socket} instead
    }
    $client_ref->{read_suspended} = 0;
    $client_ref->{write_suspended} = 0;
    $client_ref->{sync} = 0;
    $client_ref->{active}  = 0;
    $client_ref->{timeout} = undef;
    $client_ref->{starter} = undef;
}

sub command_current {
    return shift->{commands}[0];
}

sub post_action {
    my ($client_ref, $action) = @_;

    defined $action || croak "Missing post_action argument";
    my $old = $client_ref->{post_action} //
        croak "post_action outside success or error callback";
    $client_ref->{post_action} = $action ? 1 : 0;
    return $old;
}

sub error {
    my $client_ref = shift;
    my $err = shift || "Unknown error";

    $client_ref->close();

    my $command = $client_ref->{commands}[0] ||
        $client_ref->fatal("Error without command");
    my $callback = $command->{CALLBACK} ||
        $client_ref->fatal("Command without callback");
    unshift @_, $err;
    local $client_ref->{post_action} = 1;
    eval {
        if (ref $callback eq "ARRAY") {
            my $client = $client_ref->{client};
            for my $c (@$callback) {
                $c->($client, @_);
            }
        } else {
            $callback->($client_ref->{client}, @_);
        }
    };
    if ($@) {
        shift @{$client_ref->{commands}} //
            $client_ref->fatal("Error without command");
        die $@;
    }
    !$client_ref->{post_action} || shift @{$client_ref->{commands}} //
        $client_ref->fatal("Error without command");
    return;
}

# If used inside a calback (toplevel false) nothing after this call should
# change client_ref state, so typically this should be the last thing you do
# Caller is responsible to only call this with active = 0
sub success {
    my $client_ref = shift;

    $client_ref->fatal("Active during success") if $client_ref->{active};

    my $command = $client_ref->{commands}[0] ||
        $client_ref->fatal("Success without command");

    my $result = \@_;
    my $command_ref = $command->{COMMAND_REF};
    # Should maybe unshift undef on $result and let the callback
    # indicate error by just setting $result->[0]
    if ($command_ref->[PROCESS]) {
        my $process = $command_ref->[PROCESS];
        $process = $process->[0] if ref $process eq "ARRAY";
        # $_[0] as first arguments since the others will typically be ignored
        $result = eval { $process->($_[0], $command_ref->[COMMAND], $client_ref, $result) };
        if ($@) {
            my $err = $@;
            if ($client_ref->is_fatal) {
                # Just in case this wasn't the fatal making its way back
                $client_ref->close;
                die $err;
            }
            $err =~ s/\.?\s*\z//;
            my $str = display_string($_[0]);
            $client_ref->error("Assertion: Could not process $command_ref->[COMMAND] output $str: $err");
            return;
        }
        if (ref $result ne "ARRAY") {
            if (ref $result eq "SCALAR") {
                $client_ref->close;
                die $$result;
            }
            ref $result eq "" ||
                $client_ref->fatal("Could not process $command_ref->[COMMAND] output: Neither a string nor an ARRAY reference");
            $client_ref->error($result || "Unknown error") if defined $result;
            return;
        }
    }
    my $callback = $command->{CALLBACK} ||
        $client_ref->fatal("Command without callback");
    unshift @$result, undef;
    local $client_ref->{post_action} = 1;
    eval {
        if (ref $callback eq "ARRAY") {
            my $client = $client_ref->{client};
            for my $c (@$callback) {
                $c->($client, @$result);
            }
        } else {
            $callback->($client_ref->{client}, @$result);
        }
    };
    if ($@) {
        shift @{$client_ref->{commands}} //
            $client_ref->fatal("Success without command");
        die $@;
    }
    if ($client_ref->{post_action}) {
        shift @{$client_ref->{commands}} ||
            $client_ref->fatal("Success without command");
        $client_ref->{post_action} = 0;
        $client_ref->activate;
    }
    return;
}

# If used inside a calback (toplevel false) nothing after this call should
# change client_ref state, so typically this should be the last thing you do
sub activate {
    my ($client_ref, $top_level) = @_;

    return if $client_ref->{active} || !@{$client_ref->{commands}} || $client_ref->{post_action};

    for (1) {
        my $command = $client_ref->{commands}[0];
        my $command_ref = $command->command_ref;
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
        if ($client_ref->{sync}) {
            if ($command_ref->[FLAGS] & SYNC) {
                $client_ref->{out} = $command->out;
            } else {
                # This will fail, but we want it to fail without disrupting
                # the adbd server state and with a clear message
                $client_ref->{out} = pack("a44x", $command_ref->[COMMAND]);
            }
        } else {
            if ($command_ref->[FLAGS] & SYNC) {
                # This will fail,but we want it to fail without disrupting
                # the adbd server state and with a clear message
                $client_ref->{out} = sprintf("%04X", length($command_ref->[COMMAND])) . $command_ref->[COMMAND];
            } else {
                $client_ref->{out} = $command->out;
            }
        }
        $client_ref->{sent} = 0;
        info("Sending to ADB: " . display_string($client_ref->{out})) if $DEBUG;

        $client_ref->{writer} = $client_ref->{socket}->add_write(\&_writer, $client_ref);
        $client_ref->{reader} = $client_ref->{socket}->add_read(\&_reader, $client_ref);
        $client_ref->{timeout} = timer(
            $command->{transaction_timeout} //
            $client_ref->fatal("No transaction_timeout"),
            sub { $client_ref->_transaction_timed_out });

        $command->{source}->($client_ref, $command) if
            $command->{source} &&
            length $client_ref->{out} < $command->{high_water};
    }
    $client_ref->{active} = 1;
}

sub _send_data {
    my ($client_ref, $command) = @_;

    my $arguments = $command->arguments;
    my $length = length $arguments->[1];
    if ($length) {
        my $want = $command->{high_water} - length $client_ref->{out};
        $want > 0 || $client_ref->fatal("Bad water calculation: $want <= 0");
        while (1) {
            $length = $want if $length > $want;
            $length = 2**16 if $length > 2**16;
            $client_ref->{out} .= pack("a4V", "DATA", $length);
            $client_ref->{out} .= substr($arguments->[1], 0, $length, "");
            $client_ref->{nr_bytes} += $length;
            $want = $command->{high_water} - length $client_ref->{out};
            last if $want <= 0;
            $length = length $arguments->[1] || last;
        }
    }
    if ($arguments->[1] eq "") {
        delete $command->{source};
        $client_ref->{out} .=
            pack("a4V", "DONE",
                 ($command->{mtime} //= int(realtime())) - EPOCH1970);
    }
}

sub _send_data_raw {
    my ($client_ref, $command) = @_;

    my $arguments = $command->arguments;
    my $length = length $arguments->[1];
    if ($length) {
        my $want = $command->{high_water} - length $client_ref->{out};
        $want > 0 || $client_ref->fatal("Bad water calculation: $want <= 0");
        $length = $want if $length > $want;
        $client_ref->{out} .= substr($arguments->[1], 0, $length, "");
        $client_ref->{nr_bytes} += $length;
    }
    delete $command->{source} if $arguments->[1] eq "";
}

sub _transaction_timed_out {
    my ($client_ref) = @_;

    $client_ref->error("Operation timed out");
}

sub _marker {
    my ($client_ref) = @_;

    my $command = $client_ref->{commands}[0] ||
        $client_ref->fatal("No command");
    my $result = $command->arguments;
    # Don't confuse $command->{result} and $client_ref->{result}
    if (ref $result eq "") {
        info("Sending Error") if $DEBUG;
        $client_ref->success($result);
    } else {
        info("Sending Success") if $DEBUG;
        $client_ref->success(@$result);
    }
}

sub connector {
    my ($client_ref, $command_ref, $arguments, $callback, $spawn) = @_;

    my $command = {
        COMMAND_REF	=> $command_ref,
        command_ref	=> $command_ref,
        version_scan	=> 0,
        $callback ? (
            transaction_timeout =>
            delete $arguments->{transaction_timeout} ||
            $client_ref->{transaction_timeout},
            connection_timeout =>
            delete $arguments->{connection_timeout} ||
            $client_ref->{connection_timeout},
            version_min	=> delete $arguments->{version_min},
            version_max	=> delete $arguments->{version_max},
        ) : (
            connection_timeout => $client_ref->{connection_timeout},
        ),
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

    # Hack! This tells the success (and error) method not to drop the command
    # (and not call activate)
    $client_ref->post_action(0);

    my $command = $client_ref->{commands}[0] ;
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
            $client_ref->{writer} = $socket->add_write(\&_connect_writable, $client_ref);
            # $client_ref->{errorer} = $socket->add_error(\&_connect_writable, $client_ref);
            $client_ref->{in} = undef;
            $client_ref->{timeout} = timer(
                $addr->{connection_timeout} // $command->{connection_timeout} //
                $client_ref->fatal("No connection_timeout"),
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

sub suspend_read {
    my ($client_ref) = @_;

    $client_ref->fatal("Already suspended") if $client_ref->{read_suspended};
    $client_ref->{socket} || $client_ref->fatal("suspend without socket");
    $client_ref->{active} || $client_ref->fatal("suspend without active");
    if (defined $client_ref->{in}) {
        $client_ref->{reader} = undef;
        $client_ref->{timeout} = undef if
            $client_ref->{out} eq "" || $client_ref->{write_suspended};
    }
    $client_ref->{read_suspended} = 1;
}

sub resume_read {
    my ($client_ref) = @_;

    $client_ref->fatal("Not suspended") if !$client_ref->{read_suspended};
    $client_ref->{socket} || $client_ref->fatal("resume without socket");
    $client_ref->{active} || $client_ref->fatal("resume without active");
    if (defined $client_ref->{in}) {
        $client_ref->{reader} = $client_ref->{socket}->add_read(\&_reader, $client_ref);
        if ($client_ref->{out} eq "" || $client_ref->{write_suspended}) {
            my $command = $client_ref->{commands}[0] ||
                $client_ref->fatal("resume without command");
            my $timeout = $command->{$command->{COMMAND_REF}[FLAGS] & PHASE2 ?
                                     "transaction_timeout2" : "transaction_timeout"};
            $client_ref->{timeout} = timer(
                $timeout // $client_ref->fatal("No transaction_timeout"),
                sub { $client_ref->_transaction_timed_out });
        }
    }
    $client_ref->{read_suspended} = 0;
}

sub suspend_write {
    my ($client_ref) = @_;

    $client_ref->fatal("Already suspended") if $client_ref->{write_suspended};
    $client_ref->{socket} || $client_ref->fatal("suspend without socket");
    $client_ref->{active} || $client_ref->fatal("suspend without active");
    if ($client_ref->{out} ne "") {
        $client_ref->{writer} = undef;
        $client_ref->{timeout} = undef if
            !defined $client_ref->{in} || $client_ref->{read_suspended};
    }
    $client_ref->{write_suspended} = 1;
}

sub resume_write {
    my ($client_ref) = @_;

    $client_ref->fatal("Not suspended") if !$client_ref->{write_suspended};
    $client_ref->{socket} || $client_ref->fatal("resume without socket");
    $client_ref->{active} || $client_ref->fatal("resume without active");
    if ($client_ref->{out} ne "") {
        $client_ref->{writer} = $client_ref->{socket}->add_write(\&_writer, $client_ref);
        if (!defined $client_ref->{in}) {
            my $command = $client_ref->{commands}[0] ||
                $client_ref->fatal("resume without command");
            my $addr = $command->{address}[$command->{address_i}];
            $client_ref->{timeout} = timer(
                $addr->{connection_timeout} // $command->{connection_timeout} //
                $client_ref->fatal("No connection_timeout"),
                sub { $client_ref->_connection_timeout }
            );
        } elsif ($client_ref->{read_suspended}) {
            my $command = $client_ref->{commands}[0] ||
                $client_ref->fatal("resume without command");
            my $timeout = $command->{$command->{COMMAND_REF}[FLAGS] & PHASE2 ?
                                     "transaction_timeout2" : "transaction_timeout"};
            $client_ref->{timeout} = timer(
                $timeout // $client_ref->fatal("No transaction_timeout"),
                sub { $client_ref->_transaction_timed_out });
        }
    }
    $client_ref->{write_suspended} = 0;
}

sub suspendable {
    my ($client_ref, $state) = @_;

    my $command = $client_ref->{commands}[0] ||
        $client_ref->fatal("No command");
    $command->{suspendable} //
        $client_ref->fatal("Not a suspendable command");
    if ($state > 0) {
        # State > 0
        !$command->{suspendable} ||
            $client_ref->fatal("Command already suspendable");
        $command->{suspendable} = 1;

        $client_ref->suspend_read if
            length $command->{DATA} >= $command->{high_water};
    } else {
        $command->{suspendable} ||
            $client_ref->fatal("Command not suspendable");
        if ($state) {
            # State < 0
            if ($command->{done}) {
               $client_ref->{out} eq "" ||
                   $client_ref->fatal("Done while command has not yet completed");
               $command->arguments([delete $command->{DATA}]);
               $command->command_ref(MARKER);
               $client_ref->suspend_read if !$client_ref->{read_suspended};
               $client_ref->{read_suspended} = 0;
               $client_ref->{active} = 0;
            } else {
                $client_ref->resume_read if $client_ref->{read_suspended} &&
                    length $command->{DATA} < $command->{high_water};
            }
        } else {
            # State == 0
            $command->{suspendable} = 0;
            $client_ref->resume_read if $client_ref->{read_suspended};
        }
    }
}

sub _writer {
    my ($client_ref) = @_;

    my $rc = syswrite($client_ref->{socket}, $client_ref->{out}, $BLOCK_SIZE);
    if ($rc) {
        substr($client_ref->{out}, 0, $rc, "");
        if ($client_ref->{sync}) {
            my $command = $client_ref->{commands}[0] ||
                $client_ref->fatal("No command");
            $command->{source}->($client_ref, $command) if
                $command->{source} &&
                length $client_ref->{out} < $command->{high_water};
        }
        if ($client_ref->{out} eq "") {
            $client_ref->{writer} = undef;
            $client_ref->{timeout} = undef if $client_ref->{read_suspended};
        }

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
                my ($error, $str) = $client_ref->check_response(0, $command_ref);
                return $client_ref->error($str) if $error;
                if ($client_ref->{in} ne "") {
                    $str = display_string($client_ref->{in});
                    $client_ref->error("Spurious response bytes: $str");
                    return;
                }
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
    $client_ref->{in} .= $buffer;
    if ($client_ref->{out} ne "" || $client_ref->{sent} == 0) {
        my $response = display_string($client_ref->{in});
        if ($client_ref->{out} eq "") {
            # Implies sent == 0
            $client_ref->error("Response without having sent anything: $response");
            return;
        }
        # So $client_ref->{out} ne ""
        if ($client_ref->{sent} >= length $command->out) {
            # This implies we are running some command with followup data
            # E.g. Getting a FAIL while still sending send_v1 data is valid
            # This had better be a final status message though
            $client_ref->{expect_fail} = 1;
            # (read obviously isn't suspended since we just got data)
            $client_ref->suspend_write if !$client_ref->{write_suspended};
        } else {
            $client_ref->error("Response while command has not yet completed: $response");
            return;
        }
    }

    if ($client_ref->{expect_eof}) {
        my $spurious = display_string($client_ref->{in});
        $client_ref->error("Spurious response bytes $spurious");
        return;
    }

    my ($error, $str) = $client_ref->check_response($rc, $command_ref) or return;
    return $client_ref->error($str) if $error;
    if ($client_ref->{in} ne "" && !($command_ref->[FLAGS] & MAYBE_MORE)) {
        $str = display_string($client_ref->{in});
        $client_ref->error("Spurious response bytes: $str");
        return;
    }

    # Success
    if ($command_ref->[FLAGS] & EXPECT_EOF) {
        # Delay until actual EOF
        # Important for e.g. host:kill which is only finished at the EOF
        $client_ref->{expect_eof} = \$str;
        return;
    }

    # Don't expect any more input
    $client_ref->{reader} = undef;
    # Drop transaction timeout
    $client_ref->{timeout} = undef;
    # That should have been the only pending activity
    # (We already checked that out = "")
    $client_ref->{active} = 0;

    $client_ref->success($str);
}

sub check_response {
    my ($client_ref, $len_added, $command_ref) = @_;

    if ($client_ref->{sync}) {
        return ASSERTION, "Assertion: negative input" if $len_added < 0;
        for (1) {
            last if length $client_ref->{in} < 4;
            my $command = substr($client_ref->{in}, 0, 4);
            if ($command eq "FAIL") {
                last if length $client_ref->{in} < 8;
                my $more = unpack("V", substr($client_ref->{in}, 4, 4));
                last if length $client_ref->{in} < 8 + $more;
                my $response = substr($client_ref->{in}, 0, 8 + $more, "");
                substr($response, 0, 8, "");
                if ($response =~ /^unknown command ([0-9a-f]{8})$/i) {
                    # Make error message a bit friendlier
                    my $command_name = reverse pack("H*", $1);
                    $response = "Unsupported SYNC command '$command_name'";
                }
                return FAILED, $response;
            }
            last if length $client_ref->{in} < $command_ref->[NR_BYTES];
            if ($client_ref->{expect_fail}) {
                my $response = display_string($client_ref->{in});
                return BAD_ADB, "Response while command has not yet completed: $response";
            }
            return SUCCEEDED, substr($client_ref->{in}, 0, $command_ref->[NR_BYTES], "");
        }
        if ($len_added == 0) {
            # We don't have enough bytes but since we are at EOF we will
            # never get any more
            # Special case for QUIT which doesn't expect an answer, just close
            return SUCCEEDED, "" if
                ($command_ref->[FLAGS] & EXPECT_EOF) &&
                $command_ref->[NR_BYTES] == 0;
            my $response = display_string($client_ref->{in});
            return BAD_ADB, "Truncated ADB response $response";
        }
        return;
    }

    my ($error, $str) = adb_check_response(
        $client_ref, $len_added, $command_ref->[NR_BYTES],
        $command_ref->[FLAGS] & EXPECT_EOF) or return;
    if ($error == FAILED && $str eq "closed") {
        $str = ($command_ref->[FLAGS] & SYNC) && !$client_ref->{sync} ?
            "Not inside SYNC" : "Device did not recognize command";
    }
    if (!$error) {
        if ($client_ref->{expect_fail}) {
            $str = display_string($str);
            return BAD_ADB, "Response while command has not yet completed: $str";
        }
        utf8::decode($str) if $command_ref->[FLAGS] & UTF8_IN;
    }
    return $error, $str;
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

    my $filter = $client_ref->command_current->{filter};
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
    # my @devices;
    my %devices;
  DEVICE:
    while ($devices =~ s{^(\S+)[^\S\n]+(\S+|no device)(?:[^\S\n]+(\S.*\S))?\n}{}a) {
        my ($serial, $state, $description) = ($1, $2, $3);
        # Possible states seem to be:
        # offline, bootloader device host recovery rescue sideload
        # unauthorized authorizing connecting unknown
        # And UsbNoPermissionsShortHelpText() is: no permissions; [<url>])
        # We will fail to parse if that last one can really happen
        die "Multiple devices with serial number $serial" if $devices{$serial};
        # push @devices, $serial;
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
        @tracker = ADB::Client::Tracker->new($socket, $client_ref->command_current->command_ref, $client_ref->{block_size}, \%devices, $client_ref->{in});
        $client_ref->{in} = "";
    }
    # return [\%devices, \@devices, shift, @tracker];
    return [\%devices, @tracker];
}

sub process_connect {
    my ($msg) = @_;

    return $msg =~ /^connected to/i ? [$msg] : $msg;
}

sub process_tport {
    return [unpack("q", shift)];
}

sub process_phase1 {
    my ($command_ref_orig, $devices, $command_name, $client_ref, $result) = @_;

    my $command = $client_ref->command_current;
    $command->{COMMAND_REF} = $command_ref_orig;

    if ($client_ref->{in} ne "" and
        my ($error, $str) =
        $client_ref->check_response(length $client_ref->{in},
                                    $command_ref_orig)) {
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
    $client_ref->{active} = 1;
    $client_ref->{read_suspended} = 1;
    $client_ref->resume_read;
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
    my ($error, $str) = adb_check_response(\%data, length $forward, -1, EXPECT_EOF) or die "Incomplete response 'OKAY$forward'";
    return $str if $error;
    $data{in} eq "" || die "Still input left";
    return [$str];
}

sub process_reverse_list {
    my ($reverses) = @_;

    my %reverses;
    for (split /\n/, $reverses) {
        my ($from, $to) = /^\S+\s+(\S+)\s+(.*\S)\s*\z/ or
            die "Invalid reverse line '$_'";
        die "Duplicate from '$from'" if exists $reverses{$from};
        $reverses{$from} = $to;
    }
    return [\%reverses];
}

sub process_sync {
    my (undef, undef, $client_ref, $result) = @_;
    $client_ref->{sync} = 1;
    return $result;
}

sub process_lstat_v1 {
    my ($bytes, $command_name) = @_;

    my ($id, $mode, $size, $mtime) = unpack("a4VVV", $bytes);
    $id eq $command_name || return "Inconsistent response '$id'";
    return [SYNC_ERROR_NATIVE, SYNC_ERROR_ADB] unless $mode || $size || $mtime;
    return [{
        mode	=> $mode,
        perms	=> $mode & ADB_PERMISSION_MASK,
        ftype	=> $adb_file_type_from_mode{$mode & ADB_FILE_TYPE_MASK} || FILE_TYPE_UNKNOWN,
        size	=> $size,
        mtime	=> $mtime + EPOCH1970,
    }];
}

sub process_lstat_v2 {
    my ($bytes, $command_name) = @_;

    my %stat;
    (my $id, my $error,
     @stat{qw(dev ino mode nlink uid gid size atime mtime ctime)}) =
         unpack("a4VQ<Q<VVVVQ<q<q<q<", $bytes);
    $id eq $command_name || return "Inconsistent response '$id'";
    return [$errno_native_from_adb{$error}, $errno_adb{$error}] if $error;
    $stat{perms} = $stat{mode} & ADB_PERMISSION_MASK;
    $stat{ftype} = $adb_file_type_from_mode{$stat{mode} & ADB_FILE_TYPE_MASK} || FILE_TYPE_UNKNOWN,
    $stat{$_} += EPOCH1970 for qw(mtime atime ctime);
    return [\%stat];
}

sub process_list_v1 {
    my (undef, $command_name, $client_ref) = @_;

    my $command = $client_ref->command_current;
    $command->{DATA} //= {};
    # It seems we could get count from keys %{$command->{DATA}}
    # But the on_progress callback might not update $command->{DATA}
    $command->{count} //= 0;
    my $path;
    while (length $client_ref->{in} >= 4) {
        my ($id, $mode, $size, $mtime, $length) =
            unpack("a4V4", $client_ref->{in});
        if ($id eq "DENT") {
            defined $length && length $client_ref->{in} >= 20 + $length || last;
            ++$command->{count};
            my $file = substr($client_ref->{in}, 0, 20 + $length, "");
            substr($file, 0, 20, "");

            # Possibly skip . and ..
            next if ($file eq "." || $file eq "..") &&
                !($command->{self_parent} // 1);

            if ($command->command_ref->[FLAGS] & UTF8_IN) {
                utf8::decode($file);
                # utf8::downgrade($file, 1);
            }
            my $stat = {
                mode	=> $mode,
                perms	=> $mode & ADB_PERMISSION_MASK,
                ftype	=> $adb_file_type_from_mode{$mode & ADB_FILE_TYPE_MASK} || FILE_TYPE_UNKNOWN,
                size	=> $size,
                mtime	=> $mtime + EPOCH1970,
            };
            if ($command->{on_progress}) {
                $path //= $command->arguments->[0];
                eval {
                    $command->{on_progress}->($client_ref->client,
                                              $command->{DATA}, $file, $stat,
                                              $path);
                };
                if ($@) {
                    my $err = $@;
                    return \$err;
                }
            } else {
                $command->{DATA}{$file} = $stat;
            }
        } elsif ($id eq "DONE") {
            defined $length && length $client_ref->{in} >= 20 + $length || last;
            substr($client_ref->{in}, 0, 20 + $length, "");
            if ($client_ref->{in} ne "") {
                my $str = display_string($client_ref->{in});
                return "Spurious response bytes: $str";
            }
            my $result = $command->{count} ? $command->{DATA} : SYNC_ERROR_NATIVE;
            $command->{recursive} || return [$result];
            if ($command->{base}) {
                my $next = shift @{$command->{todo}};
                $$next = $result;
            } else {
                # First call
                $command->{base} = $result;
                $command->{todo} = [];
            }
            if ($command->{count}) {
                for my $f (sort keys %$result) {
                    $result->{$f}{ftype} eq "DIR" || next;
                    next if $f eq "." || $f eq "..";
                    my $fenc = $f;
                    if (($command->command_ref->[FLAGS] & UTF8_IN) &&
                        !($command->command_ref->[FLAGS] & UTF8_OUT)) {
                        utf8::encode($fenc);
                    } elsif (!($command->command_ref->[FLAGS] & UTF8_IN) &&
                             ($command->command_ref->[FLAGS] & UTF8_OUT)) {
                        utf8::decode($fenc);
                    }
                    $path //= $command->arguments->[0];
                    $result->{$f}{tree} = "$path/$fenc";
                    push @{$command->{todo}}, \$result->{$f}{tree};
                }
            }
            my $next = $command->{todo}[0] || return [$command->{base}];
            $command->command_ref($command->command_ref, $$next);
            $command->{count} = 0;
            $command->{DATA} = {};
            $client_ref->activate;
            return undef;
        } elsif ($id eq "FAIL") {
            my ($err, $str) = $client_ref->check_response(length $client_ref->{in}, $command->command_ref) or last;
            $err || $client_ref->fatal("FAIL succeeded");
            return $str;
        } else {
            return "Inconsistent response '$id'";
        }
    }
    # Go read more bytes
    $client_ref->{active} = 1;
    $client_ref->{read_suspended} = 1;
    $client_ref->resume_read;
    return undef;
}

sub process_recv_v1 {
    my (undef, undef, $client_ref) = @_;

    my $command = $client_ref->command_current;
    while (length $client_ref->{in} >= 4) {
        my ($id, $length) = unpack("a4V", $client_ref->{in});
        if ($id eq "DATA") {
            defined $length && length $client_ref->{in} >= 8 + $length || last;
            substr($client_ref->{in}, 0, 8, "") unless $command->{raw};
            if ($command->{on_progress}) {
                eval {
                    $command->{on_progress}->(
                        $client_ref->client,
                        \$command->{DATA}, $command->{raw} ?
                        substr($client_ref->{in}, 0, $length+8, "") :
                        substr($client_ref->{in}, 0, $length, ""));
                };
                if ($@) {
                    my $err = $@;
                    return \$err;
                }
            } else {
                $command->{DATA} .= $command->{raw} ?
                    substr($client_ref->{in}, 0, $length+8, "") :
                    substr($client_ref->{in}, 0, $length, "");
            }
        } elsif ($id eq "DONE") {
            defined $length || last;
            # We ignore the length on DONE
            # return "DONE with length $length" if $length;
            if ($command->{raw}) {
                $command->{DATA} .= substr($client_ref->{in}, 0, 8, "");
            } else {
                substr($client_ref->{in}, 0, 8, "");
            }
            if ($client_ref->{in} ne "") {
                my $str = display_string($client_ref->{in});
                return "Spurious response bytes: $str";
            }
            $command->{done} = 1;
            $command->{low_water} = 1;
            $command->{high_water} = INFINITY;
            last;
        } elsif ($id eq "FAIL") {
            my ($err, $str) = $client_ref->check_response(length $client_ref->{in}, $command->command_ref) or last;
            $err || $client_ref->fatal("FAIL succeeded");
            return $str;
        } else {
            return "Inconsistent response '$id'";
        }
    }
    if ($command->{sink} && !$command->{suspendable} &&
        length $command->{DATA} >= $command->{low_water}) {
        # my $continue = ADB::Client::Continue->new($client_ref, $command);
        $command->{sink}->($client_ref->client, $command,
                           \$command->{DATA}, $command->{done});
    }

    return [delete $command->{DATA}] if $command->{done} && !$command->{suspendable};

    # Go read more bytes
    if (!$client_ref->{read_suspended}) {
        $client_ref->{active} = 1;
        $client_ref->{read_suspended} = 1;
        $client_ref->resume_read;
    }
    return undef;
}

sub process_send_v1 {
    my ($bytes, undef, $client_ref) = @_;
    my $id = unpack("a4", $bytes);
    $id eq "OKAY" || return "Inconsistent response '$id'";
    return [$client_ref->{nr_bytes}, $client_ref->command_current->{mtime}];
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
