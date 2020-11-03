package ADB::Client::Ref;
use strict;
use warnings;

our $VERSION = '1.000';

use Carp;
use Scalar::Util qw(weaken refaddr);
# use IO::Socket::IP;
use IO::Socket qw();
use Errno qw(EINPROGRESS EWOULDBLOCK EINTR EAGAIN ECONNRESET ETIMEDOUT
             ECONNREFUSED EACCES EPERM ENETUNREACH EHOSTUNREACH);

use ADB::Client::Events qw(mainloop unloop loop_levels timer immediate);
use ADB::Client::ServerStart;
use ADB::Client::Utils
    qw(info caller_info dumper adb_check_response display_string
       clocktime_running
       FAILED BAD_ADB ASSERTION INFINITY $DEBUG $VERBOSE $QUIET),
    _prefix => "utils_", qw(addr_info);
use Socket qw(IPPROTO_TCP IPPROTO_UDP SOCK_DGRAM SOCK_STREAM SOL_SOCKET
              SO_ERROR TCP_NODELAY);
use ADB::Client::Command;
use Storable qw(dclone);

use Exporter::Tidy
    other	=>[qw($CALLBACK_DEFAULT
                      $ADB_HOST $ADB_PORT $ADB $ADB_SOCKET $DEBUG $VERBOSE)];

use constant {
    # Index in @COMMANDS element
    COMMAND_NAME	=> 0,
    COMMAND	=> 1,
    NR_RESULTS	=> 2,
    FLAGS	=> 3,
    PROCESS	=> 4,
    # CODE is used for SPECIAL commands
    # Cannot reuse PROCESS for this since PROCESS will run at command retirement
    # (and we want that since it could be useful)
    # It's up to the SPECIAL commands to give meaning to elements after this
    CODE	=> 2,

    # FLAGS values
    EXPECT_EOF => 1,

    # Index in commands element
    COMMAND_REF	=> 0,
    CALLBACK	=> 1,
    # possibly unify ARGUMENTS and STATE
    ARGUMENTS	=> 2,
    STATE	=> 3,

    SPECIAL	=> "",
};

our @CARP_NOT = qw(ADB::Client ADB::Client::Events);

our $BLOCK_SIZE = 65536;

our $CALLBACK_DEFAULT	= \&callback_default;
our $ADB = "adb";
our $ADB_HOST	= "127.0.0.1";
our $ADB_SOCKET	= undef;
our $ADB_PORT	= 5037;
our $TRANSACTION_TIMEOUT = 10;
our $CONNECTION_TIMEOUT = 10;

use constant {
    FATAL	=> [_fatal	=> SPECIAL, \&_fatal_run],
    MARKER	=> [marker	=> SPECIAL, \&_marker],
    CONNECT	=> [connect	=> SPECIAL, \&_connect_start],
    SPAWN	=> [spawn	=> SPECIAL, \&_connect_start],
    VERSION	=> [version	=> "host:version", -1, 1, \&process_version],
    KILL	=> [kill	=> "host:kill", 0, 1],
};

our @COMMANDS = (
    # command, number of result bytes, expect close
    # See the index in command array constants
    FATAL,
    MARKER,
    CONNECT,
    SPAWN,
    VERSION,
    KILL,
    [forget		=> SPECIAL, \&_forget],
    [resolve		=> SPECIAL, \&_resolve],
    [features		=> "host:features", -1, EXPECT_EOF,  \&process_features],
    [remount		=> "remount:", INFINITY, EXPECT_EOF,  \&process_remount],
    [devices		=> "host:devices", -1, EXPECT_EOF,   \&process_devices],
    [devices_long	=> "host:devices-l", -1, EXPECT_EOF, \&process_devices],
    [transport		=> "host:transport-%s", 0, 0],
    [tport		=> "host:tport:%s", 8, 0, \&process_tport],
    [unroot		=> "unroot:", INFINITY, EXPECT_EOF],
    [root		=> "root:", INFINITY, EXPECT_EOF],
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
    if (defined $model) {
        $model = $model->client_ref || croak "Model without client_ref";
        for my $name (qw(blocking adb adb_socket host port reresolve
                         addr_info
                         connection_timeout transaction_timeout block_size)) {
            $arguments{$name} //= $model->{$name};
        }
    }
    my $blocking = delete $arguments{blocking} // 1;
    my $adb  = delete $arguments{adb} // $ADB;
    my $adb_socket = delete $arguments{adb_socket} // $ADB_SOCKET;
    my $host = delete $arguments{host} // $ADB_HOST;
    my $port = delete $arguments{port} // $ADB_PORT;
    my $reresolve = delete $arguments{reresolve} // INFINITY;
    my $connection_timeout = delete $arguments{connection_timeout} //
        $CONNECTION_TIMEOUT;
    my $transaction_timeout = delete $arguments{transaction_timeout} //
        $TRANSACTION_TIMEOUT;
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
        children	=> {},
        result		=> [],
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

sub delete {
    my ($client_ref) = @_;

    $client_ref->close;
    my $children = $client_ref->{children};
    $client_ref->{children} = {};
    $_->delete(1) for values %$children;
    @{$client_ref->{result}} = ();
    @{$client_ref->{commands}} = [FATAL];
    #if (my $client = $client_ref->client) {
    #    $$client = undef;
    #}
}

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

sub child_add {
    my ($client_ref, $child) = @_;
    $client_ref->{children}{refaddr($child)} = $child;
}

sub child_delete {
    my ($client_ref, $child) = @_;
    delete $client_ref->{children}{refaddr($child)};
}

sub DESTROY {
    --$objects;
    info("DESTROY @_") if $DEBUG;
    shift->delete;
}

sub callback_default {
    croak $_[1] if $_[1];
}

sub connected {
    return shift->{socket} ? 1 : 0;
}

sub callback_blocking {
    my ($client_ref, $loop_levels) = @_;

    $client_ref->{result}[$loop_levels] = undef;
    return sub {
        my $client_ref = $ {shift()};
        $client_ref->{result}[$loop_levels] = \@_;
        unloop($loop_levels);
    };
}

sub wait : method {
    my ($client_ref, $loop_levels) = @_;

    mainloop();
    if (@{$client_ref->{commands}}) {
        # Remove the command we are waiting for since we sort of had an error
        croak "A previous command in the queue failed";
    }
    my $result = delete $client_ref->{result}[$loop_levels] //
        die "Assertion: Exit mainloop without setting result";
    # croak $result->[0] =~ s{(.*) at .* line \d+\.?\n}{$1}sar if $result->[0];
    $result->[0] =~ /\n\z/ ? die $result->[0] : croak $result->[0] if $result->[0];
    wantarray || return $result->[1];
    # remove the flag indicating success
    shift @$result;
    return @$result;
}

sub server_start {
    my ($client_ref, $arguments, $callback) = @_;

    ADB::Client::ServerStart->new($client_ref, $callback, $arguments);
}

sub commands_add {
    my ($class, $client_class) = @_;
    $client_class->_add_command($_) for 0..$#COMMANDS;
}

sub command_add {
    my ($class, $client_class, $command) = @_;

    push @COMMANDS, $command;
    eval { $client_class->_add_command($#COMMANDS) };
    if ($@) {
        pop @COMMANDS;
        die $@;
    }
}

sub command_get {
    my ($class, $index) = @_;

    my $command = $COMMANDS[$index] ||
        croak "No command at index '$index'";
    return
        $command->[COMMAND_NAME] || die("Assertion: No COMMAND_NAME"),
        $command->[COMMAND] =~ tr/%//,
        $command->[COMMAND] eq SPECIAL;
}

sub command_simple {
    my ($client_ref, $arguments, $callback, $index, $args) = @_;

    croak "Unknown argument " . join(", ", keys %$arguments) if %$arguments;

    my $command_ref = $COMMANDS[$index] ||
        croak "No command at index '$index'";

    my $out = sprintf($command_ref->[COMMAND], @$args);
    utf8::encode($out);
    if (length $out >= 2**16) {
        $out = display_string($out);
        croak "Command too long: $out";
    }
    push @{$client_ref->{commands}}, [$command_ref, $callback, sprintf("%04X", length $out) . $out];
    $client_ref->activate(1);
}

sub special_simple {
    my ($client_ref, $arguments, $callback, $index, $args) = @_;

    croak "Unknown argument " . join(", ", keys %$arguments) if %$arguments;

    my $command_ref = $COMMANDS[$index] ||
        croak "No command at index '$index'";

    push @{$client_ref->{commands}}, [$command_ref, $callback, $args];
    $client_ref->activate(1);
}
*marker = \&special_simple;
*forget = \&special_simple;

sub _fatal {
    my ($client_ref, $arguments, $callback, $index) = @_;
    croak "Unknown argument " . join(", ", keys %$arguments) if %$arguments;

    my $command_ref = $COMMANDS[$index] ||
        croak "No command at index '$index'";
    push @{$client_ref->{commands}}, [$command_ref];
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
        $client_ref->fatal("No command during connect");
    my $args = $command->[ARGUMENTS];
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

sub _forget {
    my ($client_ref) = @_;

    $client_ref->close();
    $client_ref->{addr_connected} = undef;
    $client_ref->success;
}

sub connect : method {
    my ($client_ref, $arguments, $callback, $index) = @_;

    croak "Unknown argument " . join(", ", keys %$arguments) if %$arguments;

    my $command_ref = $COMMANDS[$index] ||
        croak "No command at index '$index'";
    my $connector = $client_ref->connector($command_ref, $callback);

    push @{$client_ref->{commands}}, $connector;
    $client_ref->activate(1);
}

sub spawn {
    my ($client_ref, $arguments, $callback, $index) = @_;

    my $command_ref = $COMMANDS[$index] ||
        croak "No command at index '$index'";
    my $connector = $client_ref->connector($command_ref, $callback, \&_connect_spawn, $arguments);

    croak "Unknown argument " . join(", ", keys %$arguments) if %$arguments;

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
    }
    $client_ref->{active} = 0;
    $client_ref->{timeout} = undef;
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

    $client_ref->close();
    local $client_ref->{command_retired} = shift @{$client_ref->{commands}} //
        $client_ref->fatal("error without command");
    # We may need $command to exist during the callback because sometimes
    # we still use it in a closure (see e.g. _connect_done)
    local $client_ref->{post_activate} = 0;
    $client_ref->{command_retired}->[CALLBACK]->($client_ref->{client}, @_);
    $client_ref->activate if $client_ref->{post_activate};
}

# If used inside a calback (toplevel false) nothing after this call should
# change client_ref state, so typically this should be the last thing you do
sub success {
    my $client_ref = shift;

    my $result = \@_;
    my $command = shift @{$client_ref->{commands}} ||
        $client_ref->fatal("success without command");
    if ($client_ref->{active}) {
        $client_ref->{socket}->delete_read if
            $client_ref->{socket} && defined $client_ref->{in};
        $client_ref->{timeout} = undef;
        $client_ref->{active} = 0;
    }

    local $client_ref->{command_retired} = $command;
    my $command_ref = $command->[COMMAND_REF];
    if ($command_ref->[PROCESS]) {
        # $_[0] as first arguments since the others will typically be ignored
        $result = eval { $command_ref->[PROCESS]->($_[0], $command_ref->[COMMAND], $client_ref, $result) };
        if ($@) {
            my $err = $@;
            my $str = display_string($_[0]);
            unshift @{$client_ref->{commands}}, $command;
            $client_ref->error("Assertion: Could not process $command_ref->[COMMAND] output $str: $err");
            return;
        }
        if (ref $result ne "ARRAY") {
            unshift @{$client_ref->{commands}}, $command;
            if (ref $result eq "") {
                $client_ref->error($result);
            } else {
                $client_ref->error("Assertion: Could not process $command_ref->[COMMAND] output: Neither a string nor an ARRAY reference");
            }
            return;
        }
    }
    # We may need $command to exist during the callback because sometimes
    # we still use it in a closure (see e.g. _connect_done)
    local $client_ref->{post_activate} = 1;
    $command->[CALLBACK]->($client_ref->{client}, undef, @$result);
    $client_ref->activate if $client_ref->{post_activate};
}

# If used inside a calback (toplevel false) nothing after this call should
# change client_ref state, so typically this should be the last thing you do
sub activate {
    my ($client_ref, $top_level) = @_;

    return if $client_ref->{active} || !@{$client_ref->{commands}};

    for (1) {
        my $command = $client_ref->{commands}[0] ||
            $client_ref->fatal("No command during activate");
        my $command_ref = $command->[COMMAND_REF];
        if ($client_ref->{out} ne "") {
            my $response = display_string($client_ref->{out});
            $client_ref->fatal("$response to ADB still pending when starting $command_ref->[COMMAND]");
            return;
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
            $client_ref->fatal("Reconnect loop") if
                $command->[STATE]{reconnects}++;
            unshift(@{$client_ref->{commands}},
                    $client_ref->connector(CONNECT, undef, \&_connect_done));
            redo;
        }
        $client_ref->{out} = $client_ref->{commands}[0][ARGUMENTS];
        info("Sending to ADB: " . display_string($client_ref->{out})) if $DEBUG;
        $client_ref->{socket}->add_write(sub { $client_ref->_writer });
        $client_ref->{socket}->add_read(sub { $client_ref->_reader });
        my $addr = $client_ref->{addr_connected} ||
            $client_ref->fatal("Socket without addr_connected");
        $client_ref->{timeout} = timer(
            $addr->{transaction_timeout} // $client_ref->{transaction_timeout},
            sub { $client_ref->_timed_out });
    }
    $client_ref->{active} = 1;
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
    my ($client_ref, $command_ref, $callback, $step, $arguments) = @_;

    my $state = {
        command_ref => $command_ref,
    };
    if ($arguments) {
        $state->{spawn}		= 1;
        $state->{version_min}	= delete $arguments->{version_min} || 0;
        $state->{kill}		= delete $arguments->{kill};
        $state->{adb}		= delete $arguments->{adb} // $client_ref->{adb};
        $state->{adb_socket}	= delete $arguments->{adb_socket} // $client_ref->{adb_socket} // 0;

        $state->{version_min} =~ /^[1-9][0-9]*\z|^0\z/ ||
            croak "Version_min is not a positive integer";
        $state->{version_min} <= 2**16 ||
            croak "Version_min '$state->{version_min}' out of range";
    }

    my $command = ADB::Client::Command->new;
    $command->[STATE] = $state;
    $command->[COMMAND_REF] = $command_ref;
    if ($step) {
        $state->{callback} = $callback;
        $state->{step} = $step;

        $command->[CALLBACK] = \&_connector;
    } else {
        $command->[CALLBACK] = $callback;
    }

    return $command;
}

sub _connector {
    my $client_ref = shift->client_ref;

    my $command = $client_ref->{command_retired};
    unshift @{$client_ref->{commands}}, $command;
    $command->[STATE]{step}->($client_ref, $command, @_);
    # Hack! This tells the success method not to call activate
    $client_ref->post_activate(0);
}

# Called with active = 0
sub _connect_start {
    my ($client_ref) = @_;

    my $command = $client_ref->{commands}[0] ||
        $client_ref->fatal("No command during connect");
    my $state = $command->[STATE];
    $state->{address_i}	= -1;
    $state->{address} = $client_ref->{addr_connected} ?
        [$client_ref->{addr_connected}] : $client_ref->{addr_info};

    if ($client_ref->{socket}) {
        $client_ref->{addr_connected} ||
            $client_ref->fatal("socket without addr_connected");
        $state->{address_i} = 0;

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
                $state->{address_i} = -1;
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

# Called with active = 0
# Make sure to manage {active} as needed
sub _connect_next {
    my ($client_ref) = @_;

    my $command = $client_ref->{commands}[0] ||
        $client_ref->fatal("No command during connect");
    my $state = $command->[STATE];

    while (my $addr = $state->{address}[++$state->{address_i}]) {
        my $socket = IO::Socket->new();
        do {
            # Make sure CLO_EXEC is set
            local $^F = -1;
            if (!$socket->socket($addr->{family}, SOCK_STREAM, IPPROTO_TCP)) {
                $addr->{last_connect_error} = "Socket: $^E";
                next;
            }
        };
        $socket->blocking(0);
        if (connect($socket, $addr->{connect_addr})) {
            # SOL_TCP == IPPROTO_TCP
            $socket->setsockopt(IPPROTO_TCP, TCP_NODELAY, 1) //
                warn("Could not set TCP_NODELAY on connecting socket: $^E");
            $client_ref->{socket} = $socket;
            $client_ref->_connected(0) || return;
        } elsif ($! == EINPROGRESS || $! == EWOULDBLOCK) {
            $socket->setsockopt(IPPROTO_TCP, TCP_NODELAY, 1) //
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

    $client_ref->error(
        $state->{first_error} ||
        "Assertion: No failure reason after connection attempts");
}

sub _connect_writable {
    my ($client_ref) = @_;

    # Called with active = 1
    my $command = $client_ref->{commands}[0] ||
        $client_ref->fatal("No command during connect");
    my $state = $command->[STATE];

    my $addr = $state->{address}[$state->{address_i}];
    $addr->{connected} = undef;
    my $err = $client_ref->{socket}->getsockopt(SOL_SOCKET, SO_ERROR);
    if (!defined $err) {
        $addr->{last_connect_error} = "Could not getsockopt(SOL_SOCKET, SO_ERROR): $!";
        $client_ref->error("Assertion: $addr->{last_connect_error}");
        return;
    }

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
        $client_ref->fatal("No command during connect");
    my $state = $command->[STATE];
    my $addr = $state->{address}[$state->{address_i}];
    $addr->{connected} = undef;

    $! = ETIMEDOUT;
    $addr->{last_connect_error} = "Connect error: $!";
    $state->{first_error} ||= "ADB server $addr->{connect_ip} port $addr->{connect_port}: $addr->{last_connect_error}";
    $client_ref->close;
    $client_ref->_connect_next;
}

# Return 0: we are done
# return 1: continue with _connect_next (try the next ip/port)
# $err is expected to be a dualvar corresponding to $!
sub _connected {
    my ($client_ref, $err) = @_;

    # Called with active = 0
    my $command = $client_ref->{commands}[0] ||
        $client_ref->fatal("No command during connect");
    my $state = $command->[STATE];
    my $addr = $state->{address}[$state->{address_i}];

    if ($err == 0) {
        # $client_ref->{in} = "";
        # $client_ref->{sent} = 0;
        # $client_ref->{expect_eof} = undef;
        $addr->{connected} = clocktime_running() || 1e-9;
        $client_ref->{addr_connected} = $addr;
        $client_ref->success(dclone($addr));
        return 0;
    } else {
        $addr->{last_connect_error} = "Connect error: $err";
        my $msg = "ADB server $addr->{connect_ip} port $addr->{connect_port}: $addr->{last_connect_error}";
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
            # succeeded but immediately after that the connection got reset?
            # need to do a packet capture.
            # (Can be reproduced by kill -9 of adbd just before connect)
            $state->{first_error} ||= $msg;
            return 1;
        } else {
            $client_ref->error($msg);
            return 0;
        }
    }
}

# Called with active = 0 and CONNECT command already removed from the queue
sub _connect_done {
    my $client_ref = shift;
    my $command = shift;

    my $c = shift @{$client_ref->{commands}} ||
        $client_ref->fatal("_connect_done without command");
    $c == $command ||
        $client_ref->fatal("Inconsistent _connect_done");
    if ($_[0]) {
        # this calls error on the NEXT command
        # (the one that triggered this reconnect)
        $client_ref->error(@_);
    } else {
        $client_ref->activate;
    }
}

# Restore the real callback and call the success or error method on it
# Called with active = 0 and CONNECT command already removed from the queue
sub _connect_final {
    my $client_ref = shift;
    my $command = $client_ref->{commands}[0] ||
        $client_ref->fatal("No command during activate");
    my $state = $command->[STATE];
    # Restore original command_ref
    $command->[COMMAND_REF] = $state->{command_ref};
    # Restore original callback
    $command->[CALLBACK] = $state->{callback};
    if ($_[0]) {
        $client_ref->error(@_);
    } else {
        $client_ref->success(@_);
    }
}

sub _connect_spawn {
    my $command = shift;
    my $client = shift;
    my $err = shift;

    my $state = $command->[STATE];

    # Error: All connects failed
    # Success: we have a connection
    die "Not implemented yet _connect_spawn($err)" if $err;

    my $client_ref = $client->client_ref;
    $client_ref->{socket} ||
        $client_ref->fatal("connection without socket");

    if ($state->{version_min} || !$state->{kill}) {
        $command->[COMMAND_REF] = VERSION;
        $state->{step} = \&_connect_version;
    } elsif (1) {
        $command->[COMMAND_REF] = KILL;
        $state->{step} = \&_connect_kill;
    } else {
        # Simply call the original callback
        my $addr = $state->{address}[$state->{address_i}];
        $client_ref->_connect_final($err, $addr);
        return;
    }
    # We are called with a just opened connection. Don't reconnect on close
    $state->{reconnects} = 1;
    $client_ref->activate;
}

# $err implies some form of bad response
# typically this means you connected to a non-ADB server
sub _connect_version {
    my ($command, $client, $err, $version) = @_;

    my $client_ref = $client->client_ref;
    # Version is autoclose
    $client_ref->fatal("Still have socket") if $client_ref->{socket};
    my $state = $command->[STATE];
    my $addr = $state->{address}[$state->{address_i}];

    if (!$err) {
        $addr->{version} = $version;
        if ($version >= $state->{version_min}) {
            # Simply call the original callback
            $client_ref->_connect_final($err, $addr);
            return;
        }
        # Version is too low
        if ($state->{kill}) {
            # Kill server if so requested
            $state->{step} = \&_connect_kill;
            $state->{reconnects} = 0;
            $command->[COMMAND_REF] = KILL;
            $client_ref->activate;
            return;
        }
        # Otherwise report the error
        $err = "version '$version' is below '$state->{version_min}'";
    }
    $addr->{last_connect_error} = $err;
    $state->{first_error} ||= $err;
    $client_ref->_connect_final("ADB server $addr->{connect_ip} port $addr->{connect_port}: $err");
}

sub _connect_kill {
    my $command = shift;
    my $client = shift;
    my $err = shift;

    my $client_ref = $client->client_ref;
    # Kill is autoclose
    $client_ref->fatal("Still have socket") if $client_ref->{socket};
    my $state = $command->[STATE];
    my $addr = $state->{address}[$state->{address_i}];

    if ($err) {
        $addr->{last_connect_error} = $err;
        $state->{first_error} ||= $err;
        $client_ref->_connect_final("ADB server $addr->{connect_ip} port $addr->{connect_port}: $err");
        return;
    }

    $state->{step} = \&_connect_spawn;
    $state->{reconnects} = 0;
    $command->[COMMAND_REF] = SPAWN;
    $client_ref->_connect_next;
}

sub _timed_out {
    my ($client_ref) = @_;

    $client_ref->error("Operation timed out");
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
                my $command_ref = $command->[COMMAND_REF];
                # If we've not sent anything yet a close isn't so strange
                if ($client_ref->{sent} == 0 &&
                    # If we received stuff from the server go to normal
                    # error processing
                    $client_ref->{in} eq "" &&
                    !$command->[STATE]{reconnects}++) {
                    $client_ref->close;
                    $client_ref->activate;
                    return;
                }

                my ($error, $str) = adb_check_response($client_ref, 0, $command_ref->[NR_RESULTS], 0);
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
        $client_ref->fatal("No command during activate");
    my $command_ref = $command->[COMMAND_REF];
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

    my ($error, $str) = adb_check_response($client_ref, $rc, $command_ref->[NR_RESULTS], $command_ref->[FLAGS] & EXPECT_EOF) or return;
    return $client_ref->error($str) if $error;
    if ($client_ref->{in} ne "") {
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
    $client_ref->success($str);
}

sub process_version {
    my ($version) = @_;

    # Caller will already construct an error message using the input string
    $version =~ m{^[0-9a-fA-F]{4}\z} || die "Not a 4 digit hex number";
    return [hex $version];
}

sub process_remount {
    my ($str) = @_;

    $str =~ s/\.?\s*\z//;
    return [$str] if $str =~ s{\s*^remount succeeded\s*\z}{}m;
    return $str if $str =~ s{\.?\s*^remount failed\s*\z}{}m;
    return $str if $str =~ /Not running as root/i;
    # Caller will already construct an error message using the input string
    die "Cannot decode remount result";
}

sub process_features {
    my ($features) = @_;

    my @features = split /,/, $features;
    my %features;
    s/\s+\z//, s/^\s+//, ++$features{$_} for @features;
    return [\%features, \@features, $features];
}

sub process_devices {
    my ($devices, $command) = @_;

    my $long = $command eq "host:devices-l";
    my (@devices, %devices);
  DEVICE:
    while ($devices =~ s{^(\S+)[^\S\n]+(device|no device|offline)(?:[^\S\n]+(\S.*\S))?\n}{}) {
        my ($serial, $state, $description) = ($1, $2, $3);
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
    return [\%devices, \@devices, shift];
}

sub process_tport {
    my ($id) = @_;

    return [unpack("q", $id)];
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
