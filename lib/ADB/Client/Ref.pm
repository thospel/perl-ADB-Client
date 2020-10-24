package ADB::Client;
use strict;
use warnings;

our $VERSION = '1.000';

use Carp;
use Scalar::Util qw(refaddr);
# use IO::Socket::IP;
use IO::Socket qw();
use Socket qw(:addrinfo
              sockaddr_family unpack_sockaddr_in unpack_sockaddr_in6 inet_ntop
              pack_sockaddr_in pack_sockaddr_in6
              SOCK_STREAM SOCK_DGRAM SOL_SOCKET SO_ERROR IPPROTO_TCP IPPROTO_UDP
              AF_INET AF_INET6 TCP_NODELAY);
use Errno qw(EINPROGRESS EWOULDBLOCK EINTR EAGAIN EADDRINUSE EADDRNOTAVAIL
             ECONNREFUSED EACCES EPERM ENETUNREACH ETIMEDOUT ECONNRESET);
use Time::Local qw(timegm);
use POSIX qw(_exit);

use ADB::Client::Events;

use Data::Dumper;
$Data::Dumper::Indent   = 1;
$Data::Dumper::Sortkeys = 1;
$Data::Dumper::Useqq	= 1;

*mainloop	= \&ADB::Client::Events::mainloop;
*unloop		= \&ADB::Client::Events::unloop;
*loop_level	= \&ADB::Client::Events::loop_level;

use Exporter::Tidy
    other	=>[qw(addr_info mainloop unloop loop_level
                      $ADB_HOST $ADB_PORT $ADB)],
    _map	=> {
        event_init	=> \&ADB::Client::Events::init,
    };

$SIG{PIPE} = "IGNORE";

use constant {
    SADDRINUSE		=> "" . ($!=EADDRINUSE),
    SADDRNOTAVAIL	=> "" . ($!=EADDRNOTAVAIL),
    # Code assumes OKAY and FAIL both have length 4, so you can't change this
    OKAY		=> "OKAY",
    FAIL		=> "FAIL",
    CONNECT_HINTS		=> {
        socktype	=> SOCK_STREAM,
        protocol	=> IPPROTO_TCP,
    },
    BIND_HINTS		=> {
        socktype	=> SOCK_STREAM,
        protocol	=> IPPROTO_TCP,
        family		=> AI_PASSIVE,
    },
    CONNECT_HINTS_NUMERIC		=> {
        socktype	=> SOCK_STREAM,
        protocol	=> IPPROTO_TCP,
        flags		=> AI_NUMERICHOST,
    },
    BIND_HINTS		=> {
        socktype	=> SOCK_STREAM,
        protocol	=> IPPROTO_TCP,
        flags		=> AI_PASSIVE,
    },
    BIND_HINTS_NUMERIC		=> {
        socktype	=> SOCK_STREAM,
        protocol	=> IPPROTO_TCP,
        flags		=> AI_PASSIVE | AI_NUMERICHOST,
    },
};

our ($debug, $verbose, %timers, %connectors);
our $BLOCK_SIZE = 65536;

our $CALLBACK_DEFAULT	= \&callback_default;
our $ADB = "adb";
our $ADB_HOST	= "127.0.0.1";
# our $ADB_HOST	= "nas";
our $ADB_PORT	= 5037;
our $LISTEN	= 128;
our $TRANSACTION_TIMEOUT = 10;
our $CONNECT_TIMEOUT = 10;

END {
    %timers = ();
    %connectors = ();
}

sub info {
    local ($!, $^E);
    if (!@_) {
        my (undef, $filename, $line) = caller(1);
        @_ = ("$filename $line");
    }
    my $format = shift;
    $format =~ s/\n?\z/\n/;
    if (!@_) {
        @_ = ($format);
        $format = "%s";
    }
    my $time = ADB::Client::Timer->realtime;
    my $itime = int($time);
    my ($sec, $min, $hour, $day, $mon, $year) = localtime($itime);
    my $gtime = timegm($sec, $min, $hour, $day, $mon, $year);
    my $offset = ($gtime - $itime) / 60;
    my $sign = "+";
    if ($offset < 0) {
        $sign = "-";
        $offset = -$offset;
    }
    my $hoffset = $offset / 60;
    my $moffset = $offset % 60;
    printf(STDERR "%04d-%02d-%02d %02d:%02d:%06.3f %s%02d%02d: $format",
           $year+1900, $mon+1, $day, $hour, $min, $time-$itime+$sec,
           $sign, $hoffset, $moffset,
           @_);
}

sub caller_info {
    my $format = shift;
    my (@lines, $line, $i);
    push @lines, $line while $line = (caller(++$i))[2];
    if (@_) {
        info("$format [line %s]", "@lines");
    } else {
        info("$format [line @lines]");
    }
}

sub addr_info {
    my ($host, $port) = @_;

    my ($err, @ai) = getaddrinfo($host, $port, BIND_HINTS);
    die "Could not resolve($host, $port): $err" if $err;
    my ($first_err, @addres);
    for my $ai (@ai) {
        eval {
            my $udp = IO::Socket->new();
            $udp->socket($ai->{family}, SOCK_DGRAM, IPPROTO_UDP) || next;
            my ($bind_port, $b_addr) =
                $ai->{family} == AF_INET  ? unpack_sockaddr_in ($ai->{addr}) :
                $ai->{family} == AF_INET6 ? unpack_sockaddr_in6($ai->{addr}) :
                die "Assertion: Unknown family '$ai->{family}'";
            $bind_port || die "Invalid zero port";
            $udp->connect($ai->{addr}) ||
                die "Assertion: Could not connect UDP probe socket: $^E";
            my $connect_addr = $udp->peername //
                die "Assertion: No getpeername on connected UDP socket: $^E";
            my ($connect_port, $c_addr) =
                $ai->{family} == AF_INET  ? unpack_sockaddr_in ($connect_addr) :
                $ai->{family} == AF_INET6 ? unpack_sockaddr_in6($connect_addr) :
                die "Assertion: Unknown family '$ai->{family}'";
            push @addres, {
                family		=> $ai->{family},
                bind_addr	=> $ai->{addr},
                bind_port	=> $bind_port,
                bind_ip		=> inet_ntop($ai->{family}, $b_addr),
                connect_addr	=> $connect_addr,
                connect_port	=> $connect_port,
                connect_ip	=> inet_ntop($ai->{family}, $c_addr),
            };
        };
        $first_err ||= $@;
    }
    @addres || die $first_err || "No usable resolve for ($host, $port)";
    print STDERR Dumper(\@addres);
    return \@addres;
}

sub new {
    @_ % 2 == 1 || croak "Odd number of arguments";

    my ($class, %arguments) = @_;

    my $blocking = delete $arguments{blocking} // 1;
    my $adb  = delete $arguments{adb}  // $ADB;
    my $host = delete $arguments{host} // $ADB_HOST;
    my $port = delete $arguments{port} // $ADB_PORT;
    my $reresolve = delete $arguments{reresolve};

    croak "Unknown argument " . join(", ", keys %arguments) if %arguments;

    my $client = bless {
        host		=> $host,
        port		=> $port,
        reresolve	=> $reresolve,
        addr_info	=> addr_info($host, $port),
        socket		=> undef,
        blocking	=> $blocking,
        connected	=> undef,
        in		=> "",
        out		=> "",
        commands	=> [],
    }, $class;
    return $client;
}

sub close : method {
    my ($client, $error) = @_;

    defined $client->{connected} || return;

    if ($client->{connected}) {
        $client->{socket}->delete_read if @{$client->{commands}};
        $client->{socket}->delete_write if $client->{out} ne "";
    } elsif ($client->{socket}) {
        $client->{socket}->delete_write;
        $client->{socket}->delete_error;
    }
    $client->{connected} = undef;
    if ($client->{socket}) {
        close($client->{socket}) || die "Assertion: Could not close socket: $!";
        $client->{socket} = undef;
    }
    $client->{in}  = "";
    $client->{out} = "";
    my $command = $client->{commands}[0];
    @{$client->{commands}} = ();
    if ($error) {
        $command || confess("Assertion: adb socket error without pending command");
        my $callback = $command->[1] || $CALLBACK_DEFAULT;
        $callback->($client, $error);
    }
}

sub DESTROY {
    my ($client) = @_;

    print STDERR "DESTROY $client\n" if $debug;

    $client->close;
}

sub wait : method {
    my ($client) = @_;

    $client->{result} = undef;
    mainloop();
    my $result = $client->{result} //
        die "Assertion: Exit mainloop without setting result";
    $client->{result} = undef;
    die $result->[0] if $result->[0];
    wantarray || return $result->[1];
    shift @$result;
    return @$result;
}

sub callback_default {
    die $_[1] if $_[1];
}

sub callback_blocking {
    my $loop_level = loop_level();
    return sub {
        my $client = shift;
        $client->{result} = \@_;
        unloop($loop_level);
    };
}

sub _set_sock_props {
    my ($client) = @_;

    my $sockaddr = getpeername($client->{socket}) ||
        croak "Could not getpeername: $!";
    my $family = sockaddr_family($sockaddr);
    if ($family == AF_INET) {
        my ($port, $address) = unpack_sockaddr_in($sockaddr);
        $client->{local_host} = inet_ntop($family, $address);
        $client->{local_port} = $port;
    } elsif ($family == AF_INET6) {
        my ($port, $address, $scope_id, $flowinfo) = unpack_sockaddr_in6($sockaddr);
        $client->{local_host} = inet_ntop($family, $address);
        $client->{local_port} = $port;
    } else {
        die "Unknown address family '$family'";
    }
    $client->{family} = $family;
    $client->{connected} = 1;
    $client->_write(1) if @{$client->{commands}};
}

sub write : method {
    my ($client, $command, $nr, $callback) = @_;

    utf8::downgrade($command);
    length $command < 2**16 || confess("Assertion: Command too long");
    $client->_write(1) if
        1 == push(@{$client->{commands}}, [$command, $callback, $nr]) &&
        $client->{connected};
}

sub _write {
    my ($client, $initial) = @_;

    my $command = $client->{commands}[0] //
        confess("Assertion: _write without commands");
    $client->{out} eq "" ||
        confess("Assertion: _write with pending data");
    my $len = sprintf("%04X", length $command->[0]);
    length $len == 4 || confess("Assertion: Command too long");
    $client->{out} .= $len;
    $client->{out} .= $command->[0];
    $client->{connected} || return;
    $client->{socket}->add_write(sub { $client->_writer });
    $client->{socket}->add_read(sub { $client->_on_read }) if $initial;
}

sub _writer {
    my ($client) = @_;

    die "Assertion: client out buffer is in utf8" if
        utf8::is_utf8($client->{out});
    my $rc = syswrite($client->{socket}, $client->{out}, $BLOCK_SIZE);
    if ($rc) {
        substr($client->{out}, 0, $rc, "");
        $client->{socket}->delete_write if $client->{out} eq "";
        return;
    }
    die "Assertion: Length 0 write" if defined $rc;
    return if $! == EAGAIN || $! == EINTR || $! == EWOULDBLOCK;
    $client->close("Unexpected error writing to adb socket: $!");
}

sub _on_read {
    my ($client) = @_;

    my $rc = sysread($client->{socket}, my $buffer, $BLOCK_SIZE);

    if (!$rc) {
        # Handle EOF and error
        return if !defined $rc && ($! == EAGAIN || $! == EINTR || $! == EWOULDBLOCK);
        my $msg = defined $rc ? "Unexpected EOF from adb socket" :
            "Unexpected error reading from adb socket: $!";
        $client->close($msg);
        return;
    }

    # Normal input processing
    $client->{in} .= $buffer;
    die "Assertion: client in buffer is in utf8" if
        utf8::is_utf8($client->{in});
    length $client->{in} >= 4 || return;
    my $status = substr($client->{in}, 0, 4);
    my $nr = 1;
    if ($status eq "OKAY") {
        $nr = $client->{commands}[0][2] || 0;
    } elsif ($status ne "FAIL") {
        $client->close("Assertion: Unknown  status '$status' from adb socket");
        return;
    }
    print STDERR "nr=$nr, in='$client->{in}'\n";
    my $result;
    if ($nr) {
        length $client->{in} >= 8 || return;
        my $len = substr($client->{in}, 4, 4);
        if ($len !~ /^[0-9A-Fa-f]{4}\z/) {
            $client->close("Assertion: Invalid length '$len' from adb socket");
            return;
        }
        $len = hex($len)+8;
        length $client->{in} >= $len || return;
        $result = substr($client->{in}, 0, $len, "");
        substr($result, 0, 8, "");
    }
    my $command = shift @{$client->{commands}} ||
        $client->close("Assertion: read without pending command");
    if (@{$client->{commands}}) {
        $client->_write;
    } else {
        $client->{socket}->delete_read;
    }
    $command->[1] || return;
    if ($status eq "FAIL") {
        $command->[1]->($client, $result || "FAIL without reason given");
    } else {
        $command->[1]->($client, undef, $result // ());
    }
}

sub socket {
    return shift->{socket};
}

sub family {
    return shift->{family};
}

sub local_host {
    return shift->{local_host};
}

sub local_port {
    return shift->{local_port};
}

sub connect : method {
    # print STDERR "connect(@_)\n";
    @_ % 2 == 1 || croak "Odd number of arguments";
    my ($client, %arguments) = @_;

    croak "Already connecting or connected" if defined $client->{connected};

    my $host = delete $arguments{host} // $ADB_HOST;
    my $port = delete $arguments{port} // $ADB_PORT;
    my $callback = $client->{blocking} ? $client->callback_blocking : delete $arguments{callback} || $CALLBACK_DEFAULT;
    my $start = delete $arguments{start} // 1;

    croak "Unknown argument " . join(", ", keys %arguments) if %arguments;

    $client->{peer_host} = $host;
    $client->{peer_port} = $port;
    $client->{started} = !$start;

    my $socket = do {
        # Make sure CLO_EXEC is set
        local $^F = -1;
        IO::Socket::IP->new(
            PeerHost	=> $host,
            PeerPort	=> $port,
            Blocking	=> 0);
    };
    $client->{connected} = 0;
    $client->{connect_callback} = $callback;
    if ($socket) {
        $socket->add_write(sub { $client->_on_connect });
        $socket->add_error(sub { $client->_on_connect });
        $client->{socket}  = $socket;
    } else {
        my $err = $@;
        $err =~ s/\s+\z//;
        $client->{error} = ADB::Client::Timer->new(0, sub { $client->_on_connect_error($err) });
    }
    return $client->wait if $client->{blocking};
}

sub _on_connect {
    my ($client) = @_;

    my $connected = $client->{socket}->connect;
    return if !$connected && ($! == EINPROGRESS || $! == EWOULDBLOCK);
    $client->{socket}->delete_write();
    $client->{socket}->delete_error();
    if ($connected) {
        $client->_set_sock_props();
        my $callback = $client->{connect_callback};
        $client->{connect_callback} = undef;
        $callback->($client, undef);
        return;
    }
    close($client->{socket}) || die "Assertion: Could not close socket: $!";
    $client->{socket} = undef;
    if (!$client->{started}) {
        adb_server_start(blocking => 0,
                         callback => sub { $client->_on_start(@_) });
        return;
    }
    my $msg = "Could not connect to $client->{peer_host}:$client->{peer_port}: $!";
    $client->{connected} = undef;
    my $callback = $client->{connect_callback};
    $client->{connect_callback} = undef;
    $callback->($client, $msg, my $err = $!);
}

sub _on_start {
    my ($client, $errmsg) = @_;

    $client->{connected} = undef;
    my $callback = $client->{connect_callback};
    if ($errmsg) {
        $client->{connect_callback} = undef;
        $callback->($client, $errmsg);
    } else {
        local $client->{blocking} = 0;
        $client->connect(
            start	=> 0,
            host	=> $client->{peer_host},
            port	=> $client->{peer_port},
            callback	=> $callback);
    }
}

sub _on_connect_error {
    my ($client, $err) = @_;

    $client->{connected} = undef;
    my $msg = "Could not connect to $client->{peer_host}:$client->{peer_port}: $err";
    my $callback = $client->{connect_callback};
    $client->{connect_callback} = undef;
}

# Make sure callback is called during event processing
sub _immediate {
    my ($callback, @args) = @_;
    my $addr;
    my $timer = ADB::Client::Timer->new(
        0,
        sub {
            delete $timers{$addr};
            $callback->(@args);
        });
    $addr = refaddr($timer);
    $timers{$addr} = $timer;
}

sub resolve {
    my ($client) = @_;
    $client->{addr_info} = addr_info($client->{host}, $client->{port});
}

sub adb_server_start {
    @_ % 2 == 1 || croak "Odd number of arguments";

    my ($client, %arguments) = @_;
    my $kill = delete $arguments{kill};
    my $version_min = delete $arguments{version_min} || 0;
    my $callback = $client->{blocking} ? $client->callback_blocking :
        delete $arguments{callback} || $CALLBACK_DEFAULT;
    croak "Unknown argument " . join(", ", keys %arguments) if %arguments;

    $version_min =~ /^[1-9][0-9]*\z|^0\z/ ||
        croak "Version_min is not a positive integer";
    $version_min <= 2**16 || croak "Version_min out of range";

    $client->resolve() if $client->{reresolve};

    _immediate(\&_adb_server_start, $client, $kill, int($version_min), $callback);
    return $client->wait if $client->{blocking};
}

sub _adb_server_start {
    my ($client, $kill, $version_min, $callback) = @_;

    caller_info("Server_start");
    my $context = {
        # context
        kill		=> $kill,
        version_min	=> $version_min,
        callback	=> $callback,
        address		=> $client->{addr_info},
        address_i	=> -1,
        socket		=> undef,
        in		=> "",
        out		=> "",
    };
    _adb_server_check($client, $context);
    return;
    die "Boem";
    my ($host, $port, $callback);
    my $connect_port = $port =~ /^\d+\z/a ? $port :
        getservbyname($port, "tcp") // croak "Unknown service '$port'";
    $connect_port > 0 || croak "Port '$port' should be positive";
    $connect_port < 2**16 || croak "Port '$port' should be below 65536";
    # Can give a confusing fail if all UDP ports are in use
    my $socket = IO::Socket::IP->new(
        Proto		=> IPPROTO_UDP,
        LocalHost	=> $host,
        LocalPort	=> 0) || die "adb_server_start: $@";
    my $bind_ip = $socket->sockhost // die "Assertion: No sockhost";
    #close($socket) || die "Assertion: Could not close socket: $!";
    #$socket = IO::Socket::IP->new(
    #    Proto		=> IPPROTO_UDP,
    #    PeerHost	=> $bind_ip,
    #    # Udp connect sends nothing so any non-zero port will do
    #    PeerPort	=> 1,
    #    LocalHost	=> $bind_ip,
    #    LocalPort	=> 0) || die "Assertion: adb_server_start cannot UDP connect to local address: $@";
    my $sockaddr = $socket->sockname;
    my $family = sockaddr_family($sockaddr) //
        die "Assertion: No family in probe UDP socket getsockname result";
    $socket->connect($sockaddr) ||
        die "Assertion: Could not UDP self connect: $!";
    my $connect_ip = $socket->peerhost;
    close($socket) || die "Could not close UDP probe socket: $!";
    $socket = IO::Socket::IP->new(
        Family		=> $family,
        Blocking	=> 0,
        Sockopts	=> [[IPPROTO_TCP, TCP_NODELAY]]) ||
        die "Could not create socket: $@";
    die "Boem $family $connect_ip $connect_port";

    # This forces CLO_EXEC off on the new pipe ($^F is 32 bit signed)
    local $^F = 2**31-1;
    $socket = IO::Socket::IP->new(
        LocalHost	=> $ADB_HOST,
        LocalPort	=> $ADB_PORT,
        ReuseAddr	=> 1,
        Listen		=> 128) || die "Could not Listen on nas: $@";
    $socket->blocking(0);

    pipe(my $rd, my $wr) || die "Could not create a pipe: $!";
    pipe(my $erd, my $ewr) || die "Could not create a pipe: $!";
    my $pid = fork() // die "Could not fork: $!";
    if (!$pid) {
        # Child
        eval {
            close($erd) || die "Assertion: Could not close error read side";
            close($rd) || die "Assertion: Could not close read side";

            # Use double fork trick to avoid zombies
            my $pid = fork();
            if (!defined $pid) {
                print "Could not fork: $!\n";
                _exit(1);
            }
            # Quit in parent
            _exit(0) if $pid;

            # Child child
            my $sfd = fileno($socket) // die "Assertion: No fd for soxket";
            my $efd = fileno($ewr) // die "Assertion: No fd for error pipe";
            $ENV{ANDROID_ADB_LOG_PATH} = "/proc/$$/fd/$efd";
            my $fd = fileno($wr) // die "Assertion: No fd for pipe";
            # This version comes from https://dl.google.com/android/repository/platform-tools-latest-linux.zip
            # Silence perl write to STDERR if exec fails
            my @adb;
            if (defined $sfd) {
                @adb = ("/home/ton/src/platform-tools/adb", "-L", "acceptfd:$sfd", "fork-server", "server", "--reply-fd", $fd);
            } else {
                @adb = ("/home/ton/src/platform-tools/adb", "-L", "tcp:5037", "fork-server", "server", "--reply-fd", $fd);
            }
            local $SIG{__WARN__} = sub {};
            exec(@adb) || die "Could not exec adb: $!\n";
        };
        select($wr);
        $| = 1;
        print $@;
        _exit(1);
    }
    # Parent
    close($socket) || die "Assertion: Could not close listen socket";
    close($ewr) || die "Assertion: Could not close error pipe: $!";
    close($wr) || die "Assertion: Could not close pipe: $!";
    $erd->blocking(0);
    $rd->blocking(0);
    $context = {
        in	=> "",
        error	=> "",
        pid	=> $pid,
        rd	=> $rd,
        erd	=> $erd,
        callback	=> $callback,
    };
    $rd->add_read(sub {_on_exec($context)});
    $erd->add_read(sub {_on_exec_error($context)});
}

sub _adb_server_check {
    my ($context) = @_;

    my $addr = $context->{address}[++$context->{address_i}];
    if (!$addr) {
        $context->{address_i} = -1;
        _adb_server_spawn($context);
        return;
    }
    my $socket = IO::Socket->new();
    do {
        # Make sure CLO_EXEC is set
        local $^F = -1;
        $socket->socket($addr->{family}, SOCK_STREAM, IPPROTO_TCP) || next;
    };
    $socket->blocking(0);
    if (connect($socket, $addr->{addr})) {
        _adb_server_connected($context, 0);
    } elsif ($! == EINPROGRESS || $! == EWOULDBLOCK) {
        $addr->{socket} = $socket;
        my $callback = sub { _adb_server_connecting($context) };
        $socket->add_write($callback);
        # $socket->add_error($callback);
        $addr->{timeout} = ADB::Client::Timer->new(
            $CONNECT_TIMEOUT,
            sub { _adb_server_connect_timeout($context) }
        );
    } else {
        _adb_server_connected($context, $!);
    }
}

# Caller is responsible for closing socket if $err != 0
# Caller is responsible for removing any connect handlers
sub _adb_server_connected {
    my ($context, $err) = @_;

    my $addr = $context->{address}[$context->{address_i}];
    if ($err == 0) {
        $addr->{out} = sprintf("%04X%s",
                               length $addr->{command}, $addr->{command});
        $addr->{socket}->add_read (sub { _adb_server_reader($context) });
        $addr->{socket}->add_write(sub { _adb_server_writer($context) });
        $addr->{timeout} = ADB::Client::Timer->new(
            $TRANSACTION_TIMEOUT, sub { _adb_server_timeout($context) });
    } elsif ($err == ECONNREFUSED ||
                 $err == EACCES ||
                 $err == EPERM ||
                 $err == ENETUNREACH ||
                 $err == ETIMEDOUT) {
        # "Normal" connection errors
        _adb_server_check($context);
    } else {
        $context->{callback}->("Unhandled connect error to '$addr->{connect_ip}' on port $addr->{port}: $!");
    }
}

sub _adb_server_writer {
    my ($context) = @_;

    my $addr = $context->{address}[$context->{address_i}];
    my $rc = syswrite($addr->{socket}, $addr->{out}, $BLOCK_SIZE);
    if ($rc) {
        substr($addr->{out}, 0, $rc, "");
        $addr->{socket}->delete_write if $addr->{out} eq "";
    } else {
        return if !defined $rc && ($! == EAGAIN || $! == EINTR || $! == EWOULDBLOCK);
        my $err = $!;
        $addr->{socket}->delete_write;
        $addr->{socket}->delete_read;
        $addr->{socket} = undef;
        $err = defined $rc ? "Assertion: empty write" :
            "Could not write to socket: $err";
        $context->{callback}->($err);
    }
}

sub _adb_server_timeout {
    my ($context) = @_;

    my $addr = $context->{address}[$context->{address_i}];
    if ($addr->{in} eq "") {
        _adb_server_addr_close($context, "Service running on '$addr->{connect_ip}' port $addr->{port} is not responding (timeout $TRANSACTION_TIMEOUT)");
    } else {
        _adb_server_addr_close($context, "Service running on '$addr->{connect_ip}' port $addr->{port} could be adb but is responding too slow (timeout $TRANSACTION_TIMEOUT)");
    }
}

sub _adb_server_addr_close {
    my ($context, $err) = @_;

    my $addr = $context->{address}[$context->{address_i}];
    if ($addr->{out} ne "") {
        $addr->{socket}->delete_write;
        $addr->{out} = "";
    }
    $addr->{socket}->delete_read;
    $addr->{in} = "";
    $addr->{socket} = undef;
    $addr->{timeout} = undef;
    $context->{callback}->($err) if $err;
}

sub _adb_server_reader {
    my ($context) = @_;

    my $addr = $context->{address}[$context->{address_i}];
    my $rc = sysread($addr->{socket}, my $buffer, $BLOCK_SIZE);
    if ($rc) {
        $addr->{in} .= $buffer;
        my $len = length($addr->{in});
        my $bad;
        if ($len - $rc < 4) {
            # $addr->{in} was shorter than 4 bytes
            my $have = $len < 4 ? $len : 4;
            my $prefix = substr($addr->{in}, 0, $have);
            if ($prefix ne substr(OKAY, 0, $have) &&
                    $prefix ne substr(FAIL, 0, $have)) {
                # Answer is neither OKAY nor FAIL
                _adb_server_addr_close($context, "Service running on '$addr->{connect_ip}' port $addr->{port} is not adb (bad prefix '$prefix'");
                return;
            }
        }
        if ($len > 4 && $len - $rc < 8) {
            # $addr->{in} was shorter than 8 bytes and is now more than 4 bytes
            my $code = substr($addr->{in}, 4, $len < 8 ? $len-4 : 8-4);
            if ($code !~ /^[0-9a-fA-F]{1,4}\z/) {
                _adb_server_addr_close($context, "Service running on '$addr->{connect_ip}' port $addr->{port} is not adb (bad code '$code'");
                return;
            }
        }
        if ($len >= 8) {
            my $code = substr($addr->{in}, 4, 4);
            if ($code !~ /^[0-9a-fA-F]{4}\z/) {
                _adb_server_addr_close($context, "Assertion: adb length code is suddenly '$code'");
                return;
            }
            my $more = hex $code;
            if ($len >= $more + 8) {
                if ($len > $more + 8) {
                    _adb_server_addr_close($context, "Service running on '$addr->{connect_ip}' port $addr->{port} looks like adb but sends unxpected extra bytes");
                    return;
                }
                my $status = substr($addr->{in}, 0, 4);
                my $result = substr($addr->{in}, 8, $more);
                if ($status eq FAIL) {
                    _adb_server_addr_close($context, "Command '$addr->{command}' failed: $result");
                } elsif ($status eq OKAY) {
                    _adb_server_addr_close($context);
                    if ($addr->{command} eq "host:version") {
                        if ($result !~ /^[0-9a-fA-F]{4}\z/) {
                            _adb_server_addr_close($context, "Service running on '$addr->{connect_ip}' port $addr->{port} looks like adb but sends a bad version string");
                            return;
                        }
                        my $version = hex $result;
                        if ($version >= $context->{version_min}) {
                            $addr->{version} = $version;
                            $context->{callback}->(undef, $addr);
                            return;
                        }
                        # Reconnect, but now go for the kill
                        $addr->{command} = "host:kill";
                        --$context->{address_i};
                    }
                    # Continue processing
                    _adb_server_check($context);
                } else {
                    _adb_server_addr_close($context, "Assertion: adb status is suddenly '$status'");
                }
                return;
            }
        }
    } elsif (!defined $rc && ($! == EAGAIN || $! == EINTR || $! == EWOULDBLOCK)) {
        return;
    } else {
        my $err = $!;
        if (!defined $rc && $err != ECONNRESET) {
            _adb_server_addr_close($context, "Could not read from socket: $err");
            return;
        }
        my $len = length $addr->{in};
        if ($len < 4) {
            _adb_server_addr_close($context, "Service running on '$addr->{connect_ip}' port $addr->{port} is not adb (bad prefix '$addr->{in}'");
            return;
        }
        my $status = substr($addr->{in}, 0, 4);
        if ($status eq OKAY) {
            if ($len == 4 && $addr->{command} eq "host:kill") {
                _adb_server_addr_close($context);
                # Continue processing
                _adb_server_check($context);
            } else {
                _adb_server_addr_close($context, "Service running on '$addr->{connect_ip}' port $addr->{port} looks like adb but OKAY reponse is truncated");
            }
        } elsif ($status eq FAIL) {
            _adb_server_addr_close($context, "Service running on '$addr->{connect_ip}' port $addr->{port} looks like adb but FAIL reponse is truncated");
        } else {
            _adb_server_addr_close($context, "Assertion: adb status is suddenly '$status'");
        }
    }
}

sub _adb_server_connecting {
    my ($context) = @_;

    my $addr = $context->{address}[$context->{address_i}];
    $addr->{timeout} = undef;
    $addr->{socket}->delete_write();
    my $err = $addr->{socket}->getsockopt(SOL_SOCKET, SO_ERROR);
    if (!defined $err) {
        $err = "Assertion: Could not getsockopt(SOL_SOCKET, SO_ERROR): $!";
        $addr->{socket} = undef;
        $context->{callback}->($err);
        return;
    }
    # We should get a final result
    $addr->{socket} = undef if $err;
    if ($err == EINPROGRESS || $err == EWOULDBLOCK) {
        $context->{callback}->("Assertion: Socket writable while connection still in progress");
        return;
    };
    _adb_server_connected($context, $err);
}

sub _adb_server_connect_timeout {
    my ($context) = @_;

    my $addr = $context->{address}[$context->{address_i}];
    $addr->{timeout} = undef;
    $addr->{socket}->delete_write();
    $context->{callback}->("Connection to '$addr->{connect_ip}' port $addr->{port} timed out (timeout $CONNECT_TIMEOUT)");
}

sub _adb_server_spawn {
    my ($context) = @_;

    my $addr = $context->{address}[++$context->{address_i}];
    die "Bam";
}

sub _on_exec_error {
    my ($context) = @_;

    my $rc = sysread($context->{erd}, my $buffer, $BLOCK_SIZE);
    if ($rc) {
        $context->{error} .= $buffer;
    } elsif (defined $rc) {
        $context->{erd}->delete_read;
        close($context->{erd}) || die "Assertion: Could not close error pipe: $!";
    } else {
        return if $! == EINTR || $! == EAGAIN || $! == EWOULDBLOCK;
        my $err = $!;
        $context->{erd}->delete_read;
        close($context->{erd}) || die "Assertion: Could not close error pipe: $!";
        die "Assertion: Could not read from error pipe: $err"
    }
}

sub _on_exec {
    my ($context) = @_;

    my $rc = sysread($context->{rd}, my $buffer, $BLOCK_SIZE);
    if ($rc) {
        $context->{in} .= $buffer;
    } elsif (defined $rc) {
        $context->{rd}->delete_read;
        close($context->{rd}) || die "Assertion: Could not close pipe: $!";
        my $pid = waitpid($context->{pid}, 0);
        die "Assertion: Failed to wait for $pid" if $pid <= 0;
        if ($context->{in} eq "OK\n") {
            $context->{callback}->() if $context->{callback};
        } else {
            $context->{in} =~ s/\s+\z//;
            my $msg = "Unexpected output from adb server: '$context->{in}' ($context->{error}";
            if ($context->{callback}) {
                $context->{callback}->($msg);
                return;
            } else {
                die $msg;
            }
        }
    } else {
        return if $! == EINTR || $! == EAGAIN || $! == EWOULDBLOCK;
        my $err = $!;
        $context->{rd}->delete_read;
        close($context->{rd}) || die "Assertion: Could not close pipe: $!";
        die "Assertion: Could not read from pipe: $err"
    }
}

sub version {
    @_ % 2 == 1 || croak "Odd number of arguments";
    my ($client, %arguments) = @_;
    my $callback = $client->{blocking} ? $client->callback_blocking : delete $arguments{callback};
    croak "Unknown argument " . join(", ", keys %arguments) if %arguments;

    $client->write("host:version", 1, $callback);
    return $client->wait if $client->{blocking};
}

sub devices {
    @_ % 2 == 1 || croak "Odd number of arguments";
    my ($client, %arguments) = @_;
    my $callback = $client->{blocking} ? $client->callback_blocking : delete $arguments{callback};
    my $long = delete $arguments{long};
    croak "Unknown argument " . join(", ", keys %arguments) if %arguments;

    $client->write($long ? "host:devices-l": "host:devices", 1, $callback);
    return $client->wait if $client->{blocking};
}

sub kill : method {
    @_ % 2 == 1 || croak "Odd number of arguments";
    my ($client, %arguments) = @_;
    my $callback = $client->{blocking} ? $client->callback_blocking : delete $arguments{callback};
    croak "Unknown argument " . join(", ", keys %arguments) if %arguments;

    $client->write("host:kill", 0, $callback);
    return $client->wait if $client->{blocking};
}

1;
__END__
# Below is stub documentation for your module. You'd better edit it!

=head1 NAME

ADB::Client - Perl extension for blah blah blah

=head1 SYNOPSIS

  use ADB::Client;
  blah blah blah

=head1 DESCRIPTION

Stub documentation for ADB::Client, created by h2xs. It looks like the
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
