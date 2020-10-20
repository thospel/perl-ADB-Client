package ADB::Client;
use strict;
use warnings;

our $VERSION = '1.000';

use Carp;
use IO::Socket::IP;
use Socket qw(sockaddr_family unpack_sockaddr_in unpack_sockaddr_in6 inet_ntop inet_ntoa
              AF_INET AF_INET6);
use Errno qw(EINPROGRESS EWOULDBLOCK EINTR EAGAIN);
use Time::Local qw(timegm);
use POSIX qw(_exit);

use ADB::Client::Events;

use Exporter::Tidy
    other	=>[qw(adb_server_start)],
    _map	=> {
        mainloop	=> \&ADB::Client::Events::mainloop,
        event_init	=> \&ADB::Client::Events::init,
    };

$SIG{PIPE} = "IGNORE";

our ($debug, $verbose);
our $BLOCK_SIZE = 65536;

our $CALLBACK_DEFAULT = sub { die $_[1] if $_[1] };

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

sub new {
    @_ % 2 == 1 || croak "Odd number of arguments";
    my ($class, %arguments) = @_;

    my $socket = delete $arguments{socket};

    croak "Unknown argument " . join(", ", keys %arguments) if %arguments;

    my $client = bless {
        socket		=> $socket,
        connected	=> undef,
        in		=> "",
        out		=> "",
        commands	=> [],
    }, $class;
    if ($socket) {
        $client->_set_sock_props();
    }
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

    print STDERR "DESTROY $client\n";

    $client->close;
}

sub _set_sock_props {
    my ($client) = @_;

    my $sockaddr = getpeername($client->{socket}) ||
        croak "Could not getpeername: $!";
    my $family = sockaddr_family($sockaddr);
    if ($family == AF_INET) {
        my ($port, $address) = unpack_sockaddr_in($sockaddr);
        $client->{local_host} = inet_ntoa($address);
        $client->{local_port} = $port;
    } elsif ($family == AF_INET6) {
        my ($port, $address, $scope_id, $flowinfo) = unpack_sockaddr_in6($sockaddr);
        $client->{local_host} = inet_ntop($address);
        $client->{local_port} = $port;
    } else {
        die "Unknown address family '$family'";
    }
    $client->{family} = $family;
    $client->{connected} = 1;
    $client->{socket}->add_read(sub { $client->_on_read }) if @{$client->{commands}};
    $client->{out} eq "" ||
        $client->{socket}->add_write(sub { $client->_writer });
}

sub write : method {
    my ($client, $command, $callback) = @_;

    my $was_empty = $client->{out} eq "";
    utf8::downgrade($command);
    my $len = sprintf("%04X", length $command);
    length $len == 4 || die "Assertion: Command too long (0x$len bytes)";
    $client->{out} .= $len;
    $client->{out} .= $command;
    push @{$client->{commands}}, [$command, $callback];
    $client->{connected} || return;
    $client->{socket}->add_write(sub { $client->_writer }) if $was_empty;
    $client->{socket}->add_read(sub { $client->_on_read }) if @{$client->{commands}} == 1;
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
    return if $! == EAGAIN || $! == EINTR;
    $client->close("Unexpected error writing to adb socket: $!");
}

sub _on_read {
    my ($client) = @_;

    my $rc = sysread($client->{socket}, my $buffer, $BLOCK_SIZE);

    if (!$rc) {
        # Handle EOF and error
        return if !defined $rc && ($! == EAGAIN || $! == EINTR);
        my $msg = defined $rc ? "Unexpected EOF from adb socket" :
            "Unexpected error reading from adb socket: $!";
        $client->close($msg);
        return;
    }

    # Normal input processing
    $client->{in} .= $buffer;
    die "Assertion: client in buffer is in utf8" if
        utf8::is_utf8($client->{in});
    length $client->{in} >= 8 || return;
    my $len = substr($client->{in}, 4, 4);
    if ($len !~ /^[0-9A-F]{4}\z/) {
        $client->close("Assertion: Invalid length '$len' from adb socket");
        return;
    }
    $len = hex($len)+8;
    length $client->{in} >= $len || return;
    my $result = substr($client->{in}, 0, $len, "");
    my $status = substr($result, 0, 4);
    substr($result, 0, 8, "");
    if ($status ne "OKAY" && $status ne "FAIL") {
        $client->close("Assertion: Unknown  status '$status' from adb socket");
    }
    my $command = shift @{$client->{commands}} ||
        $client->close("Assertion: read without pending command");
    @{$client->{commands}} || $client->{socket}->delete_read;
    $command->[1] || return;
    if ($status eq "FAIL") {
        $command->[1]->($client, $result || "FAIL without reason given");
    } else {
        $command->[1]->($client, undef, $result);
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

    my $host = delete $arguments{host} // "127.0.0.1";
    my $port = delete $arguments{port} // 5037;
    my $callback = delete $arguments{callback} || $CALLBACK_DEFAULT;
    my $start = delete $arguments{start} // 1;

    croak "Unknown argument " . join(", ", keys %arguments) if %arguments;

    $client->{peer_host} = $host;
    $client->{peer_port} = $port;
    $client->{started} = !$start;

    my $socket = IO::Socket::IP->new(
        PeerHost	=> $host,
        PeerPort	=> $port,
        Type		=> SOCK_STREAM,
        Blocking	=> 0);
    $client->{connected} = 0;
    $client->{connect_callback} = $callback;
    if (!$socket) {
        my $err = $@;
        $err =~ s/\s+\z//;
        $client->{error} = ADB::Client::Timer->new(0, sub { $client->_on_connect_error($err) });
        return;
    }
    $socket->add_write(sub { $client->_on_connect });
    $socket->add_error(sub { $client->_on_connect });
    $client->{socket}  = $socket;
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
        adb_server_start(callback => sub { $client->_on_start(@_) });
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

sub adb_server_start {
    @_ % 2 == 0 || croak "Odd number of arguments";

    my %arguments = @_;
    my $callback = delete $arguments{callback};
    croak "Unknown argument " . join(", ", keys %arguments) if %arguments;

    # This forces CLO_EXEC off on the new pipe ($^F is 32 bit signed)
    local $^F = 2**31-1;
    pipe(my $rd, my $wr) || die "Could not create a pipe: $!";
    my $pid = fork() // die "Could not fork: $!";
    if (!$pid) {
        # Child
        eval {
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
            my $fd = fileno($wr) // die "Assertion: No fd for pipe";
            local $SIG{__WARN__} = sub {};
            exec("adb", "-L", "tcp:5037", "fork-server", "server", "--reply-fd", $fd) || die "Could not exec adb: $!\n";
        };
        select($wr);
        $| = 1;
        print $@;
        _exit(1);
    }
    # Parent
    close($wr) || die "Assertion: Could not close pipe: $!";
    $rd->blocking(0);
    my $context = {
        in	=> "",
        pid	=> $pid,
        rd	=> $rd,
        callback	=> $callback,
    };
    $rd->add_read(sub {_on_exec($context)});
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
            my $msg = "Unexpected output from adb server: $context->{in}";
            if ($context->{callback}) {
                $context->{callback}->($msg);
                return;
            } else {
                die $msg;
            }
        }
    } else {
        return if $! == EINTR || $! == EAGAIN;
        my $err = $!;
        $context->{rd}->delete_read;
        close($context->{rd}) || die "Assertion: Could not close pipe: $!";
        die "Assertion: Could not read from pipe: $err"
    }
}

sub version {
    @_ % 2 == 1 || croak "Odd number of arguments";
    my ($client, %arguments) = @_;
    my $callback = delete $arguments{callback};
    croak "Unknown argument " . join(", ", keys %arguments) if %arguments;

    $client->write("host:version", $callback);
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
