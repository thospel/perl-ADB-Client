package ADB::Client::ServerStart;
use strict;
use warnings;

# Note: kill and version have their own implementation but should really be
# using the logic from ADB::Client::Ref

our $VERSION = '1.000';

use Carp;
use Scalar::Util qw(weaken);
use IO::Socket qw();
use Socket qw(IPPROTO_TCP IPPROTO_UDP SOCK_DGRAM SOCK_STREAM SOL_SOCKET
              SO_ERROR TCP_NODELAY);
use Errno qw(EWOULDBLOCK EINPROGRESS ECONNREFUSED EACCES EPERM ENETUNREACH
             ETIMEDOUT EAGAIN EINTR ECONNRESET);

use ADB::Client::Events qw(timer immediate);
use ADB::Client::Spawn;
use ADB::Client::Utils qw(dumper addr_info info adb_check_response
                          string_from_value $DEBUG
                          OKAY FAIL FAILED BAD_ADB ASSERTION);

our @CARP_NOT = qw(ADB::Client::Ref);

sub new {
    my ($class, $client_ref, $callback, $arguments) = @_;

    my %context = (
        client_ref	=> $client_ref,
        callback	=> $callback,
        address_i	=> -1,
    );
    weaken($context{client_ref});

    my $connect = !$arguments;
    if ($connect) {
        $client_ref->resolve() if $client_ref->{reresolve};
        $context{address} = $client_ref->{addr_info};
        $context{connect_only} = 1;
    } else {
        $context{kill}		= delete $arguments->{kill};
        $context{version_min}	= delete $arguments->{version_min} || 0;
        $context{adb}		= delete $arguments->{adb} // $client_ref->{adb};
        $context{adb_socket}	= delete $arguments->{adb_socket} // $client_ref->{adb_socket} // 0;
        $context{host}		= delete $arguments->{host};
        $context{port}		= delete $arguments->{port};

        $context{version_min} =~ /^[1-9][0-9]*\z|^0\z/ ||
            croak "Version_min is not a positive integer";
        $context{version_min} <= 2**16 || croak "Version_min out of range";

        if (defined $context{host} || defined $context{port}) {
            $context{host} //= $client_ref->{host};
            $context{port} //= $client_ref->{port};
            $context{address} = addr_info($context{host}, $context{port});
        } else {
            $client_ref->resolve() if $client_ref->{reresolve};
            $context{host} = $client_ref->{host};
            $context{port} = $client_ref->{port};
            $context{address} = $client_ref->{addr_info};
        }
        $context{spawn} = 1;
        croak "Unknown argument " . join(", ", keys %$arguments) if %$arguments;
    }

    my $context = bless \%context, $class;
    $client_ref->child_add($context);

    $context{timeout} = immediate($connect ? sub { $context->_check } : sub { $context->_start });

    # $context->dump;
    return $context;
}

sub dump : method {
    my ($context) = @_;

    local $context->{client_ref} = "--- CLIENT_REF ---";
    dumper($context);
}

sub delete {
    my ($context, $deleted) = @_;

    $context->{timeout} &&= undef;
    $context->{client_ref}->child_delete($context) if
        !$deleted && $context->{client_ref};
    if ($context->{connected}) {
        # Connection established
        if ($context->{out} ne "") {
            $context->{socket}->delete_write;
            $context->{out} = undef;
        }
        $context->{socket}->delete_read;
        $context->{in} = undef;
        $context->{connected} = undef;
    } elsif (defined $context->{connected}) {
        # Connection in progress
        $context->{socket}->delete_write;
        # $context->{socket}->delete_error;
        $context->{connected} = undef;
    }
    $context->{socket} = undef;

    if ($context->{starter}) {
        $context->{starter}{starter}->remove($context->{starter}{index});
        $context->{starter} = undef;
    }
}

sub DESTROY {
    info("DESTROY @_") if $DEBUG;
    shift->delete;
}

sub close : method {
    my $context = shift;

    $context->delete;
    if (@_) {
        my $msg = shift;
        $msg =~ s/\s+\z// if $msg;
        $context->{callback}->($context->{client_ref}{client}, $msg, @_);
    }
}

sub _start {
    my ($context) = @_;

    # Filter out only bindable adresses
    my ($first_err, @address);
    for my $address (@{$context->{address}}) {
        eval {
            my $udp = IO::Socket->new();
            $udp->socket($address->{family}, SOCK_DGRAM, IPPROTO_UDP) || next;
            $udp->bind($address->{bind_addr0}) ||
                die "Could not bind to '$context->{host}' ($address->{bind_ip}): $!\n";
            $address->{command} = $context->{kill} && !$context->{version_min} ?
                "host:kill" : "host:version";
            push @address, $address;
        };
        $first_err ||= $@;
    }
    if (!@address) {
        $context->close($first_err || "No usable bind for ($context->{host}, $context->{port})");
        return;
    }
    $context->{address} = \@address;
    # Start processing
    $context->_check();
}

sub _check {
    my ($context) = @_;

    while (my $addr = $context->{address}[++$context->{address_i}]) {
        my $socket = IO::Socket->new();
        do {
            # Make sure CLO_EXEC is set
            local $^F = -1;
            $socket->socket($addr->{family}, SOCK_STREAM, IPPROTO_TCP) || next;
        };
        $socket->blocking(0);
        if (connect($socket, $addr->{connect_addr})) {
            # SOL_TCP == IPPROTO_TCP
            $socket->setsockopt(IPPROTO_TCP, TCP_NODELAY, 1) //
                warn("Could not set TCP_NODELAY on connecting socket: $!");
            _connected($context, 0);
        } elsif ($! == EINPROGRESS || $! == EWOULDBLOCK) {
            $socket->setsockopt(IPPROTO_TCP, TCP_NODELAY, 1) //
                warn("Could not set TCP_NODELAY on connecting socket: $!");
            $context->{socket} = $socket;
            $context->{connected} = 0;
            my $callback = sub { $context->_connecting };
            $socket->add_write($callback);
            # $socket->add_error($callback);
            $context->{timeout} = timer(
                $context->{client_ref}{connection_timeout},
                sub { $context->_connection_timeout }
            );
        } else {
            $context->_connected($!);
        }
        return;
    }
    if ($context->{spawn}) {
        $context->{address_i}	= -1;
        $context->{first_error} = undef;
        $context->_spawn;
    } elsif ($context->{first_error}) {
        $context->close(@{$context->{first_error}});
    } elsif ($context->{connect_only}) {
        $context->close("Assertion: No failure reason after connection attempts");
    } else {
        $context->close("Assertion: No failure reason after version check after spawn");
    }
}

sub _check_result {
    my ($context, @msg) = @_;

    $context->{first_error} //= \@msg if @msg;
    $context->_check;
}

sub _connection_timeout {
    my ($context) = @_;

    my $addr = $context->{address}[$context->{address_i}];
    $context->close("Connection to port $addr->{connect_port} on $addr->{connect_ip} timed out (timeout $context->{client_ref}{connection_timeout})");
}

sub _connecting {
    my ($context) = @_;

    my $addr = $context->{address}[$context->{address_i}];
    $context->{timeout} = undef;
    $context->{socket}->delete_write();
    $context->{connected} = undef;
    my $err = $context->{socket}->getsockopt(SOL_SOCKET, SO_ERROR);
    if (!defined $err) {
        $context->close("Assertion: Could not getsockopt(SOL_SOCKET, SO_ERROR): $!");
        return;
    }
    $context->{socket} = undef if $err;
    # We should get a final result
    if ($err == EINPROGRESS || $err == EWOULDBLOCK) {
        $context->close("Assertion: Socket writable while connection still in progress");
        return;
    };
    $context->_connected($err);
}

# Caller is responsible for closing socket and unsetting connected if $err != 0
# Caller is responsible for removing any connect handlers
sub _connected {
    my ($context, $err) = @_;

    my $addr = $context->{address}[$context->{address_i}];
    if ($err == 0) {
        if ($context->{connect_only}) {
            # Must make a copy of socket since the delete method will undef it
            my $socket = $context->{socket};
            if ($addr->{peer_addr} = $socket->peername) {
                $context->close(undef, $addr, $socket);
            } else {
                $context->close("Assertion: Could not get peername on connected socket: $^E");
            }
            return;
        }
        $context->{socket}->add_write(sub { $context->_writer });
        $context->{out} = sprintf("%04X%s",
                                  length $addr->{command}, $addr->{command});
        $context->{socket}->add_read (sub { $context->_reader });
        $context->{in} = "";
        $context->{timeout} = timer(
            $context->{client_ref}{transaction_timeout},
            sub { $context->_timeout });
        $context->{connected} = 1;
    } else {
        local $! = $err;
        if ($err == ECONNREFUSED ||
                $err == ECONNRESET ||
                $err == EACCES ||
                $err == EPERM ||
                $err == ENETUNREACH ||
                $err == ETIMEDOUT) {
            # "Normal" connection errors
            # Not sure about ECONNRESET, it's not documented in man 2 connect
            # but I *DO* get it from SOL_SOCKET SO_ERROR. Maybe the connection
            # succeeded but just after that the connection got reset?
            # need to do a packet capture, Can be reproduced by uncommenting the
            # kill 9
            $context->_check_result("Could not connect to port $addr->{connect_port} on $addr->{connect_ip} on : $!");
        } else {
            $context->close("Unhandled connect error to  port $addr->{connect_port} on $addr->{connect_ip}: $!");
        }
    }
}

sub _timeout {
    my ($context) = @_;

    my $addr = $context->{address}[$context->{address_i}];
    if ($context->{in} eq "") {
        $context->close("Service running on $addr->{connect_ip} port $addr->{connect_port} is not responding (timeout $context->{client_ref}{transaction_timeout})");
    } else {
        $context->close("Service running on $addr->{connect_ip} port $addr->{connect_port} could be adb but is responding too slowly (timeout $context->{client_ref}{transaction_timeout})");
    }
}

sub _writer {
    my ($context) = @_;

    my $addr = $context->{address}[$context->{address_i}];
    my $rc = syswrite($context->{socket}, $context->{out}, $context->{client_ref}{block_size});
    if ($rc) {
        substr($context->{out}, 0, $rc, "");
        $context->{socket}->delete_write if $context->{out} eq "";
    } else {
        return if !defined $rc && ($! == EAGAIN || $! == EINTR || $! == EWOULDBLOCK);
        $context->close(defined $rc ? "Assertion: empty write" :
                            "Could not write to socket: $!");
    }
}

sub _reader {
    my ($context) = @_;

    my $addr = $context->{address}[$context->{address_i}];
    my $rc = sysread($context->{socket}, my $buffer, $context->{client_ref}{block_size});
    if ($rc) {
        $context->{in} .= $buffer;
        my ($error, $str) = adb_check_response(
            $context, $rc,
            $addr->{command} eq "host:version" ? -1 : 0,
            # Expect EOF so we will error on spurious bytes
            1) or return;
        if ($error) {
            if ($error == BAD_ADB) {
                $context->close("Service running on $addr->{connect_ip} port $addr->{connect_port} is not adb: $str");
            } elsif ($error == FAILED) {
                $context->close("Command '$context->{command}' failed: $str");
            } elsif ($error == ASSERTION) {
                $context->close($str || "Assertion: no msg from adb_check_response");
            } else {
                $context->close("Assertion: bad error code from adb_check_response");
            }
            return;
        }
        # status is OKAY
        if ($addr->{command} eq "host:kill") {
            # Invariant: $context->{in} is empty beacuse we asked
            # adb_check_response for expect_eof
            # Put back the response.
            $context->{in} = OKAY;
            # Now wait for EOF. Any extra bytes we receive will cause an error
            return;
        }
        # Here we handle "host:version"
        if ($str !~ /^[0-9a-fA-F]{4}\z/) {
            $context->close("Service running on $addr->{connect_ip} port $addr->{connect_port} looks like adb but sends a bad version string");
            return;
        }
        my $version = hex $str;
        if ($version >= $context->{version_min}) {
            $addr->{version} = $version;
            $context->close(undef, $addr);
            return;
        }
        if (!$context->{kill}) {
            $addr->{version} = $version;
            $context->close("ADB service running on $addr->{connect_ip} port $addr->{connect_port} with version '$version', below the minimum of '$context->{version_min}'");
            return;
        }
        $context->close;
        # Reconnect, but now go for the kill
        $addr->{command} = "host:kill";
        --$context->{address_i};
        $context->_check_result;
    } elsif (!defined $rc && ($! == EAGAIN || $! == EINTR || $! == EWOULDBLOCK)) {
        return;
    } else {
        my $err = $!;
        if (!defined $rc && $err != ECONNRESET) {
            $context->close("Could not read from socket: $err");
            return;
        }
        # We should never reach EOF since proper adb responses are already
        # processed when read. So this is not adb or the response was truncated
        # *Except* that for "host:kill we manually put back the OKAY response
        # since we want to wait until the adb server realy closed so we are
        # sure we can reuse the bind address
        my $len = length $context->{in};
        if ($len < 4) {
            if ($context->{in} eq "") {
                $context->close("Service running on $addr->{connect_ip} port $addr->{connect_port} is not adb (empty response)");
            } else {
                my $prefix = string_from_value($context->{in});
                $context->close("Service running on $addr->{connect_ip} port $addr->{connect_port} is not adb: Bad prefix $prefix");
            }
            return;
        }
        if ($addr->{command} eq "host:kill") {
            # Continue processing (next kill)
            $context->close;
            $context->_check_result;
        } else {
            my $status = string_from_value(substr($context->{in}, 0, 4));
            $context->close("Service running on $addr->{connect_ip} port $addr->{connect_port} looks like adb but $status reponse is truncated");
        }
    }
}

sub _spawn {
    my ($context) = @_;

    my $addr = $context->{address}[++$context->{address_i}];
    if ($addr) {
        ADB::Client::Spawn->join($context,
                                   $addr->{bind_addr},
                                   $addr->{unlog});
        return;
    }
    # All spawns failed
    if (!$context->{first_error}) {
        $context->close("Assertion: No more adb binds to try but no failure reason");
        return;
    }

    $addr = $context->{first_error}[1];
    if ($addr->{empty} && !$addr->{unlog} &&
            $^O eq "linux" && -d "/proc/$$/fd/" && -x _) {
        delete $addr->{empty};
        $addr->{unlog} = -1;
        # Retry just the first starter (which failed without response)
        $context->{address} = [$addr];
        $context->{address_i} = -1;
        $context->{first_error} = undef;
        $context->_spawn();
        return;
    }
    $context->close(@{$context->{first_error}});
}

# Result of a previous _spawn
sub _spawn_result {
    my ($context, $error, $empty) = @_;

    my $addr = $context->{address}[$context->{address_i}];
    if ($error) {
        # Error
        $addr->{empty} = 1 if $empty;
        $context->{first_error} //= [$error, $addr, $empty];
        # Try the next addr
        $context->_spawn();
        return;
    }
    # Success
    warn("adb server start unexpectedly succeeded while logging was suppressed") if $addr->{unlog} && $addr->{unlog} < 0;
    $addr->{pid_adb} = $empty;
    if ($context->{version_min}) {
        # Go check that what we started has an acceptable version
        $addr->{command} = "host:version";
        # Can be used to reproduce the ECONNRESET delayed connection error
        # kill 9, $addr->{pid_adb};
        $context->{kill} = undef;
        $context->{spawn} = 0;
        $context->{address} = [$addr];
        $context->{address_i}	= -1;
        $context->{first_error} = undef;
        $context->_check;
        return;
    }
    $context->close(undef, $addr);
}

1;
