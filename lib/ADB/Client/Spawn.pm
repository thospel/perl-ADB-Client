package ADB::Client::Spawn;
use strict;
use warnings;

our $VERSION = '1.000';

use POSIX qw(_exit);
use Errno qw(EINTR EAGAIN EWOULDBLOCK ESRCH);
use Carp;
use Socket qw(AF_INET AF_INET6 SOCK_STREAM IPPROTO_TCP SOL_SOCKET SO_REUSEADDR
              IPV6_V6ONLY IPPROTO_IPV6
              sockaddr_family inet_ntop unpack_sockaddr_in unpack_sockaddr_in6);

use Exporter::Tidy
    other	=> [qw(%IP_ANY $ADB
                       $ADB_SPAWN_TIMEOUT $SIGTERM_TIMEOUT $SIGKILL_TIMEOUT)];

use constant {
    OK => "OK\n",
    READAHEAD	=> 80,
};

use ADB::Client::Events qw(timer immediate);
use ADB::Client::Utils qw(info display_string  ip_port_from_addr is_listening
                          $DEBUG $QUIET IPV6);
use ADB::Client::SpawnRef;

our @CARP_NOT = qw(ADB::Client::ServerStart);

our $BLOCK_SIZE = 65536;
our $LISTEN	= 128;
# How long a Spawn object waits for an adb server to start up
our $ADB_SPAWN_TIMEOUT = 20;
our $SIGTERM_TIMEOUT = 2;
our $SIGKILL_TIMEOUT = 2;
our $ADB = "adb";
# Which binds will get the -a option
our %IP_ANY = (
    "::" => 1,
    # Also add 0.0.0.0 though on IPv6 hosts this will also listen on ::
    "0.0.0.0" => 1,
);

my $objects = 0;
my $spawns = 0;
my $kills = {
    TERM	=> 0,
    KILL	=> 0,
};
my (%starters, $ended);

sub delete {
    my ($starter, $deleted) = @_;

    if ($starter->{rd} && $starter->{pid_adb}) {
        # Getting here should only be possible during global cleanup ($ended=1)
        # with an adb server started but never received an answer (OK or close)
        # In principle it could still be starting up, but likely it's hanging
        warn("Trying to KILL $starter->{pid_adb}");
        kill "KILL", $starter->{pid_adb};
        $starter->{pid_adb} = undef;
    }
    $starter->{timeout} = undef;
    if ($starter->{exec_rd}) {
        $starter->{exec_rd}->delete_read;
        $starter->{exec_rd} = undef;
        $starter->{rd} = undef;
        $starter->{log_rd} = undef;
    } else {
        if ($starter->{rd}) {
            $starter->{rd}->delete_read;
            $starter->{rd} = undef;
        }
        if ($starter->{log_rd}) {
            $starter->{log_rd}->delete_read;
            $starter->{log_rd} = undef;
        }
    }
    if ($starter->{pid}) {
        # This can take some time, but it should be short due to the double fork
        # We shouldn't get here anyways, normal processing will do a waitpid
        # after exec_rd closes
        my $pid = waitpid($starter->{pid}, 0);
        warn("Assertion: Failed to wait for $pid") if $pid <= 0;
        $starter->{pid} = undef;
    }

    return if $deleted;

    delete $starters{$starter->{key}};
    if (%{$starter->{client_refs}}) {
        warn("Assertion: Still have client_refs during destruction of $starter") if $DEBUG || !$ended;
        my @client_refs = values %{$starter->{client_refs}};
        %{$starter->{client_refs}} = ();
        $_->{starter} = undef for @client_refs;
    }
}

sub DESTROY {
    --$objects;
    info("DESTROY @_") if $DEBUG;
    shift->delete(1);
}

sub close : method {
    my ($starter, @msg) = @_;

    if (!defined $msg[0]) {
        # Success
        if ($starter->{pid_adb}) {
            push @msg, $starter->{pid_adb};
        } else {
            @msg = "Assertion: adb started without pid";
        }
    }

    $starter->{pid_adb} = undef;

    for my $client_ref (@{$starter->{client_refs}}{sort { $a <=> $b } keys %{$starter->{client_refs}}}) {
        # The DESTROY should clean out $starter->{client_refs}
        $client_ref->{starter} = undef;
        # don't naively try to optimise this without timers
        # join() can call this method and when join returns the caller does not
        # yet expect the carpet to have been yanked out from under him
        $client_ref->{timeout} = immediate(
            sub {
                # It's up to _spawn_result to clear timeout
                # $client_ref->{timeout} = undef;
                $client_ref->_spawn_result(@msg);
            }
        );
    }
    $starter->delete;
}

sub _spawn {
    my ($starter) = @_;

    ++$spawns;
    # This forces CLO_EXEC off ($^F is 32 bit signed)
    local $^F = 2**31-1;
    pipe(my $rd, my $wr) || die "Could not create adb pipe: $^E\n";
    my $fd = fileno($wr) // die "Assertion: No fd for adb pipe";
    my ($log_rd, $log_wr, $log_fd, $accept_socket, $accept_fd);
    if (1) {
        pipe($log_rd, $log_wr) || die "Could not create log pipe: $^E\n";
        $log_fd = fileno($log_wr) //
            die "Assertion: No fd for log pipe";
    }
    if ($starter->{adb_socket}) {
        $accept_fd = fileno($starter->{bind_addr});
        if (defined $accept_fd) {
            $accept_socket = $starter->{bind_addr};
        } else {
            socket($accept_socket ,$starter->{family}, SOCK_STREAM, IPPROTO_TCP) ||
                die "Could not create listen socket: $^E\n";
            $accept_fd = fileno($accept_socket) //
                die "Assertion: No fd for accept socket";
            # adb itself also sets socket to non blocking
            # so this is not really needed
            $accept_socket->blocking(0);
            $starter->{family} != AF_INET6 ||
                setsockopt($accept_socket, IPPROTO_IPV6, IPV6_V6ONLY, $starter->{adb_socket} > 0 ? 0 : 1) ||
                die "Assertion: Could not setsockopt(IPPROTO_IPV6, IPV6_V6ONLY): $^E";
            setsockopt($accept_socket, SOL_SOCKET, SO_REUSEADDR, 1) ||
                die "Assertion: Could not setsockopt(SOL_SOCKET, SO_REUSEADDR): $^E";
            bind($accept_socket, $starter->{bind_addr}) ||
                die "Could not bind($starter->{ip}, $starter->{port}): $^E\n";
            listen($accept_socket, $LISTEN) ||
                die "Could not listen on port $starter->{port} at $starter->{ip}: $^E\n";
            $starter->{bind_addr} = $accept_socket;
        }
    }

    # This forces CLO_EXEC on ($^F is 32 bit signed)
    $^F = -1;
    pipe(my $exec_rd, my $exec_wr) ||
        die "Could not create exec pipe: $^E";

    my $opt_L =
        $accept_socket ? "acceptfd:$accept_fd" :
        $starter->{ip} eq "127.0.0.1" || $IP_ANY{$starter->{ip}} ?
        "tcp:$starter->{port}" : "tcp:$starter->{ip}:$starter->{port}";
    my @opt_a = !$accept_fd && $IP_ANY{$starter->{ip}} ? "-a" : ();

    my $pid = fork() // die "Could not fork: $^E";
    if (!$pid) {
        # Child
        eval {
            select($exec_wr);
            $| = 1;
            close($exec_rd) ||
                die "Assertion: Error closing exec reader: $^E";
            if ($log_rd) {
                close($log_rd) ||
                    die "Assertion: Error closing log reader: $^E";
            }
            close($rd) ||
                die "Assertion: Error closing adb reader: $^E";
            # Use double fork trick to avoid zombies
            my $pid = fork() // die "Could not fork: $^E";
            # Parent
            _exit(0) if $pid;
            print $exec_wr "PID=$$\n";
            my @adb = ($starter->{adb}, @opt_a, "-L", $opt_L, "fork-server", "server", "--reply-fd", $fd);
            if (defined $log_fd) {
                $^F = 2;
                open(STDERR, ">&", $log_wr) ||
                    die "Could not dup log pipe to STDERR: $^E";
                if ($starter->{unlog} > 0) {
                    # We'd like to use "adb nodaemon server" which writes
                    # to STDERR, but unfortunately it doesn't support
                    # --reply-fd, so we don't know if/when the server runs
                    $ENV{ANDROID_ADB_LOG_PATH} = "/proc/$$/fd/$log_fd";
                }
            }
            no warnings "exec";
            exec(@adb) || die "$^E\n";
        };
        my $err = $@ || "Assertion: Missing error";
        eval { print $exec_wr "ERR=$err" };
        _exit(1);
    }
    # Parent
    $starter->{pid} = $pid;
    close($exec_wr) || die "Assertion: Error closing exec writer: $^E";
    if ($log_wr) {
        close($log_wr) ||
            die "Assertion: Error closing log writer: $^E";
    }
    close($wr) || die "Assertion: Error closing adb writer: $^E";

    $exec_rd->blocking(0);
    $exec_rd->add_read(sub { $starter->_reader_exec });
    $starter->{exec_rd} = $exec_rd;
    $starter->{log_rd}  = $log_rd;

    $rd->blocking(0);
    $starter->{rd} = $rd;
}

# Assumes it is being called during an event callback
sub join {
    my ($class, $client_ref, $bind_addr) = @_;

    # info("Spawn::join($class, $client_ref, $ip, $port)") if $DEBUG;

    $client_ref->fatal("Already spawning an ADB server") if
        $client_ref->{starter};

    # Make sure we don't lose events. Caller should set a timeout AFTER join()
    $client_ref->fatal("Already have a timeout") if $client_ref->{timeout};

    my ($addr, $adb_socket);
    if (defined fileno $bind_addr) {
        $addr = getsockname($bind_addr) ||
-            return "Cannot getsockname: $^E";
        if (!eval { is_listening($bind_addr) }) {
            $@ || return "Socket is not listening";
            $@ =~ s/ at .*//s;
            return "$@";
        }
    } else {
        $addr = $bind_addr;
    }
    my ($ip, $port, $family) = ip_port_from_addr($addr);

    if (defined fileno $bind_addr) {
        if ($family == AF_INET6) {
            my $packed = getsockopt($bind_addr, IPPROTO_IPV6, IPV6_V6ONLY) ||
                return "Cannot getsockopt(IPPROTO_IPV6, IPV6_V6ONLY): $^E";
            $adb_socket = unpack("I", $packed) ? -1 : 1;
        } else {
            $adb_socket = 1;
        }
    } else {
        $adb_socket = $client_ref->{adb_socket} <=> 0;
        $adb_socket = 1 if $adb_socket < 0 && $family != AF_INET6;
    }

    my $key = "$ip:$port";
    my $starter = $starters{$key};
    if ($starter) {
        $client_ref->{adb} eq $starter->{adb} ||
            return "Attempt to start '$client_ref->{adb}' on $ip port $port while already busy starting '$starter->{adb}' there";
        $adb_socket == $starter->{adb_socket} ||
            return "Attempt to start with adb_socket '$adb_socket' on $ip port $port while already busy starting with '$starter->{adb_socket}' there";
        return ADB::Client::SpawnRef->new($starter, $client_ref);
    }

    $starters{$key} = $starter = bless {
        key		=> $key,
        adb		=> $client_ref->{adb},
        adb_socket	=> $adb_socket,
        family		=> $family,
        bind_addr	=> $bind_addr,
        ip		=> $ip,
        port		=> $port,
        unlog		=> 0,
        index		=> 0,
        pid		=> undef,
        pid_adb		=> undef,
        client_refs	=> {},
        in		=> "",
        log_in	=> "",
        rd		=> undef,
        log_rd		=> undef,
        exec_rd		=> undef,
        timeout		=> undef,
    }, $class;
    ++$objects;
    eval { $starter->_spawn };
    if ($@) {
        my $err = $@;
        $err =~ s/\s+\z//;
        $starter->close($err);
        return $err;
    }
    return ADB::Client::SpawnRef->new($starter, $client_ref);
}

sub _reader_exec {
    my ($starter) = @_;

    my $rc = sysread($starter->{exec_rd}, my $buffer, $BLOCK_SIZE);
    if ($rc) {
        $starter->{in} .= $buffer;
    } elsif (defined $rc) {
        # EOF
        $starter->{pid_adb} = $1 if $starter->{in} =~ s/^PID=(\d+)\s*\n//a;

        if ($starter->{in} ne "") {
            $starter->{in} =~ s/^ERR=//;
            $starter->{in} =~ s/\s+\z//;
            $starter->{in} = "Reason unknown" if $starter->{in} eq "";
            $starter->close("Could not start '$starter->{adb}': $starter->{in}");
            return;
        }

        if (!$starter->{pid_adb}) {
            $starter->close("Assertion: No pid for forked process");
            return;
        }

        $starter->{timeout} = timer($ADB_SPAWN_TIMEOUT, sub {$starter->_timeout("TERM")});

        $starter->{exec_rd}->delete_read;
        $starter->{exec_rd} = undef;

        $starter->{rd}->add_read(sub { $starter->_reader });
        $starter->{log_rd}->add_read(sub { $starter->_reader_log }) if
            $starter->{log_rd};

        my $pid = waitpid($starter->{pid}, 0);
        warn("Assertion: Failed to wait for $pid") if $pid <= 0;
        $starter->{pid} = undef;
    } elsif ($! == EAGAIN || $! == EINTR || $! == EWOULDBLOCK) {
    } else {
        # Read error
        $starter->close("Assertion: Error reading from exec pipe: $^E");
    }
}

sub _timeout {
    my ($starter, $signal) = @_;

    if (!$signal) {
        $starter->close("Unresponsive '$starter->{adb}' even with pid $starter->{pid_adb} gone");
        return;
    }
    ++$kills->{$signal};
    if (!kill $signal, $starter->{pid_adb}) {
        if ($! != ESRCH) {
            $starter->close("Could not kill unresponsive ADB at pid $starter->{pid_adb}: $^E");
            return;
        }
    } else {
        $starter->{killed} = $signal;
        if ($signal ne "KILL") {
            $starter->{timeout} = timer($SIGTERM_TIMEOUT, sub {$starter->_timeout("KILL")});
            return;
        }
    }
    $starter->{timeout} = timer($SIGKILL_TIMEOUT, sub {$starter->_timeout});
}

sub _reader_log {
    my ($starter) = @_;

    my $rc = sysread($starter->{log_rd}, my $buffer, $BLOCK_SIZE);
    if ($rc) {
        $starter->{log_in} .= $buffer;
    } elsif (defined $rc) {
        # EOF
        if ($starter->{rd}) {
            $starter->{log_rd}->delete_read;
            $starter->{log_rd} = undef;
            # We'll wait for $starter->{rd} to finish
        } else {
            if ($starter->{killed}) {
                $starter->close("Killed unresponsive '$starter->{adb}' with SIG$$starter->{killed}");
            } elsif ($starter->{in} eq OK) {
                warn("ADB server on $starter->{ip} port $starter->{port} started without logging\n") if $starter->{unlog};
                $starter->close(undef);
            } else {
                # _nok always does a delete
                $starter->_nok();
            }
        }
    } elsif ($! == EAGAIN || $! == EINTR || $! == EWOULDBLOCK) {
    } else {
        # Read error
        $starter->close("Assertion: Error reading from log pipe: $^E");
    }
}

sub _reader {
    my ($starter) = @_;

    my $len = length $starter->{in};
    my $rc = sysread($starter->{rd}, $starter->{in}, READAHEAD - $len, $len);
    if ($rc) {
        if (length $starter->{in} >= READAHEAD) {
            $starter->{in} =~ s/\n/\\n/;
            $starter->close("Started '$starter->{adb}' but got unexpected reply '$starter->{in}...'");
        }
    } elsif (defined $rc) {
        # EOF
        if ($starter->{killed}) {
            $starter->close("Killed unresponsive '$starter->{adb}' with SIG$starter->{killed}");
        } elsif ($starter->{in} eq OK) {
            warn("ADB server on $starter->{ip} port $starter->{port} started without logging\n") if $starter->{unlog};
            $starter->close(undef);
        } elsif ($starter->{log_rd}) {
            $starter->{rd}->delete_read;
            $starter->{rd} = undef;
            # We'll wait for $starter->{log_rd} to finish
        } else {
            # _nok always does a delete
            $starter->_nok();
        }
    } elsif ($! == EAGAIN || $! == EINTR || $! == EWOULDBLOCK) {
    } else {
        # Read error
        $starter->close("Assertion: Error reading from adb pipe: $^E");
    }
}

sub _nok {
    my ($starter) = @_;

    # The program complained: Immediately return error
    if ($starter->{in} ne "") {
        my $response = display_string($starter->{in});
        $starter->close("Could not start '$starter->{adb}': Unexpected response $response)");
        return;
    }

    if (my ($line) = $starter->{log_in} =~ /\s*(.*\S)\s*\z/) {
        $starter->close("Could not start '$starter->{adb}': $line", 1);
        return;
    }

    # We have no idea why the program failed. Try again and intercept logging
    # (we don't do this by default since it stops normal adb logging)
    if (!$starter->{unlog} && $^O eq "linux" && -d "/proc/$$/fd/" && -x _) {
        # Remove all callbacks
        $starter->{pid_adb} = undef;
        $starter->delete(1);

        # And redo
        $starter->{unlog} = 1;
        eval { $starter->_spawn };
        if ($@) {
            my $err = $@;
            $err =~ s/\s+\z//;
            $starter->close($err);
        }
        return;
    }

    $starter->close("Could not start '$starter->{adb}': Reason unknown", 1);
}

sub objects {
    return $objects;
}

sub spawns {
    return $spawns;
}

sub kills {
    return $kills;
}

END {
    # info("END Spawn") if $DEBUG;
    info("Still have %d %s objects at program end", $objects, __PACKAGE__) if !$QUIET && $objects;
    $ended = 1;
    %starters = ();
}

1;
