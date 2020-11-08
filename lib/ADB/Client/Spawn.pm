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
    other	=> [qw($ADB)];

use constant {
    OK => "OK\n",
    READAHEAD	=> 80,
};

use ADB::Client::Events qw(timer immediate);
use ADB::Client::Utils qw(info display_string $DEBUG $QUIET);
use ADB::Client::SpawnRef;

our @CARP_NOT = qw(ADB::Client::ServerStart);

our $BLOCK_SIZE = 65536;
our $LISTEN	= 128;
our $SPAWN_TIMEOUT = 10;
our $TERM_TIMEOUT = 2;
our $KILL_TIMEOUT = 2;
our $ADB = "adb";

my $objects = 0;
my (%starters, $ended);

sub delete {
    my ($starter, $deleted) = @_;

    $deleted || delete $starters{$starter->{key}};
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

    if (my @client_refs = @{$starter->{client_refs}}{sort { $a <=> $b } keys %{$starter->{client_refs}}}) {
        $_->{starter} = undef for @client_refs;
        # The DESTROY should clean out $starter->{client_refs}
        %{$starter->{client_refs}} = ();
        $starter->delete;
        # don't naively try to optimise this without timers
        # join() can call this method and when join returns the caller does not
        # yet expect the carpet to have been yanked out from under him
        for my $client_ref (@client_refs) {
            $client_ref->{timeout} = immediate(
                sub {
                    # It's up to _spawn_result to clear timeout
                    # $client_ref->{timeout} = undef;
                    $client_ref->_spawn_result(@msg);
                }
            );
        }
    } else {
        $starter->delete;
    }
}

# Assumes it is being called during an event callback
sub join {
    my ($class, $client_ref, $bind_addr, $unlog) = @_;
    # $unlog = $unlog ? 1 : 0;
    $unlog = $unlog ? 1 : -1;

    # info("Spawn::join($class, $client_ref, $ip, $port, $unlog)") if $DEBUG;

    $client_ref->fatal("Already spawning an ADB server") if
        $client_ref->{starter};

    # Make sure we don't lose events. Caller should set a timeout AFTER join()
    $client_ref->fatal("Already have a timeout") if $client_ref->{timeout};

    my $family = sockaddr_family($bind_addr);
    my ($port, $packed_ip) =
        $family == AF_INET  ? unpack_sockaddr_in ($bind_addr) :
        $family == AF_INET6 ? unpack_sockaddr_in6($bind_addr) :
        return "Assertion: Unknown family $family";
    my $ip = inet_ntop($family, $packed_ip);

    my $key = "$ip:$port";
    my $starter = $starters{$key};
    if ($starter) {
        $client_ref->{adb} eq $starter->{adb} ||
            return "Attempt to start '$client_ref->{adb}' on $ip port $port while already busy starting '$starter->{adb}' there";
        $unlog == $starter->{unlog} ||
            return "Attempt to start with unlog '$unlog' on $ip port $port while already busy starting with '$starter->{unlog}' there";
        $client_ref->{adb_socket} == $starter->{adb_socket} ||
            return "Attempt to start with adb_socket '$client_ref->{adb_socket}' on $ip port $port while already busy starting with '$starter->{adb_socket}' there";
        return ADB::Client::SpawnRef->new($starter, $client_ref);
    }

    $starters{$key} = $starter = bless {
        key		=> $key,
        adb		=> $client_ref->{adb},
        adb_socket	=> $client_ref->{adb_socket},
        unlog		=> $unlog,
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
    eval {
        # This forces CLO_EXEC off ($^F is 32 bit signed)
        local $^F = 2**31-1;
        pipe(my $rd, my $wr) || die "Could not create adb pipe: $^E\n";
        my $fd = fileno($wr) // die "Assertion: No fd for adb pipe";
        my ($log_rd, $log_wr, $log_fd, $accept_socket, $accept_fd);
        if ($unlog) {
            pipe($log_rd, $log_wr) || die "Could not create log pipe: $^E\n";
            $log_fd = fileno($log_wr) //
                die "Assertion: No fd for log pipe";
        }
        if ($starter->{adb_socket}) {
            $accept_fd = fileno($starter->{adb_socket});
            if (defined $accept_fd) {
                $accept_socket = $starter->{adb_socket};
            } else {
                socket($accept_socket ,$family, SOCK_STREAM, IPPROTO_TCP) ||
                    die "Could not create listen socket: $^E\n";
                $accept_fd = fileno($accept_socket) //
                    die "Assertion: No fd for accept socket";
                # adb itself also sets socket to non blocking
                # so this is not really needed
                $accept_socket->blocking(0);
                $family != AF_INET6 ||
                    setsockopt($accept_socket, IPPROTO_IPV6, IPV6_V6ONLY, $starter->{adb_socket} > 0 ? 0 : 1) ||
                    die "Assertion: Could not setsockopt(IPPROTO_IPV6, IPV6_V6ONLY): $^E";
                setsockopt($accept_socket, SOL_SOCKET, SO_REUSEADDR, 1) ||
                    die "Assertion: Could not setsockopt(SOL_SOCKET, SO_REUSEADDR): $^E";
                bind($accept_socket, $bind_addr) ||
                    die "Could not bind($ip, $port): $^E\n";
                listen($accept_socket, $LISTEN) ||
                    die "Could not listen on port $port at $ip: $^E\n";
            }
        }

        # This forces CLO_EXEC on ($^F is 32 bit signed)
        $^F = -1;
        pipe(my $exec_rd, my $exec_wr) ||
            die "Could not create exec pipe: $^E";

        my $opt_L =
            $accept_socket ? "acceptfd:$accept_fd" :
            $ip eq "127.0.0.1" || $ip eq "::" ? "tcp:$port" :
            "tcp:$ip:$port";
        my @opt_a = !$accept_fd && $ip eq "::" ? "-a" : ();

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
                my @adb = ($client_ref->{adb}, @opt_a, "-L", $opt_L, "fork-server", "server", "--reply-fd", $fd);
                if (defined $log_fd) {
                    $^F = 2;
                    open(STDERR, ">&", $log_wr) ||
                        die "Could not dup log pipe to STDERR: $^E";
                    if ($unlog > 0) {
                        # We'd like to use "adb nodaemon server" which writes
                        # to STDERR, but unfortunately it doesn't support
                        # --reply-fd, so we don't know if/when the server runs
                        $ENV{ANDROID_ADB_LOG_PATH} = "/proc/$$/fd/$log_fd";
                    }
                }
                local $SIG{__WARN__} = sub {};
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
    };
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
            $starter->close("Could not start '$starter->{adb}': $starter->{in}");
            return;
        }

        if (!$starter->{pid_adb}) {
            $starter->close("Assertion: No pid for forked process");
            return;
        }

        $starter->{timeout} = timer($SPAWN_TIMEOUT, sub {$starter->_timeout("TERM")});

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
    if (!kill $signal, $starter->{pid_adb}) {
        if ($! != ESRCH) {
            $starter->close("Could not kill unresponsive ADB at pid $starter->{pid_adb}: $^E");
            return;
        }
    } else {
        $starter->{killed} = $signal;
        if ($signal ne "KILL") {
            $starter->{timeout} = timer($TERM_TIMEOUT, sub {$starter->_timeout("KILL")});
            return;
        }
    }
    $starter->{timeout} = timer($KILL_TIMEOUT, sub {$starter->_timeout});
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
                $starter->close(undef);
            } else {
                # _nok always does a close
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
            $starter->close(undef);
        } elsif ($starter->{log_rd}) {
            $starter->{rd}->delete_read;
            $starter->{rd} = undef;
            # We'll wait for $starter->{log_rd} to finish
        } else {
            # _nok always does a close
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

    if ($starter->{in} eq "") {
        if (my ($line) = $starter->{log_in} =~ /(\S.*\S)\s*\z/) {
            $starter->close("Could not successfully start '$starter->{adb}': $line", 1);
            return;
        }
        $starter->close("Could not successfully start '$starter->{adb}': Reason unknown", 1);
    } else {
        my $response = display_string($starter->{in});
        $starter->close("Could not successfully start '$starter->{adb}': Unexpected response $response)");
    }
}

sub objects {
    return $objects;
}

END {
    # info("END Spawn") if $DEBUG;
    info("Still have %d %s objects at program end", $objects, __PACKAGE__) if !$QUIET && $objects;
    $ended = 1;
    %starters = ();
}

1;
