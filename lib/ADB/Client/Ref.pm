package ADB::Client::Ref;
use strict;
use warnings;

our $VERSION = '1.000';

use Carp;
use Scalar::Util qw(weaken refaddr);
# use IO::Socket::IP;
use Errno qw(EINPROGRESS EWOULDBLOCK EINTR EAGAIN ECONNRESET);

use ADB::Client::Events qw(mainloop unloop loop_level realtime clocktime
                           $BASE_REALTIME $BASE_CLOCKTIME $CLOCK_TYPE);
use ADB::Client::ServerStart;
use ADB::Client::Utils qw(addr_info info caller_info dumper adb_check_response
                          display_string
                          FAILED BAD_ADB ASSERTION
                          $DEBUG $VERBOSE);

use Exporter::Tidy
    other	=>[qw(addr_info mainloop unloop loop_level dumper
                      info caller_info realtime clocktime
                      COMMAND_NAME @SIMPLE_COMMANDS
                      $BASE_REALTIME $BASE_CLOCKTIME $CLOCK_TYPE
                      $CALLBACK_DEFAULT
                      $ADB_HOST $ADB_PORT $ADB $ADB_SOCKET $DEBUG $VERBOSE)];

use constant {
    # Index in @SIMPLE_COMMAND element
    COMMAND_NAME	=> 0,
    COMMAND	=> 1,
    NR_RESULTS	=> 2,
    EXPECT_EOF	=> 3,

    # Index in commands element
    COMMAND_REF	=> 0,
    CALLBACK	=> 1,
};

our @CARP_NOT = qw(ADB::Client);

our $BLOCK_SIZE = 65536;

our $CALLBACK_DEFAULT	= \&callback_default;
our $ADB = "adb";
our $ADB_HOST	= "127.0.0.1";
our $ADB_SOCKET	= undef;
our $ADB_PORT	= 5037;
our $TRANSACTION_TIMEOUT = 10;
our $CONNECT_TIMEOUT = 10;

our @SIMPLE_COMMANDS = (
    # command, number of results, expect close, callback
    # See the index in command array constants
    ["version"		=> "host:version", -1, 1],
    ["kill"		=> "host:kill", 0, 1],
    ["features"		=> "host:features", -1, 1],
    ["remount"		=> "remount:", 9**9**9, 1],
    ["devices"		=> "host:devices", -1, 1],
    ["devices_long"	=> "host:devices-l", -1, 1],
    ["transport_usb"	=> "host:transport-usb", 0, 0],
    ["transport_tcp"	=> "host:transport-local", 0, 0],
    ["transport_any"	=> "host:transport-any", 0, 0],
    ["tport_any"	=> "host:tport:any", 8, 0],
);

# Notice that the client argument isn't yet blessed at this point
sub new {
    @_ % 2 == 0 || croak "Odd number of arguments";

    my ($class, $client, %arguments) = @_;

    my $blocking = delete $arguments{blocking} // 1;
    my $adb  = delete $arguments{adb} // $ADB;
    my $adb_socket = delete $arguments{adb_socket} // $ADB_SOCKET;
    my $host = delete $arguments{host} // $ADB_HOST;
    my $port = delete $arguments{port} // $ADB_PORT;
    my $reresolve = delete $arguments{reresolve};
    my $connect_timeout = delete $arguments{connect_timeout} //
        $CONNECT_TIMEOUT;
    my $transaction_timeout = delete $arguments{transaction_timeout} //
        $TRANSACTION_TIMEOUT;
    my $block_size = delete $arguments{block_size} || $BLOCK_SIZE;

    croak "Unknown argument " . join(", ", keys %arguments) if %arguments;

    my $client_ref = bless {
        client		=> $client,
        blocking	=> $blocking,
        host		=> $host,
        port		=> $port,
        reresolve	=> $reresolve,
        addr_info	=> undef,
        connect_timeout		=> $connect_timeout,
        transaction_timeout	=> $transaction_timeout,
        adb		=> $adb,
        adb_socket	=> $adb_socket,
        block_size	=> $block_size,
        socket		=> undef,
        expect_eof	=> undef,
        in		=> "",
        out		=> "",
        # Invariant: !@commands => !active (or active => @commands)
        active		=> undef,
        commands	=> [],
        children	=> {},
        result		=> [],
    }, $class;
    weaken($client_ref->{client});
    $client_ref->resolve();
    return $client_ref;
}

sub delete {
    my ($client_ref) = @_;

    $client_ref->close;
    my $children = $client_ref->{children};
    $client_ref->{children} = {};
    $_->delete(1) for values %$children;
    @{$client_ref->{result}} = ()
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
    info("DESTROY @_") if $DEBUG;
    shift->delete;
}

sub callback_default {
    die $_[1] if $_[1];
}

sub callback_blocking {
    my ($client_ref, $loop_level) = @_;

    $client_ref->{result}[$loop_level] = undef;
    return sub {
        my $client_ref = $ {shift()};
        $client_ref->{result}[$loop_level] = \@_;
        unloop($loop_level);
    };
}

sub wait : method {
    my ($client_ref, $loop_level) = @_;

    mainloop();
    my $result = delete $client_ref->{result}[$loop_level] //
        die "Assertion: Exit mainloop without setting result";
    croak $result->[0] if $result->[0];
    wantarray || return $result->[1];
    # remove the flag indicating success
    shift @$result;
    return @$result;
}

sub resolve {
    my ($client_ref) = @_;
    $client_ref->{addr_info} = addr_info($client_ref->{host}, $client_ref->{port});
}

sub server_start {
    my ($client_ref, $arguments, $callback) = @_;

    ADB::Client::ServerStart->new($client_ref, $callback, $arguments);
}

sub command_simple {
    my ($client_ref, $arguments, $index, $callback) = @_;

    croak "Unknown argument " . join(", ", keys %$arguments) if %$arguments;

    my $command = $SIMPLE_COMMANDS[$index] ||
        croak "No command at index '$index'";
    $client_ref->activate if 1 == push @{$client_ref->{commands}}, [$command, $callback];
}

sub close : method {
    my ($client_ref) = @_;

    if ($client_ref->{socket}) {
        $client_ref->{out} eq "" || $client_ref->{socket}->delete_write;
        $client_ref->{socket}->delete_read;
        $client_ref->{socket} = undef;
    }
    $client_ref->{expect_eof} = undef;
}

sub error {
    my $client_ref = shift;

    $client_ref->close();
    $client_ref->{active} = 0;
    my $command = shift @{$client_ref->{commands}} //
        die "Assertion: error without command";
    $command->[CALLBACK]->($client_ref->{client}, @_);
}

sub success {
    my ($client_ref, $str) = @_;

    my $command = shift @{$client_ref->{commands}};
    if (!$command) {
        $client_ref->close;
        $client_ref->{active} = 0;
        confess("Assertion: success without command");
    }
    $client_ref->{active} = 0 if !@{$client_ref->{commands}};
    if (@{$client_ref->{commands}}) {
        $client_ref->_next_command_start;
    } else {
        # No more work pending
        $client_ref->{active} = 0;
    }
    $command->[CALLBACK]->($client_ref->{client}, undef, $str);
}

sub first_command {
    my ($client_ref) = @_;

    my $command = $client_ref->{commands}[0];
    if (!$command) {
        $client_ref->close;
        $client_ref->{active} = 0;
        die "Assertion: _write_command without commands";
    }
    return $command->[COMMAND_REF];
}

sub _next_command_start {
    my ($client_ref) = @_;

    my $command_ref = $client_ref->first_command;
    if ($client_ref->{out} ne "") {
        my $response = display_string($client_ref->{out});
        $client_ref->error("Assertion: $response still pending when starting $command_ref->[COMMAND]");
        return;
    }
    $client_ref->{out} = sprintf("%04X", length $command_ref->[COMMAND]) . $command_ref->[COMMAND];
    $client_ref->{socket}->add_write(sub { $client_ref->_writer });
}

sub _connect_result {
    # info("CONNECT_RESULT");
    my $client_ref = $ {shift()};
    if ($_[0]) {
        # Error
        $client_ref->error(@_);
        return;
    }
    shift;
    $client_ref->{socket_info} = shift;
    $client_ref->{socket}      = shift;
    $client_ref->_next_command_start;
    $client_ref->{in} = "";
    $client_ref->{socket}->add_read(sub { $client_ref->_reader });
}

sub _writer {
    my ($client_ref) = @_;

    if (utf8::is_utf8($client_ref->{out})) {
        $client_ref->error("Assertion: client_ref out buffer is in utf8");
        return;
    }

    my $rc = syswrite($client_ref->{socket}, $client_ref->{out}, $BLOCK_SIZE);
    if ($rc) {
        substr($client_ref->{out}, 0, $rc, "");
        $client_ref->{socket}->delete_write if $client_ref->{out} eq "";
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
            if ($client_ref->{out} ne "") {
                # Any close while we are still writing is unexpected
                my $out = display_string($client_ref->{out});
                $client_ref->error("Unexpected EOF while still writing $out to adb socket");
                return;
            }
            my $command = $client_ref->{commands}[0];
            if ($client_ref->{expect_eof}) {
                if (!$command) {
                    # Logic for expect_eof should imply this is never reached
                    $client_ref->close();
                    $client_ref->{active} = 0;
                    die "Assertion: expect_eof without command";
                }
                if ($client_ref->{in} ne "") {
                    # Logic for expect_eof should imply this is never reached
                    my $spurious = display_string($client_ref->{in});
                    $client_ref->error("Assertion: Spurious response bytes $spurious");
                    return;
                }
                my $str = $client_ref->{expect_eof};
                $client_ref->close();
                eval { $client_ref->success($$str) };
                my $err = $@;
                shift @{$client_ref->{commands}};
                if (@{$client_ref->{commands}}) {
                    ADB::Client::ServerStart->new($client_ref, \&_connect_result);
                } else {
                    $client_ref->{active} = 0;
                }
                die $err if $@;
            } elsif ($command) {
                my $command_ref = $command->[COMMAND_REF];
                my ($error, $str) = adb_check_response($client_ref, 0, $command_ref->[NR_RESULTS], 0);
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
    $client_ref->{in} .= $buffer;
    if ($client_ref->{expect_eof}) {
        my $spurious = display_string($client_ref->{in});
        $client_ref->error("Spurious response bytes $spurious");
        return;
    }
    my $command_ref = $client_ref->first_command;
    if ($client_ref->{out} ne "") {
        my $out = display_string($client_ref->{out});
        $client_ref->error("Already reading response while command $out has not yet been completed");
        return;
    }
    my ($error, $str) = adb_check_response($client_ref, $rc, $command_ref->[NR_RESULTS], $command_ref->[EXPECT_EOF]) or return;
    return $client_ref->error($str) if $error;
    if ($client_ref->{in} ne "") {
        $str = display_string($client_ref->{in});
        $client_ref->error("Spurious response bytes: $str");
        return;
    }
    # Success
    if ($command_ref->[EXPECT_EOF]) {
        # Delay until actual EOF
        # Important for e.g. host:kill which is only finished at the EOF
        $client_ref->{expect_eof} = \$str;
        if ($client_ref->{in} ne "") {
            my $spurious = display_string($client_ref->{in});
            $client_ref->error("Spurious response bytes $spurious");
        }
        return;
    }
    $client_ref->success($str);
}

sub activate {
    my ($client_ref) = @_;

    croak "Already active" if $client_ref->{active};
    if ($client_ref->{socket}) {
        $client_ref->_next_command_start;
    } else {
        ADB::Client::ServerStart->new($client_ref, \&_connect_result);
    }
    $client_ref->{active} = 1;
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
