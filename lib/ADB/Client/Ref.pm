package ADB::Client::Ref;
use strict;
use warnings;

our $VERSION = '1.000';

use Carp;
use Scalar::Util qw(weaken refaddr);
# use IO::Socket::IP;
use Errno qw(EINPROGRESS EWOULDBLOCK EINTR EAGAIN ECONNRESET);

use ADB::Client::Events qw(mainloop unloop loop_levels);
use ADB::Client::ServerStart;
use ADB::Client::Utils qw(info caller_info dumper adb_check_response
                          display_string addr_info
                          FAILED BAD_ADB ASSERTION INFINITY
                          $DEBUG $VERBOSE);

use Exporter::Tidy
    other	=>[qw(mainloop unloop loop_levels
                      COMMAND_NAME COMMAND @SIMPLE_COMMANDS
                      $CALLBACK_DEFAULT
                      $ADB_HOST $ADB_PORT $ADB $ADB_SOCKET $DEBUG $VERBOSE)];

use constant {
    # Index in @SIMPLE_COMMAND element
    COMMAND_NAME	=> 0,
    COMMAND	=> 1,
    NR_RESULTS	=> 2,
    EXPECT_EOF	=> 3,
    PROCESS	=> 4,
    # Cannot reuse PROCESS for this since PROCESS will run at command retirement
    # (and we want that since it could be useful)
    CODE	=> 2,

    # Index in commands element
    COMMAND_REF	=> 0,
    CALLBACK	=> 1,
    ARGUMENTS	=> 2,

    SPECIAL	=> "",
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
    # command, number of result bytes, expect close
    # See the index in command array constants
    ["marker"		=> SPECIAL, \&_marker],
    ["connect"		=> SPECIAL, \&_connect],
    ["version"		=> "host:version", -1, 1,   \&process_version],
    ["kill"		=> "host:kill", 0, 1],
    ["features"		=> "host:features", -1, 1,  \&process_features],
    ["remount"		=> "remount:", INFINITY, 1,  \&process_remount],
    ["devices"		=> "host:devices", -1, 1,   \&process_devices],
    ["devices_long"	=> "host:devices-l", -1, 1, \&process_devices],
    ["transport"	=> "host:transport-%s", 0, 0],
    ["tport"		=> "host:tport:%s", 8, 0, \&process_tport],
    ["unroot"		=> "unroot:", INFINITY, 1],
    ["root"		=> "root:", INFINITY, 1],
);

# Notice that the client argument isn't yet blessed at this point
sub new {
    @_ % 2 == 0 || croak "Odd number of arguments";

    my ($class, $client, %arguments) = @_;

    my $model = delete $arguments{model};
    if (defined $model) {
        $model = $model->client_ref || croak "Model without client_ref";
        for my $name (qw(blocking adb adb_socket host port reresolve connect_timeout transaction_timeout block_size)) {
            $arguments{$name} //= $model->{$name};
        }
    }
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
        timeout		=> undef,
        adb		=> $adb,
        adb_socket	=> $adb_socket,
        block_size	=> $block_size,
        socket		=> undef,
        expect_eof	=> undef,
        in		=> "",
        out		=> "",
        # Invariant: !@commands => !active (or active => @commands)
        #            !active => !reading (or reading => active)
        #            active && socket => reading
        active		=> undef,
        commands	=> [],
        children	=> {},
        result		=> [],
    }, $class;
    weaken($client_ref->{client});
    $client_ref->resolve();
    return $client_ref;
}

sub client {
    return shift->{client};
}

sub delete {
    my ($client_ref) = @_;

    $client_ref->close;
    my $children = $client_ref->{children};
    $client_ref->{children} = {};
    $_->delete(1) for values %$children;
    @{$client_ref->{result}} = ();
    if (my $client = $client_ref->client) {
        $$client = undef;
    }
}

sub fatal {
    shift->delete;
    die shift;
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
    my $result = delete $client_ref->{result}[$loop_levels] //
        die "Assertion: Exit mainloop without setting result";
    # croak $result->[0] =~ s{(.*) at .* line \d+\.?\n}{$1}sar if $result->[0];
    $result->[0] =~ /\n\z/ ? die $result->[0] : croak $result->[0] if $result->[0];
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
    my ($client_ref, $arguments, $index, $callback, $args) = @_;

    croak "Unknown argument " . join(", ", keys %$arguments) if %$arguments;

    my $command = $SIMPLE_COMMANDS[$index] ||
        croak "No command at index '$index'";
    $client_ref->activate(1) if 1 == push @{$client_ref->{commands}}, [$command, $callback, $args];
}

sub close : method {
    my ($client_ref) = @_;

    if ($client_ref->{socket}) {
        if ($client_ref->{out} ne "") {
            $client_ref->{socket}->delete_write;
            $client_ref->{out} = "";
        }
        $client_ref->{socket}->delete_read if $client_ref->{active};
        $client_ref->{in} = "";
        $client_ref->{socket} = undef;
    }
    $client_ref->{active} = 0;
    $client_ref->{expect_eof} = undef;
    $client_ref->{timeout} = undef;
}

sub error {
    my $client_ref = shift;

    $client_ref->close();
    my $command = shift @{$client_ref->{commands}} //
        die "Assertion: error without command";
    $command->[CALLBACK]->($client_ref->{client}, @_);
}

# If used inside a calback (toplevel false) nothing after this call should
# change client_ref state, so typically this should be the last thing you do
sub success {
    my $client_ref = shift;

    my $result = \@_;
    my $command = shift @{$client_ref->{commands}};
    if (!$command) {
        $client_ref->close;
        confess("Assertion: success without command");
    }
    if ($client_ref->{active}) {
        $client_ref->{socket}->delete_read if $client_ref->{socket};
        $client_ref->{timeout} = undef;
        $client_ref->{active} = 0;
    }

    my $command_ref = $command->[COMMAND_REF];
    if ($command_ref->[PROCESS]) {
        # $_[0] as first arguments since the others will typically be ignored
        $result = eval { $command_ref->[PROCESS]->($_[0], $command_ref->[COMMAND], $client_ref, $result) };
        if ($@) {
            my $err = $@;
            my $str = display_string($_[0]);
            # Cannot call error since we already shifted commands
            $client_ref->close;
            $command->[CALLBACK]->($client_ref->{client}, "Assertion: Could not process $command_ref->[COMMAND] output $str: $err");
            return;
        }
        if (ref $result ne "ARRAY") {
            # Cannot call error since we already shifted commands
            $client_ref->close;
            if (ref $result eq "") {
                $command->[CALLBACK]->($client_ref->{client}, $result);
            } else {
                $command->[CALLBACK]->($client_ref->{client}, "Assertion: Could not process $command_ref->[COMMAND] output: Neither a string nor an ARRAY reference");
            }
            return;
        }
        @_ = @$result;
    }
    $command->[CALLBACK]->($client_ref->{client}, undef, @$result);
    $client_ref->activate;
}

sub first_command {
    my ($client_ref) = @_;

    my $command = $client_ref->{commands}[0];
    if (!$command) {
        $client_ref->close;
        die "Assertion: _write_command without commands";
    }
    return $command->[COMMAND_REF];
}

# If used inside a calback (toplevel false) nothing after this call should
# change client_ref state, so typically this should be the last thing you do
sub activate {
    my ($client_ref, $top_level) = @_;

    return if $client_ref->{active} || !@{$client_ref->{commands}};

    for (1) {
        my $command_ref = $client_ref->first_command;
        if ($client_ref->{out} ne "") {
            my $response = display_string($client_ref->{out});
            my $msg = "Assertion: $response to ADB still pending when starting $command_ref->[COMMAND]";
            die $msg if $top_level;
            $client_ref->error($msg);
            return;
        }
        if ($client_ref->{in} ne "") {
            my $response = display_string($client_ref->{in});
            my $msg = "Assertion: $response from ADB still pending when starting $command_ref->[COMMAND]";
            die $msg if $top_level;
            $client_ref->error($msg);
            return;
        }
        if ($command_ref->[COMMAND] eq SPECIAL) {
            # Special
            my $code = $command_ref->[CODE] ||
                $client_ref->fatal("Assertion: No CODE in special command '$command_ref->[COMMAND_NAME]'");
            # Return true for the normal case: we activated something
            # but possibly did not set the active flag (which gets set below)
            # Return false if you twiddled things yourself
            # e.g. you called success which calls activate and you do now want
            # to change whatever value {active} got into
            last if $code->($client_ref, $command_ref, $top_level);
            return;
        }
        if (!$client_ref->{socket}) {
            if (0) {
                if ($client_ref->{reresolve}) {
                    my $addr_info = addr_info($client_ref->{host}, $client_ref->{port});
                    if (ref $addr_info ne "ARRAY") {
                        # Report it on the pending command
                        $client_ref->error($addr_info);
                        return;
                    }
                    $client_ref->{addr_info} = $addr_info;
                }
                $client_ref->error("Wee");
            }
            ADB::Client::ServerStart->new($client_ref, \&_connect_result);
            last;
        }
        my $command = sprintf($command_ref->[COMMAND],
                              @{$client_ref->{commands}[0][ARGUMENTS]});
        if (length $command >= 2**16) {
            my $msg = sprintf("Assertion: Command %s is too long", display_string($command));
            die $msg if $top_level;
            $client_ref->error($msg);
            return;
        }
        info("Sending to ADB: %s", display_string($command)) if $DEBUG;
        $client_ref->{out} = sprintf("%04X", length $command) . $command;
        $client_ref->{socket}->add_write(sub { $client_ref->_writer });
        $client_ref->{socket}->add_read(sub { $client_ref->_reader });
        $client_ref->{timeout} = ADB::Client::Timer->new($client_ref->{transaction_timeout}, sub { $client_ref->_timed_out });
    }
    $client_ref->{active} = 1;
}

sub _marker {
    my ($client_ref, $command_ref, $top_level) = @_;

    info("Sending (not) MARKER") if $DEBUG;
    if (!$top_level) {
        # This can potentially lead to endless recursion through
        # activate -> success -> activate -> success ...
        # if the callback just keeps on pushing markers
        $client_ref->success($client_ref, undef);
        return 0;
    }
    $client_ref->{timeout} = ADB::Client::Timer->new(0, sub { $client_ref->success(undef) });
    # We don't expect any read here, but it's needed to maintain our
    # invariant active & socket => reader (otherwise any ->close will fail)
    $client_ref->{socket}->add_read(sub { $client_ref->_reader }) if $client_ref->{socket};
    return 1;
}

sub _timed_out {
    my ($client_ref) = @_;

    $client_ref->error("Operation timed out");
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
    $client_ref->{in} = "";
    if ($client_ref->{active}) {
        $client_ref->{active} = 0;
        $client_ref->activate;
    }
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
                $client_ref->success($$str);
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
    while ($devices =~ s{^(\S+)\s+(device| no device|offline)(?: (\S.*\S))?\n}{}) {
        my ($serial, $state, $description) = ($1, $2, $3);
        die "Multiple devices with serial number $serial" if $devices{$serial};
        push @devices, $serial;
        if ($long) {
            my %description = (state => $state);
            my @description = split(" ", $description);
            for my $description (@description) {
                my ($key, $value) = $description =~ m{^([^:]+):(.*)} or last DEVICE;
                last if exists $description{$key};
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

    # Just in case
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
