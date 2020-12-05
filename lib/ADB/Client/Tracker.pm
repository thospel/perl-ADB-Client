package ADB::Client::Tracker;
use strict;
use warnings;

our $VERSION = '1.000';

use Scalar::Util qw(weaken);
use Errno qw(ECONNRESET EAGAIN EINTR EWOULDBLOCK);
use Storable qw(dclone);
use Carp;
our @CARP_NOT = qw(ADB::Client);

use ADB::Client::Events qw(mainloop loop_levels unloop immediate);
use ADB::Client::Utils qw(display_string info $DEBUG $QUIET);
use ADB::Client::Command qw(PROCESS COMMAND);

our $BLOCK_SIZE = int(2**16);

my $objects = 0;

sub new {
    my ($class, $socket, $command_ref, $block_size, $current, $in) = @_;
    ++$objects;
    return bless {
        socket		=> $socket,
        in		=> $in,
        reader		=> undef,
        result		=> undef,
        current		=> {%$current},
        command_ref	=> $command_ref,
        callback	=> undef,
        timeout		=> undef,
        block_size	=> $block_size || $BLOCK_SIZE,
    }, $class;
}

sub track {
    my $tracker = shift;
    my $callback = shift || croak "No callback";

    croak "Already tracking" if $tracker->{callback};
    croak "Socket closed" if !$tracker->{socket};
    weaken($tracker);
    if ($tracker->{in} eq "") {
        $tracker->{reader} = $tracker->{socket}->add_read($tracker, \&_reader);
    } else {
        $tracker->{timeout} = immediate($tracker, \&_process);
    }
    $tracker->{callback} = $callback;
}

sub untrack {
    my ($tracker) = @_;

    croak "Not tracking" if !$tracker->{callback};
    $tracker->{callback} = undef;

    if ($tracker->{timeout}) {
        $tracker->{timeout} = undef;
    } else {
        $tracker->{reader} = undef;
    }
}

sub is_tracking {
    return !!shift->{callback};
}

sub error {
    my $tracker = shift;
    my $err = shift || "Unknown error";

    my $callback = $tracker->{callback};
    $tracker->untrack;
    $tracker->{socket} = undef;
    $callback->($tracker, $err, @_);
    return;
}

sub _process {
    my ($tracker) = @_;

    if ($tracker->{timeout}) {
        $tracker->{timeout} = undef;
        $tracker->{reader} = $tracker->{socket}->add_read($tracker, \&_reader);
    }
    while (length $tracker->{in} >= 4) {
        my $len = substr($tracker->{in}, 0, 4);
        if ($len !~ /^[0-9a-fA-F]{4}\z/) {
            $len = display_string($len);
            $tracker->error("Bad ADB hex length $len");
            return;
        }
        $len = hex $len;
        last if length $tracker->{in} < $len + 4;
        my $result = substr($tracker->{in}, 0, $len +4, "");
        substr($result, 0, 4, "");
        $result = [$result];
        my $command_ref = $tracker->{command_ref};
        if (my $process = $command_ref->[PROCESS]) {
            # $result->[0] as first arguments since the others will typically
            # be ignored
            $result = eval { $command_ref->[PROCESS]->($result->[0], $command_ref->[COMMAND], $tracker, $result) };
            if ($@) {
                my $err = $@;
                $err =~ s/\s+\z//;
                my $str = display_string($_[0]);
                $tracker->error("Assertion: Could not process $command_ref->[COMMAND] output $str: $err");
                return;
            }
            if (ref $result ne "ARRAY") {
                $tracker->error(ref $result eq "" && $result);
                return;
            }
            my %change;
            my $new_devices = dclone($result->[0]);
            while (my ($serial, $new_state) = each %$new_devices) {
                if (my $old_state = delete $tracker->{current}{$serial}) {
                    $change{$serial} = [$old_state, $new_state] if $old_state ne $new_state;
                } else {
                    $change{$serial} = ["", $new_state];
                }
            }
            while (my ($serial, $old_state) = each %{$tracker->{current}}) {
                $change{$serial} = [$old_state, ""];
            }
            $tracker->{current} = $new_devices;
            push @$result, \%change;
        }
        eval { $tracker->{callback}->($tracker, undef, @$result) };
        if ($@) {
            my $err = $@;
            $tracker->untrack;
            $tracker->{socket} = undef;
            die $err;
        }
        $tracker->is_tracking || return
    }
}

sub _reader {
    my ($tracker, $probe) = @_;

    my $rc = sysread($tracker->{socket}, my $buffer, $tracker->{block_size});
    if ($rc) {
        $tracker->{in} .= $buffer;
        $tracker->_process unless $probe;
        return $rc;
    }
    if (defined $rc || $! == ECONNRESET) {
        # EOF
        my $eof = defined $rc ? 1 : 2;
        if ($tracker->{in} ne "") {
            my $in = display_string($tracker->{in});
            $tracker->error("EOF with unfinished input: $in", -$eof);
        } else {
            $tracker->error("EOF", $eof);
        }
        return 0;
    }
    return undef if $! == EAGAIN || $! == EINTR || $! == EWOULDBLOCK || $probe;
    $tracker->error("Unexpected error reading from socket: $^E");
}

sub is_empty {
    my ($tracker, $probe) = @_;

    if ($probe) {
        return 0 if $tracker->{in} ne "";
        $tracker->_reader(1);
    }
    return $tracker->{in} eq "" ? 1 : 0;
}

sub wait : method {
    my ($tracker) = @_;

    croak "Assertion: Already waiting" if defined $tracker->{result};
    my $loop_levels = loop_levels();
    $tracker->track(sub {
        my $tracker = shift;

        defined $tracker->{result} || die "Assertion: Not waiting";
        confess "Assertion: Result already set" if $tracker->{result};
        $tracker->{result} = \@_;
        unloop($loop_levels);
        $tracker->untrack;
    });
    $tracker->{result} = "";

    eval { mainloop() };
    if ($@) {
        my $err = $@;
        $tracker->{result} = undef;
        die $err;
    }

    my $result = $tracker->{result};
    $tracker->{result} = undef;
    $result || confess "Assertion: Exit mainloop without setting result";
    # croak $result->[0] =~ s{(.*) at .* line \d+\.?\n}{$1}sar if $result->[0];
    $result->[0] =~ /\n\z/ ? die $result->[0] : croak $result->[0] if $result->[0];
    wantarray || return $result->[1];
    # remove the flag indicating success
    shift @$result;
    return @$result;
}

sub DESTROY {
    my ($tracker) = @_;

    --$objects;
    info("DESTROY @_") if $DEBUG;
    $tracker->untrack if $tracker->{callback};
    $tracker->{socket} = undef;
}

sub objects {
    return $objects;
}

END {
    # $QUIET first for easier code coverage
    info("Still have %d %s objects at program end", $objects, __PACKAGE__) if !$QUIET && $objects;
}

1;
