package ADB::Client::Events;
# A small event core
use strict;
use warnings;

our $VERSION = '1.000';

use Carp;
use Errno qw(EINTR);
use Scalar::Util qw(weaken isweak);

use ADB::Client::Timer qw(timers_collect timers_run);
use ADB::Client::Utils qw(dumper info caller_info $DEBUG $VERBOSE);

our $EVENT_INITER = \&event_init;
our $AIO_INITER = \&aio_init;
# our ($DEBUG, $VERBOSE);

my ($timer, $immediate);
BEGIN {
    $timer = sub {
        $EVENT_INITER->() if $EVENT_INITER;
        my $package = caller();
        if ($timer == ($package->can("timer") || 0)) {
            no strict "refs";
            no warnings "redefine";
            *{$package . "::timer"} = \&ADB::Client::Events::timer;
        }
        goto \&ADB::Client::Events::timer;
    };
    $immediate = sub {
        $EVENT_INITER->() if $EVENT_INITER;
        my $package = caller();
        if ($immediate == ($package->can("immediate") || 0)) {
            no strict "refs";
            no warnings "redefine";
            *{$package . "::immediate"} = \&ADB::Client::Events::immediate;
        }
        goto \&ADB::Client::Events::immediate;
    };
}

use Exporter::Tidy
    other => [qw(mainloop unloop loop_levels event_init aio_init
                 $IGNORE_PIPE_LOCAL $EVENT_INITER $AIO_INITER)],
    _map => {
        # Can't just export the placeholders since the replace will not
        # impact the imported symbol
        timer => $timer,
        immediate => $immediate,
    };

# We want errors reported at the call site since they are bugs
# our @CARP_NOT = qw(ADB::Client::Ref);

our $IGNORE_PIPE_LOCAL = 0;

my $read_fixed = 0;
my $read_mask  = "";
my $write_mask = "";
my $error_mask = "";
my (%read_refs, %write_refs, %error_refs, @unlooping);

my @NOP = (bless([]), sub {});
sub ADB::Client::Events::Read::DESTROY {
    my $fd = ${shift()} // die "No filedescriptor";
    caller_info("delete_read $fd") if $DEBUG;
    # This strange assign after delete is to update the reference the for in
    # sub mainloop may still have
    @{delete $read_refs{$fd}} = @NOP;
    if (%read_refs) {
        vec($read_mask, $fd, 1) = 0;
        $read_mask =~ s/\x00+\z//;
    } else {
        $read_mask = "";
    }
}

sub ADB::Client::Events::Write::DESTROY {
    my $fd = ${shift()} // die "No filedescriptor";
    caller_info("delete_write $fd") if $DEBUG;
    # This strange assign after delete is to update the reference the for in
    # sub mainloop may still have
    @{delete $write_refs{$fd}} = @NOP;
    if (%write_refs) {
        vec($write_mask, $fd, 1) = 0;
        $write_mask =~ s/\x00+\z//;
    } else {
        $write_mask = "";
    }
}

sub ADB::Client::Events::Error::DESTROY {
    my $fd = ${shift()} // die "No filedescriptor";
    caller_info("delete_error $fd") if $DEBUG;
    # This strange assign after delete is to update the reference the for in
    # sub mainloop may still have
    @{delete $error_refs{$fd}} = @NOP;
    if (%error_refs) {
        vec($error_mask, $fd, 1) = 0;
        $error_mask =~ s/\x00+\z//;
    } else {
        $error_mask = "";
    }
}

package ADB::Client::Events;
sub add_read {
    my $fd = fileno(shift) // croak "Not a filehandle";
    caller_info("add_read $fd") if $DEBUG;
    croak "Descriptor $fd already selected for read" if $read_refs{$fd};
    vec($read_mask, $fd, 1) = 1;
    weaken(($read_refs{$fd} = [shift, shift])->[0]);
    return bless \$fd, "ADB::Client::Events::Read";
}

sub add_write {
    my $fd = fileno(shift) // croak "Not a filehandle";
    caller_info("add_write $fd") if $DEBUG;
    croak "Descriptor $fd already selected for write" if $write_refs{$fd};
    vec($write_mask, $fd, 1) = 1;
    weaken(($write_refs{$fd} = [shift, shift])->[0]);
    return bless \$fd, "ADB::Client::Events::Write";
}

sub add_error {
    my $fd = fileno(shift) // croak "Not a filehandle";
    caller_info("add_error $fd") if $DEBUG;
    croak "Descriptor $fd already selected for error" if $error_refs{$fd};
    vec($error_mask, $fd, 1) = 1;
    weaken(($error_refs{$fd} = [shift, shift])->[0]);
    return bless \$fd, "ADB::Client::Events::Error";
}

sub unloop {
    my $loop_levels = shift // -1;
    info("Unloop (level $loop_levels)") if $DEBUG;
    $unlooping[$loop_levels] = shift || 1;
}

sub loop_levels {
    return scalar @unlooping;
}

sub mainloop {
    local $@;
    $EVENT_INITER->() if $EVENT_INITER;
    my $level = push(@unlooping, undef)-1;
    eval {
        info("Entering mainloop (level $level)") if $VERBOSE || $DEBUG;
        local $SIG{PIPE} = "IGNORE" if $IGNORE_PIPE_LOCAL;
        my ($r, $w, $e, $name, $timeout);
        until ($unlooping[-1]) {
            ($timeout = timers_collect()) //
                (keys %read_refs > $read_fixed || %write_refs || %error_refs || !$AIO_INITER && IO::AIO::nreqs() || last);
            if ((select($r = $read_mask,
                        $w = $write_mask,
                        $e = $error_mask, $timeout) || next) > 0) {
                # The copy to @tmp is because the stack doesn't keep values
                # alive, so any deletes on xxx_refs during the loop can make
                # the value go poof. The copy temporarily increases the
                # refcount so the value doesn't go away. That is also why the
                # delete_xxx functions modify the value before delete
                # $name = $_->[1], $_->[0]->$name for my @tmp=(
                $_->[1]->($_->[0]) for my @tmp=(
                # $$_ and $$_->[0]->(@{$$_->[1]}[1..$#{$$_->[1]}]) for
                    @read_refs{ grep vec($r, $_, 1), keys %read_refs},
                    @write_refs{grep vec($w, $_, 1), keys %write_refs},
                    @error_refs{grep vec($e, $_, 1), keys %error_refs});
            } elsif ($! == EINTR) {
                redo;
            } else {
                die "Select failed: $^E";
            }
        } continue {
            timers_run();
        }
        info("Exiting mainloop (level $level)") if $VERBOSE || $DEBUG;
    };
    my $tmp = pop @unlooping;
    die $@ if $@;
    return $tmp;
}

sub event_init {
    $EVENT_INITER || return;

    $SIG{PIPE} = "IGNORE" if defined $IGNORE_PIPE_LOCAL && !$IGNORE_PIPE_LOCAL;

    no warnings "once";
    *IO::Handle::add_read     = \&add_read;
    *IO::Handle::add_write    = \&add_write;
    *IO::Handle::add_error    = \&add_error;
    *timer     = \&ADB::Client::Timer::timer;
    *immediate = \&ADB::Client::Timer::immediate;

    $EVENT_INITER = undef;
}

sub aio_init {
    $AIO_INITER || return;
    require IO::AIO;

    $EVENT_INITER->() if $EVENT_INITER;
    my $fd = IO::AIO::poll_fileno();
    $read_refs{$fd} = \&IO::AIO::poll_cb;
    vec($read_mask, $fd, 1) = 1;
    ++$read_fixed;

    $AIO_INITER = undef;
}

1;
