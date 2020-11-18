package ADB::Client::Events;
# A small event core
use strict;
use warnings;
use Carp;
use Errno qw(EINTR);

use ADB::Client::Timer qw(timers_collect timers_run);
use ADB::Client::Utils qw(info caller_info $DEBUG $VERBOSE);

our $EVENT_INITER = \&event_init;

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
    other => [qw(mainloop unloop loop_levels event_init
                 $IGNORE_PIPE_LOCAL $EVENT_INITER)],
    _map => {
        # Can't just export the placeholders since the replace will not
        # impact the imported symbol
        timer => $timer,
        immediate => $immediate,
    };

# We want errors reported at the call site since they are bugs
# our @CARP_NOT = qw(ADB::Client::Ref);

our $IGNORE_PIPE_LOCAL = 0;

my $read_mask  = "";
my $write_mask = "";
my $error_mask = "";
my (%read_refs, %write_refs, %error_refs, @unlooping);

package ADB::Client;
our ($DEBUG, $VERBOSE);

package ADB::Client::Events;
sub add_read(*$ ) {
    my $fd = fileno(shift) // croak "Not a filehandle";
    caller_info("add_read $fd") if $DEBUG;
    croak "Descriptor $fd already selected for read" if $read_refs{$fd};
    $read_refs{$fd} = shift;
    vec($read_mask, $fd, 1) = 1;
}

sub add_write(*$ ) {
    my $fd = fileno(shift) // croak "Not a filehandle";
    caller_info("add_write $fd") if $DEBUG;
    croak "Descriptor $fd already selected for write" if $write_refs{$fd};
    $write_refs{$fd} = shift;
    vec($write_mask, $fd, 1) = 1;
}

sub add_error(*$ ) {
    my $fd = fileno(shift) // croak "Not a filehandle";
    caller_info("add_error $fd") if $DEBUG;
    croak "Descriptor $fd already selected for error" if $error_refs{$fd};
    $error_refs{$fd} = shift;
    vec($error_mask, $fd, 1) = 1;
}

sub delete_read(*) {
    my $fd = fileno(shift) // croak "Not a filehandle";
    caller_info("delete_read $fd") if $DEBUG;
    croak "Descriptor $fd wasn't selected for read" unless $read_refs{$fd};
    # This strange assign before delete is to poison the reference the for in
    # sub mainloop may still have
    $read_refs{$fd} = undef;
    delete $read_refs{$fd};
    if (%read_refs) {
        vec($read_mask, $fd, 1) = 0;
        $read_mask =~ s/\x00+\z//;
    } else {
        $read_mask = "";
    }
}

sub delete_write(*) {
    my $fd = fileno(shift) // croak "Not a filehandle";
    caller_info("delete_write $fd") if $DEBUG;
    croak "Descriptor $fd wasn't selected for write" unless $write_refs{$fd};
    # This strange assign before delete is to poison the reference the for in
    # sub mainloop may still have
    $write_refs{$fd} = undef;
    delete $write_refs{$fd};
    if (%write_refs) {
        vec($write_mask, $fd, 1) = 0;
        $write_mask =~ s/\x00+\z//;
    } else {
        $write_mask = "";
    }
}

sub delete_error(*) {
    my $fd = fileno(shift) // croak "Not a filehandle";
    caller_info("delete_error $fd") if $DEBUG;
    croak "Descriptor $fd wasn't selected for error" unless $error_refs{$fd};
    # This strange assign before delete is to poison the reference the for in
    # sub mainloop may still have
    $error_refs{$fd} = undef;
    delete $error_refs{$fd};
    if (%error_refs) {
        vec($error_mask, $fd, 1) = 0;
        $error_mask =~ s/\x00+\z//;
    } else {
        $error_mask = "";
    }
}

sub unloop {
    my $loop_levels = shift // -1;
    $unlooping[$loop_levels] = shift || 1;
}

sub loop_levels {
    return scalar @unlooping;
}

sub mainloop {
    $EVENT_INITER->() if $EVENT_INITER;
    my $level = push(@unlooping, undef)-1;
    eval {
        info("Entering mainloop (level $level)") if $VERBOSE || $DEBUG;
        local $SIG{PIPE} = "IGNORE" if $IGNORE_PIPE_LOCAL;
        my ($r, $w, $e);
        until ($unlooping[-1]) {
            my $timeout = timers_collect();
            $timeout // (%read_refs || %write_refs || %error_refs || last);
            if ((select($r = $read_mask,
                        $w = $write_mask,
                        $e = $error_mask, $timeout) || (timers_run(), next)) > 0) {
                $$_ && $$_->() for
                    \@read_refs{ grep vec($r, $_, 1), keys %read_refs},
                    \@write_refs{grep vec($w, $_, 1), keys %write_refs},
                    \@error_refs{grep vec($e, $_, 1), keys %error_refs};
                timers_run();
            } elsif ($! != EINTR) {
                die "Select failed: $^E";
            }
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
    *IO::Handle::delete_read  = \&delete_read;
    *IO::Handle::delete_write = \&delete_write;
    *IO::Handle::delete_error = \&delete_error;
    *timer     = \&ADB::Client::Timer::timer;
    *immediate = \&ADB::Client::Timer::immediate;

    $EVENT_INITER = undef;
}

1;
