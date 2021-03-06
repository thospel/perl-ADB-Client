package ADB::Client::Timer;
# A small timer manager
use strict;
use warnings;

our $VERSION = '1.000';

use Scalar::Util qw(weaken refaddr);
use Carp;

our @CARP_NOT = qw(ADB::Client::Events);

# In general don't use ADB::Client::Timer (consider it an implementation detail)
# Do everyything through ADB::Client::Events
use Exporter::Tidy other => [qw($now timers_collect timers_run timer immediate)];

use ADB::Client::Utils qw(caller_info info callers clocktime $DEBUG);

my @timers = (undef);
# @expired must be persistent so no timers get lost if a callback dies
my @expired;
# Simular for @immediate. @immediate contains timeout 0 timers that we don't
# even bother to put in @timers
my @immediate;

# Timer indices
sub INDEX	() { 0 };
sub TIME	() { 1 };
sub OBJ		() { 2 };	# Must come after TIME
sub CODE	() { 3 };
sub CALLERS	() { 4 };

# Timers are kept in a simple binary heap @timers
sub timer {
    if ($DEBUG) {
        local $DEBUG;
        my $timer = timer(@_);
        my $callers = callers();
        $timer->[CALLERS] = $callers;
        info("add Timer(%s) %08x [%s]", $_[0], refaddr($timer), $callers);
        return $timer;
    }
    my $time = shift() + clocktime();
    my $i = @timers;
    while ($i > 1 && $time < $timers[$i >> 1][TIME]) {
        weaken($timers[$i] = $timers[$i >> 1]);
        $i = ($timers[$i][INDEX] = $i) >> 1;
    }
    my $timer = bless [$i, $time, @_];
    weaken($timer->[OBJ]);
    weaken($timers[$i] = $timer);
    return $timer;
}

sub immediate {
    # If we ever expose the TIME element we should put clocktime() there
    my $timer = bless [0, 0, @_];
    weaken($timer->[OBJ]);
    weaken($immediate[@immediate] = $timer);
    if ($DEBUG) {
        my $callers = callers();
        $timer->[CALLERS] = $callers;
        info("add Immediate Timer(0) %08x [%s]", refaddr($timer), $callers);
    }
    return $timer;
}

sub delete : method {
    my ($timer) = @_;

    my $i = $timer->[INDEX];
    if ($DEBUG && defined $i) {
        if ($timer->[CALLERS]) {
            my $callers = callers();
            info("delete Timer %08x [%s], added add %s", refaddr($timer), $callers, $timer->[CALLERS]);
        } else {
            caller_info("delete Timer %08x", refaddr($timer));
        }
    }
    if (!$i) {
        croak "Not a timer reference" unless defined($i) && $i == 0;
        # Could be a timer sitting on the expired queue in run_now
        $#$timer = TIME if @$timer > TIME;
        return;
    }
    $timer->[INDEX] = 0;
    # Last element or beyond...
    if ($i >= $#timers) {
        croak "Not a timer reference" if $i > $#timers;
        pop(@timers);
        return;
    }
    my $time = $timers[-1][TIME];
    if ($i > 1 && $time < $timers[$i >> 1][TIME]) {
        # percolate to root
        do {
            weaken($timers[$i] = $timers[$i >> 1]);
            $i = ($timers[$i][INDEX] = $i) >> 1;
        } while ($i > 1 && $time < $timers[$i >> 1][TIME]);
    } else {
        # percolate to leafs
        my $n = @timers-2;
        my $l = $i * 2;
        while ($l < $n) {
            if ($timers[$l][TIME] < $time) {
                if ($timers[$l+1][TIME] < $timers[$l][TIME]) {
                    weaken($timers[$i] = $timers[$l+1]);
                    $timers[$i][INDEX] = $i;
                    $i = $l+1;
                } else {
                    weaken($timers[$i] = $timers[$l]);
                    $timers[$i][INDEX] = $i;
                    $i = $l;
                }
            } elsif ($timers[$l+1][TIME] < $time) {
                weaken($timers[$i] = $timers[$l+1]);
                $timers[$i][INDEX] = $i;
                $i = $l+1;
            } else {
                last;
            }
            $l = $i * 2;
        }
        if ($l == $n && $timers[$l][TIME] < $time) {
            weaken($timers[$i] = $timers[$l]);
            $timers[$i][INDEX] = $i;
            $i = $l;
        }
    }
    weaken($timers[$i] = pop @timers);
    $timers[$i][INDEX] = $i;
}

sub DESTROY {
    shift->delete;
}

sub timers_collect {
    if (@immediate) {
        my $from = @expired;
        push @expired, @immediate;
        weaken($expired[$_]) for $from .. $#expired;
        @immediate = ();
    }
    return @expired ? 0 : undef if @timers <= 1;
    my $now = clocktime();
    return @expired ? 0 : $timers[1][TIME] - $now if $timers[1][TIME] > $now;

    # We will expire at least 1 timer
    # @timers > 2 makes sure that if we pop @timers we don't remove $timers[1]
    while (@timers > 2) {
        $timers[1][INDEX] = 0;
        weaken($expired[@expired] = $timers[1]);

        my $time = $timers[-1][TIME];
        my $n = @timers-2;
        my $i = 1;
        my $l = 2;
        while ($l < $n) {
            if ($timers[$l][TIME] < $time) {
                if ($timers[$l+1][TIME] < $timers[$l][TIME]) {
                    weaken($timers[$i] = $timers[$l+1]);
                    $timers[$i][INDEX] = $i;
                    $i = $l+1;
                } else {
                    weaken($timers[$i] = $timers[$l]);
                    $timers[$i][INDEX] = $i;
                    $i = $l;
                }
            } elsif ($timers[$l+1][0] < $time) {
                weaken($timers[$i] = $timers[$l+1]);
                $timers[$i][INDEX] = $i;
                $i = $l+1;
            } else {
                last;
            }
            $l = $i * 2;
        }
        if ($l == $n && $timers[$l][TIME] < $time) {
            weaken($timers[$i] = $timers[$l]);
            $timers[$i][INDEX] = $i;
            $i = $l;
        }
        weaken($timers[$i] = pop @timers);
        $timers[$i][INDEX] = $i;
        return 0 if $timers[1][TIME] > $now;
    }
    if (@timers == 2) {
        $timers[1][INDEX] = 0;
        weaken($expired[@expired] = pop @timers);
    }

    return 0;
}

sub timers_run {
    @expired || return;
    my $fun;

    # Using while instead of for in case a callback dies
    ($fun = shift @expired) && $fun->[OBJ] && $fun->[CODE]->($fun->[OBJ]) while @expired;
}

1;
