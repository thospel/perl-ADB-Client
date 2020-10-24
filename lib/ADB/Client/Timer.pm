package ADB::Client::Timer;
# A small timer manager
use strict;
use warnings;
use Scalar::Util qw(weaken);
use Carp;
use Time::HiRes qw(clock_gettime CLOCK_REALTIME CLOCK_MONOTONIC );

my @timers = (undef);
# @immediate must be persistent so no timers get lost if a callback dies
my (@immediate, $now);

# Timer indices
sub TIME	() { 0 };
sub INDEX	() { 1 };
sub CODE	() { 2 };	# Must come after INDEX

our $CLOCK_TYPE;
our $CLOCK_TYPE_NAME =
    eval { $CLOCK_TYPE = CLOCK_MONOTONIC; "MONOTONIC" } ||
    eval { $CLOCK_TYPE = CLOCK_REALTIME;  "REAL" } ||
    die "Time::HiRes doesn't even have CLOCK_REALTIME";

sub realtime {
    return clock_gettime(CLOCK_REALTIME);
}

sub clocktime {
    return clock_gettime($CLOCK_TYPE);
}

our $BASE_REALTIME  = realtime();
our $BASE_CLOCKTIME = clocktime();

# Timers are kept in a simple binary heap @timers
sub new {
    my ($class, $time, $fun) = @_;

    $now = clock_gettime($CLOCK_TYPE);
    $time = $time + $now;
    $time = 0.5+1/3-$time if $time < 1;
    my $i = @timers;
    while ($i > 1 && $time < $timers[$i >> 1][TIME]) {
        weaken($timers[$i] = $timers[$i >> 1]);
        $i = ($timers[$i][INDEX] = $i) >> 1;
    }
    my $timer = bless [$time, $i, $fun], $class;
    weaken($timers[$i] = $timer);
    return $timer;
}

sub delete : method {
    my ($timer) = @_;

    my $i = $timer->[INDEX];
    if (!$i) {
        croak "Not a timer reference" unless defined($i) && $i == 0;
        # Could be a timer sitting on the expired queue in run_now
        $#$timer = INDEX if @$timer > INDEX;
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

sub run_now {
    $now = clock_gettime($CLOCK_TYPE);
    goto EXPIRED if @timers <= 1 || $timers[1][TIME] > $now;
    # @timers > 2 makes sure that if we pop @timers we don't remove $timers[1]
    while (@timers > 2) {
        $timers[1][INDEX] = 0;
        weaken($immediate[@immediate] = $timers[1]);

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
        goto EXPIRED if $timers[1][TIME] > $now;
    }
    if (@timers == 2) {
        $timers[1][INDEX] = 0;
        weaken($immediate[@immediate] = pop @timers);
    }
  EXPIRED:
    if (@immediate) {
        my $fun;
        ($fun = shift @immediate)->[CODE] && $fun->[CODE]->() while @immediate;
        $now = clock_gettime($CLOCK_TYPE);
    }
    return
        @timers <= 1 ? undef :
        $timers[1][0]-$now > 0 ? $timers[1][0]-$now :
        0;
}

1;
