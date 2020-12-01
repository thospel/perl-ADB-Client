#!/usr/bin/perl -w
# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl 81_event_speed.t'
#########################
## no critic (UselessNoCritic MagicNumbers)
# Test some simple commands

use strict;
use warnings;

our $VERSION = "1.000";

use FindBin qw($Bin);
use lib $Bin;

use Errno qw(EAGAIN EINTR EWOULDBLOCK);
use Test::More tests => 1;

use ADB::Client::Events qw(timer event_init mainloop);
use ADB::Client::Utils qw(clocktime $DEBUG);

$DEBUG = 0;
event_init();

pipe(my $rd, my $wr) || die "Could not create pipe: $^E";
$rd->blocking(0);
$wr->blocking(0);

my ($rc, $buffer, $count);

my $ref = bless [];
my ($reader, $writer);

sub reader {
    if ($rc = sysread($rd, $buffer, 2**16)) {
        $writer = $wr->add_write($ref, \&writer);
        ++$count;
        return;
    }
    die "Unexpected EOF" if defined $rc;
    return if $! == EAGAIN || $! == EINTR || $! == EWOULDBLOCK;
    die "Unexpected read error: $^E";
}

sub writer {
    if ($rc = syswrite($wr, $buffer, 2**16)) {
        $writer = undef;
        ++$count;
        return;
    }
    die "Length 0 write" if defined $rc;
    return if $! == EAGAIN || $! == EINTR || $! == EWOULDBLOCK;
    die "Unexpected write error: $^E";
}

sub stop_loop {
    $writer = $reader = undef;
}

$count = 0;
$buffer = "a";
$reader = $rd->add_read ($ref, \&reader);
$writer = $wr->add_write($ref, \&writer);
my $timeout = timer(1, $ref, \&stop_loop);

my $period = clocktime();
mainloop();
$period = clocktime() - $period;
ok($count, "Did loops");
diag(sprintf "%.0f loops per second", $count/2/$period);
