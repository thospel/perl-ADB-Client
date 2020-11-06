#!/usr/bin/perl -w
# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl 04_kill.t'
#########################
## no critic (UselessNoCritic MagicNumbers)

use strict;
use warnings;

our $VERSION = "1.000";

use FindBin qw($Bin);
use lib $Bin;
use Test::More tests => 15;
use TestDrive qw(adb_start adb_stop);

# We already checked loading in 02_adb_client.t
use ADB::Client qw(mainloop timer);

$ADB::Client::Events::IGNORE_PIPE_LOCAL = 1;

$SIG{__DIE__} = sub {
    BAIL_OUT("Unexpected exception: @_");
};

timer(0, sub {});
# To improve coverage

my $port = adb_start();
# $port = 5037;

my $client = new_ok("ADB::Client" => [host => "127.0.0.1", port => $port]);
my $result = $client->kill;
is($result, "", "Kill returns empty string");

$client = new_ok("ADB::Client" => [host => "127.0.0.1", port => $port]);
my $dummy = eval {
    local $SIG{__DIE__} = undef;
    $client->kill;
};
my $err = $@;
like($err, qr{^ADB server 127\.0\.0\.1 port $port: Connect error: },
     "Must have a connection error") ||
    BAIL_OUT("Stopped fake adb server does not lead to the proper error message");

# This implies fake adb server must have stopped even without closing STDIN
adb_stop(1) ||
    BAIL_OUT("Unexpected final status from fake adb server");
