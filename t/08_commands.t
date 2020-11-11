#!/usr/bin/perl -w
# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl 08_commands.t'
#########################
## no critic (UselessNoCritic MagicNumbers)

# Test some simple commands

use strict;
use warnings;

our $VERSION = "1.000";

use FindBin qw($Bin);
use lib $Bin;
use Test::More tests => 22;
use TestDrive qw(adb_start dumper);

# We already checked loading in 02_adb_client.t
use ADB::Client qw(mainloop);

my $port = adb_start();
# $port = 5037;

my ($client, @result);

$client = new_ok("ADB::Client" => [host => "127.0.0.1", port => $port]);

# Test special characters and utf8
my $str = "Abcde";
is($client->echo($str), $str, "Special characters and utf8");
$str = " abc\nd\r\tf zg\x{123}z\0z ";
is($client->echo($str), $str, "Special characters and utf8");

is($client->remount, qq(Not running as root. Try "adb root" first.\n),
   "Cannot remount as non-root");
is($client->root,   "restarting adbd as root\n", "Can set root");
is($client->root,   "adbd is already running as root\n", "Can set root");
is($client->unroot, "restarting adbd as non root\n", "Can set root");
is($client->unroot, "adbd not running as root\n", "Can set root");
is($client->root,   "restarting adbd as root\n", "Can set root");
is($client->remount, qq(remount succeeded\n),
   "Can remount as root");
