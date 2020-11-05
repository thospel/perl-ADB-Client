#!/usr/bin/perl -w
# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl 08_spawn.t'
#########################
## no critic (UselessNoCritic MagicNumbers)

use strict;
use warnings;

our $VERSION = "1.000";

use FindBin qw($Bin);
use lib $Bin;
use Test::More tests => 13;
use TestDrive qw(adb_start adb_unacceptable adb_unreachable dumper);

# We already checked loading in 02_adb_client.t
use ADB::Client qw(mainloop);

my $port = adb_start();

my $client = new_ok("ADB::Client" =>
                    [host => "127.0.0.1", port => $port, blocking => 1]);
is($client->version, 39, "Expected version");
$client->forget;
#eval { $client->spawn(version_min => 40, kill => 1) };
is($@, "", "No error");
is($client->version, 39, "Expected version");
