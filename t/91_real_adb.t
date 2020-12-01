#!/usr/bin/perl -w
# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl 91_real_adb.t'
#########################
## no critic (UselessNoCritic MagicNumbers)

use strict;
use warnings;

our $VERSION = "1.000";

use FindBin qw($Bin);
use lib $Bin;
use IO::Socket::IP qw();

use Test::More;
my ($adb, $host, $port);
BEGIN {
    $adb = $ENV{ADB_CLIENT_TEST_REAL};
    if (!$adb) {
        plan skip_all => "By default no tests are done using the real ADB client. If you want to run this please set the environment variable ADB_CLIENT_TEST_REAL to the name of your adb binary (probably 'adb') before doing 'make test'. Notice that this will start the real ADB server and leave it running even after the test finishes";
        exit;
    }
    delete $ENV{ANDROID_ADB_LOG_PATH};
    # Intentionally leave ANDROID_ADB_SERVER_ADDRESS, ANDROID_ADB_SERVER_PORT
    $host = $ENV{ANDROID_ADB_SERVER_ADDRESS} // "127.0.0.1";
    $port = $ENV{ANDROID_ADB_SERVER_PORT} // 5037;

    {
        # If ANDROID_ADB_SERVER_ADDRESS isn't localhost or unset this will
        # probably fail
        no warnings "exec";
        open(my $fh, "-|", $adb, "start-server") || do {
            plan skip_all => "Cannot start '$adb': $^E";
            exit;
        };
        my $out = do { local $/; <$fh> };
        diag($out) if $out ne "";
        close($fh);
        if ($?) {
            plan skip_all => "Unexpected exit code $? from '$adb start-server'";
            return;
        }
    }
    my $socket = IO::Socket::IP->new(
        PeerHost => $host,
        PeerPort => $port);
    if (!$socket) {
        plan skip_all => "Cannot connect to 127.0.0.1:5037 even after '$adb start-server': $@";
        exit;
    }
    plan tests => 19;
}

use TestDrive qw(adb_start adb_run dumper);
use ADB::Client qw($ADB $ADB_HOST $ADB_PORT);

# Use the real server
$ADB = $adb;

# Connect to the real server
my $client = new_ok("ADB::Client" => [host => $host, port => $port]);
my $version = $client->version;
ok($version, "adb version $version");
diag("Your ADB server has version $version");

$port = adb_start($version);
$ADB_HOST = $ENV{ANDROID_ADB_SERVER_ADDRESS} = "127.0.0.1";
$ADB_PORT = $ENV{ANDROID_ADB_SERVER_PORT}    = $port;
$client = new_ok("ADB::Client");
is($client->version, $version, "Expect same version") || exit;
my $adb_version = adb_run("version");
like($adb_version, qr{^\QAndroid Debug Bridge version 1.0.$version\E\s},
     "Expected adb binary");
is(adb_run("devices"), "List of devices attached
10.253.0.13:5555	device
52000c4748d6a283	device

", "Talking to fake adb server");
