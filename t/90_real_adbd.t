#!/usr/bin/perl -w
# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl 90_real_adbd.t'
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
        plan skip_all => "By default no tests are done using the real ADB server. If you want to run this please set the environment variable ADB_CLIENT_TEST_REAL to the name of your adb binary (probably 'adb') before doing 'make test'. Notice that this will start the real ADB server and leave it running even after the test finishes";
        exit;
    }
    delete $ENV{ANDROID_ADB_LOG_PATH};
    # Intentionally leave ANDROID_ADB_SERVER_ADDRESS, ANDROID_ADB_SERVER_PORT
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
    $host = $ENV{ANDROID_ADB_SERVER_ADDRESS} // "127.0.0.1";
    $port = $ENV{ANDROID_ADB_SERVER_PORT} // 5037;
    my $socket = IO::Socket::IP->new(
        PeerHost => $host,
        PeerPort => $port);
    if (!$socket) {
        plan skip_all => "Cannot connect to 127.0.0.1:5037 even after '$adb start-server': $@";
        exit;
    }
    plan tests => 17;
}

use TestDrive qw(dumper);
use ADB::Client qw($ADB $ADB_HOST $ADB_PORT);

# Use the real server
$ADB = $adb;
$ADB_HOST = $host;
$ADB_PORT = $port;

# Connect to the real server
my $client = new_ok("ADB::Client");
my $version = $client->version;
ok($version, "adb version $version");
# diag("Your ADB server has version $version");
ok($client->devices, "Can fetch devices");

# See if we can start servers by ourselves
my $s = IO::Socket::IP->new(
    LocalHost	=> "127.0.0.1",
    LocalPort	=> 0,
    ReuseAddr	=> 1) || die "Could not create bound socket: $@";
my $free_port = $s->sockport;
my $client2 = new_ok("ADB::Client" => [port => $free_port]);
$client2->spawn();
my $version2 = $client2->version;
ok($version2, "New ADB server has version $version2");
# Notice that this should be the same as $version since start-server will
# kill the running server if the versions don't match
is($version2, $version, "Versions match");
is($client2->kill, "", "Kill the new ADB server");

# Not exactly sure when "acceptfd" was implemented.
# It's not there in adb version 39, but works in adb version 41
SKIP: {
    skip "ADB version is below 41", 3 if $version2 < 41;

    my $s = IO::Socket::IP->new(
        LocalHost	=> "127.0.0.1",
        LocalPort	=> 0,
        ReuseAddr	=> 1,
        Listen	=> 5) || die "Could not create listening socket: $@";

    my $client3 = ADB::Client->spawn_socket($s);
    my $version3 = $client3->version;
    ok($version3, "New ADB server has version $version3");
    is($version3, $version2, "Versions match");
    is($client3->kill, "", "Kill the new ADB server");
}
