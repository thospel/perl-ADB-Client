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

my $tests;
BEGIN { $tests = 38 }
use Test::More tests => $tests;

use TestDrive qw(adb_start adb_server dumper $developer $tests_driver);

# We already checked loading in 04_adb_client.t
use ADB::Client qw(mainloop $ADB_HOST $ADB_PORT);

SKIP : {
    if ($developer) {
        my $version = adb_server();
        if ($version < 41) {
          SKIP: {
                diag("ADB server version $version has too many missing features for a developer test. Need at least version 41");
                skip "ADB server version $version has too many missing features for a developer test. Need at least version 41", $tests-$tests_driver;
            }
            exit;
        }
        skip "Developer mode doesn't start a fake adb server", 5;
    }
    my $port_10 = adb_start(10);

    $ADB_HOST = "127.0.0.1";
    $ADB_PORT = $port_10;
}

my ($client, @result);

$client = new_ok("ADB::Client");

# Test special characters and utf8
my $str = "Abcde";
SKIP: {
    skip "Real ADB daemon does not support echo", 2 if $developer;
    is($client->echo($str), $str, "Special characters and utf8");
    $str = " abc\nd\r\tf zg\x{123}z\0z ";
    is($client->echo($str), $str, "Special characters and utf8");
}
$str = " abc\nd\r\tf zg\x{123}z\0z ";
eval { $client->connect($str) };
like($@, qr{^\QArgument cannot be converted to native 8 bit encoding at },
     "But by default no utf8");

@result = $client->host_features;
is_deeply(\@result, [
    {
        "abb" => 1,
        "abb_exec" => 1,
        "apex" => 1,
        "cmd" => 1,
        "fixed_push_mkdir" => 1,
        "fixed_push_symlink_timestamp" => 1,
        "ls_v2" => 1,
        "push_sync" => 1,
        "remount_shell" => 1,
        "sendrecv_v2" => 1,
        "sendrecv_v2_brotli" => 1,
        "sendrecv_v2_dry_run_send" => 1,
        "sendrecv_v2_lz4" => 1,
        "sendrecv_v2_zstd" => 1,
        "shell_v2" => 1,
        "stat_v2" => 1,
        "track_app" => 1
    },
    [
        "shell_v2",
        "cmd",
        "stat_v2",
        "ls_v2",
        "fixed_push_mkdir",
        "apex",
        "abb",
        "fixed_push_symlink_timestamp",
        "abb_exec",
        "remount_shell",
        "track_app",
        "sendrecv_v2",
        "sendrecv_v2_brotli",
        "sendrecv_v2_lz4",
        "sendrecv_v2_zstd",
        "sendrecv_v2_dry_run_send",
        "push_sync"
    ],
    "shell_v2,cmd,stat_v2,ls_v2,fixed_push_mkdir,apex,abb,fixed_push_symlink_timestamp,abb_exec,remount_shell,track_app,sendrecv_v2,sendrecv_v2_brotli,sendrecv_v2_lz4,sendrecv_v2_zstd,sendrecv_v2_dry_run_send,push_sync"
], "Get host features") || dumper(\@result);

if ($developer) {
    my $transport_id = $client->tport_usb;
    if ($client->unroot =~ /restarting/i) {
        $client->wait_id($transport_id, "disconnect");
        $client->wait_usb("device");
    }
}
is($client->transport_usb, "", "Connect to usb device");
is($client->remount, qq(Not running as root. Try "adb root" first.\n),
   "Cannot remount as non-root");
my $transport_id = $client->tport_usb;
like($transport_id, qr{^\d+\z}a, "Connect to usb device");
if ($developer) {
    like($client->root, qr{^restarting adbd as root\n\z|^\z}, "Can set root");
} else {
    is($client->root, "restarting adbd as root\n", "Can set root");
}
$client->wait_id($transport_id, "disconnect");
$client->wait_usb("device");
$transport_id = $client->tport_usb;
like($transport_id, qr{^\d+\z}a, "Connect to usb device");
is($client->root,   "adbd is already running as root\n", "Can set root");

is($client->transport_id($transport_id), "", "Connect to usb device");
if ($developer) {
    like($client->unroot, qr{^restarting adbd as non root\n\z|^\z},
         "Can unset root");
} else {
    is($client->unroot, "restarting adbd as non root\n", "Can unset root");
}
$client->wait_id($transport_id, "disconnect");
$client->wait_usb("device");
$transport_id = $client->tport_usb;
like($transport_id, qr{^\d+\z}a, "Connect to usb device");
is($client->unroot, "adbd not running as root\n", "Can not unroot twice");
is($client->tport_id($transport_id), $transport_id, "Connect to usb device");
if ($developer) {
    like($client->root, qr{^restarting adbd as root\n\z|^\z}, "Can set root");
} else {
    is($client->root, "restarting adbd as root\n", "Can set root");
}
$client->wait_id($transport_id, "disconnect");
$client->wait_usb("device");
my $transport_id2 = $client->tport_usb;
like($transport_id2, qr{^\d+\z}a, "Connect to usb device");
cmp_ok($transport_id2, "!=", $transport_id, "root changes transport");
is($client->remount, qq(remount succeeded\n),
   "Can remount as root");

# Verity changes need root
# I know of no query to get the current state. Since my real devices don't
# support verity as $developer I don't care in what state this is left.
# And as non $developer all this is only on the fake adbd so it doesn't matter
is($client->transport_usb, "", "Connect to usb device");
is($client->verity_enable, "", "Can enable verity");
is($client->transport_usb, "", "Connect to usb device");
is($client->verity_disable, "", "Can disable verity");
eval { $client->verity_disable };
like($@, qr{^\Qdevice offline (no transport) at}, "verity needs a transport");

# Restore to non-root (in case we are $developer, don't leave adbd rooted)
$transport_id = $client->tport_usb;
if ($developer) {
    like($client->unroot, qr{^restarting adbd as non root\n\z|^\z},
         "Restore non-root");
} else {
    is($client->unroot, "restarting adbd as non root\n", "Restore non-root");
}
$client->wait_id($transport_id, "disconnect");
$client->wait_usb("device");
