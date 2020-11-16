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
use Test::More tests => 35;
use TestDrive qw(adb_start dumper);

# We already checked loading in 04_adb_client.t
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
is($client->transport_usb, "", "Connect to usb device");
is($client->remount, qq(Not running as root. Try "adb root" first.\n),
   "Cannot remount as non-root");
is($client->transport_usb, "", "Connect to usb device");
is($client->root,   "restarting adbd as root\n", "Can set root");
is($client->transport_usb, "", "Connect to usb device");
is($client->root,   "adbd is already running as root\n", "Can set root");
is($client->transport_usb, "", "Connect to usb device");
is($client->unroot, "restarting adbd as non root\n", "Can set root");
is($client->transport_usb, "", "Connect to usb device");
is($client->unroot, "adbd not running as root\n", "Can set root");
is($client->transport_usb, "", "Connect to usb device");
is($client->root,   "restarting adbd as root\n", "Can set root");
is($client->transport_usb, "", "Connect to usb device");
is($client->remount, qq(remount succeeded\n),
   "Can remount as root");

is($client->transport_usb, "", "Connect to usb device");
is($client->verity_enable, "", "Can enable verity");
is($client->transport_usb, "", "Connect to usb device");
is($client->verity_disable, "", "Can disable verity");
eval { $client->verity_disable };
like($@, qr{^\Qdevice offline (no transport) at}, "verity needs a transport");
