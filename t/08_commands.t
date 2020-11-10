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
use Test::More tests => 26;
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

# Test host:devices
@result = $client->devices;
is_deeply(\@result, [
    {
        "10.253.0.13:5555" => "device",
        "52000c4748d6a283" => "device"
    },
    ["10.253.0.13:5555", "52000c4748d6a283"],
    "10.253.0.13:5555\tdevice\n52000c4748d6a283\tdevice\n"
], "Expected devices result");

# Test host:devices-l
@result = $client->devices_long;
is_deeply(\@result, [
    {
        "10.253.0.13:5555" => {
            "device" => "zeroflte",
            "model" => "SM_G920F",
            "product" => "zerofltexx",
            "state" => "device",
            "transport_id" => 3
        },
        "52000c4748d6a283" => {
            "device" => "kminiltexx",
            "model" => "SM_G800F",
            "product" => "lineage_kminilte",
            "state" => "device",
            "transport_id" => 2,
            "usb" => "1-1.2"
        }
    },
    ["10.253.0.13:5555", "52000c4748d6a283"],
    "10.253.0.13:5555\tdevice\tproduct:zerofltexx model:SM_G920F device:zeroflte transport_id:3\n52000c4748d6a283\tdevice\tusb:1-1.2 product:lineage_kminilte model:SM_G800F device:kminiltexx transport_id:2\n"],
          "Expected devices long result");

is($client->remount, qq(Not running as root. Try "adb root" first.\n),
   "Cannot remount as non-root");
is($client->root,   "restarting adbd as root\n", "Can set root");
is($client->root,   "adbd is already running as root\n", "Can set root");
is($client->unroot, "restarting adbd as non root\n", "Can set root");
is($client->unroot, "adbd not running as root\n", "Can set root");
is($client->root,   "restarting adbd as root\n", "Can set root");
is($client->remount, qq(remount succeeded\n),
   "Can remount as root");

eval { $client->features };
like($@, qr{^more than one device/emulator at }, "Cannot get features from more than 1 device");
$client->device_drop("10.253.0.13:5555");
@result = $client->features;
is_deeply(\@result, [{
    "cmd" => 1,
    "shell_v2" => 1,
    "stat_v2" => 1
}, [
    "shell_v2",
    "cmd",
    "stat_v2"
], "shell_v2,cmd,stat_v2"], "Can get features from single device");
$client->device_drop("52000c4748d6a283");
eval { $client->features };
like($@, qr{^no devices/emulators found at }, "Cannot get features without devices");
