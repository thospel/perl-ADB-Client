#!/usr/bin/perl -w
# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl 09_devices.t'
#########################
## no critic (UselessNoCritic MagicNumbers)
# Test some simple commands

use strict;
use warnings;

our $VERSION = "1.000";

use FindBin qw($Bin);
use lib $Bin;
use Test::More tests => 19;
use TestDrive qw(adb_server adb_start adb_version dumper $developer);

# We already checked loading in 04_adb_client.t
use ADB::Client qw(mainloop $ADB_HOST $ADB_PORT);

my $version = 10;
SKIP : {
    if ($developer) {
        $version = adb_server();
        skip "Developer mode doesn't start a fake adb server", 5;
    }
    my $port = adb_start($version);

    $ADB_HOST = "127.0.0.1";
    $ADB_PORT = $port;
}

my $client = new_ok("ADB::Client");
$client->forward_kill_all if $developer;

my @result =$client->forward_list;
is_deeply(\@result, [{}], "No forwards yet") || dumper(\@result);
eval { $client->forward("tcp:0", "unix:/abc") };
like($@, $developer ?
     qr{^\Qmore than one device/emulator at \E|^\QUnknown error at } :
     qr{^\Qmore than one device/emulator at },
     "Must select a device");
my $port0 = $client->forward_usb("tcp:0", "unix:/abc");
like($port0, qr{^[1-9][0-9]*\z}, "Add a forward, get an allocated port");
is($client->forward_local("tcp:4088", "unix:/def"), $developer && $version < 41 ? "" : 4088, "Add a forward");
is($client->forward_serial("10.253.0.13:5555", "tcp:4088", "unix:/fgh"), "",
   "Set through serial");
@result =$client->forward_list;
is_deeply(\@result, [
    {
        "tcp:$port0" => {
            "serial" => "52000c4748d6a283",
            "to" => "unix:/abc"
        },
        "tcp:4088" => {
            "serial" => "10.253.0.13:5555",
            "to" => "unix:/fgh"
        }
    }
], "No forwards yet") || dumper(\@result);
