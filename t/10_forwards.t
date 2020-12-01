#!/usr/bin/perl -w
# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl 10_forwards.t'
#########################
## no critic (UselessNoCritic MagicNumbers)
# Test some simple commands

use strict;
use warnings;

our $VERSION = "1.000";

use FindBin qw($Bin);
use lib $Bin;
use Test::More tests => 40;
use TestDrive qw(adb_server adb_start adb_version dumper $developer $tests_pre);

# We already checked loading in 04_adb_client.t
use ADB::Client qw(mainloop $ADB_HOST $ADB_PORT);

my $version = 10;
SKIP : {
    if ($developer) {
        $version = adb_server();
        skip "Developer mode doesn't start a fake adb server", $tests_pre;
    }
    my $port = adb_start($version);

    $ADB_HOST = "127.0.0.1";
    $ADB_PORT = $port;
}

my $socket = IO::Socket::IP->new(
    LocalHost	=> "127.0.0.1",
    LocalPort	=> 0,
    ReuseAddr	=> 1) || die "Could not create bound socket: $@";
my $free_port = $socket->sockport;

my $client = new_ok("ADB::Client");

### Forward
is($client->forward_kill_all, "", "Remove all forwards");

my @result =$client->forward_list;
is_deeply(\@result, [{}], "No forwards yet") || dumper(\@result);
eval { $client->forward("tcp:0", "unix:/abc") };
like($@, $developer ?
     qr{^\Qmore than one device/emulator at \E|^\QUnknown error at } :
     qr{^\Qmore than one device/emulator at },
     "Must select a device");
my $port0 = $client->forward_usb("tcp:0", "unix:/abc");
like($port0, qr{^[1-9][0-9]*\z}, "Add a forward, get an allocated port");
is($client->forward_local("tcp:$free_port", "unix:/def"), $developer && $version < 41 ? "" : $free_port, "Add a forward");
is($client->forward_serial("10.253.0.13:5555", "tcp:$free_port", "unix:/fgh"), "",
   "Set through serial");
@result =$client->forward_list;
is_deeply(\@result, [
    {
        "tcp:$port0" => {
            "serial" => "52000c4748d6a283",
            "to" => "unix:/abc"
        },
        "tcp:$free_port" => {
            "serial" => "10.253.0.13:5555",
            "to" => "unix:/fgh"
        }
    }
], "Forwards were effective") || dumper(\@result);
is($client->forward_kill_usb("tcp:$port0"), "", "Remove Port");

eval { $client->forward_kill_usb("tcp:$port0") };
like($@, qr{^\Qlistener 'tcp:$port0' not found at }, "Cannot remove a forward twice");

eval { $client->forward_norebind_serial("10.253.0.13:5555", "tcp:$free_port", "unix:/ghi") };
like($@, qr{^\Qcannot rebind existing socket at }, "Cannot remove a forward twice");

is($client->forward_serial("10.253.0.13:5555", "tcp:$free_port", "unix:/xyz"),
   "", "Can rebind without norebind");
@result = $client->forward_list;
is_deeply(\@result, [
    {
        "tcp:$free_port" => {
            "serial" => "10.253.0.13:5555",
            "to" => "unix:/xyz"
        }
    }
], "Forwards did indeed rebind") || dumper(\@result);

is($client->forward_kill_all, "", "Remove all forwards");
@result = $client->forward_list;
is_deeply(\@result, [{}], "Clear all forwards") || dumper(\@result);

### Reverse
my $devices = $client->devices;
if ($developer) {
    for my $serial (keys %$devices) {
        $client->transport_serial($serial);
        $client->reverse_kill_all();
    }
}

my ($serial1, $serial2) = sort keys %$devices;
$client->transport_serial($serial1);
@result =$client->reverse_list;
is_deeply(\@result, [{}], "No reverses yet") || dumper(\@result);

$client->transport_serial($serial1);
$port0 = $client->reverse("tcp:0", "unix:/abc");
like($port0, qr{^[1-9][0-9]*\z}, "Add a reverse, get an allocated port");

$client->transport_serial($serial1);
@result =$client->reverse_list;
is_deeply(\@result, [ { "tcp:$port0" => "unix:/abc" } ],
          "Reverses were effective") || dumper(\@result);

$client->transport_serial($serial2);
is($client->reverse("tcp:$free_port", "unix:/def"), $developer ? "" : $free_port, "Add a reverse");

$client->transport_serial($serial2);
@result =$client->reverse_list;
is_deeply(\@result, [ { "tcp:$free_port" => "unix:/def" } ],
          "Reverses were effective") || dumper(\@result);

$client->transport_serial($serial2);
is($client->reverse_kill("tcp:$free_port"), "", "Remove Port");

$client->transport_serial($serial2);
eval { $client->reverse_kill("tcp:$free_port") };
like($@, qr{^\Qlistener 'tcp:$free_port' not found at }, "Cannot remove a reverse twice");

$client->transport_serial($serial1);
eval { $client->reverse_norebind("tcp:$port0", "unix:/ghi") };
like($@, qr{^\Qcannot rebind existing socket at }, "Cannot remove a reverse twice");

$client->transport_serial($serial1);
is($client->reverse("tcp:$port0", "unix:/xyz"),
   "", "Can rebind without norebind");

$client->transport_serial($serial1);
@result = $client->reverse_list;
is_deeply(\@result, [ { "tcp:$port0" => "unix:/xyz" } ],
          "Reverses did indeed rebind") || dumper(\@result);

for my $serial ($serial1, $serial2) {
    $client->transport_serial($serial);
    is($client->reverse_kill_all, "", "Remove all reverses");

    $client->transport_serial($serial);
    @result = $client->reverse_list;
    is_deeply(\@result, [{}], "Cleared all reverses") || dumper(\@result);
}
