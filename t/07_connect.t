#!/usr/bin/perl -w
# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl 07_connect.t'
#########################
## no critic (UselessNoCritic MagicNumbers)

use strict;
use warnings;

our $VERSION = "1.000";

use FindBin qw($Bin);
use lib $Bin;
use Test::More tests => 24;
use TestDrive qw(adb_start adb_unacceptable adb_unreachable dumper);
use Storable qw(dclone);

# We already checked loading in 02_adb_client.t
use ADB::Client qw(mainloop);

my $port = adb_start();
# $port = 5037;

my @results;
my $callback = sub { push @results, [shift->connected, @{dclone(\@_)}] };

my $client = new_ok("ADB::Client" =>
                    [host => "127.0.0.1", port => $port, blocking => 0]);
is_deeply(\@results, [], "No results yet");
$client->marker(callback => $callback);
$client->connect(callback => $callback);
$client->marker(callback => $callback);
$client->version(callback => $callback);
$client->marker(callback => $callback);
mainloop();
# dumper(\@results);
is_deeply(\@results, [
    [ 0, undef ],
    [ 1, undef, {
        "bind_addr" => $results[1][2]{bind_addr},
        "bind_addr0" => $results[1][2]{bind_addr0},
        "bind_ip" => "127.0.0.1",
        "bind_port" => $port,
        "connect_addr" => $results[1][2]{connect_addr},
        "connect_ip" => "127.0.0.1",
        "connect_port" => $port,
        "connected" => $results[1][2]{connected},
        "family" => 2
    }],
    [ 1, undef ],
    [ 0, undef, 39 ],
    [ 0, undef ]
], "Expected connection results");
ok($results[1][2]{connected}, "Is connected after connect");

 # Connect to something that listens but does not accept
# (but the OS will still accept for you)
@results = ();
my $aport = adb_unacceptable();
$client = new_ok("ADB::Client" =>
                    [host => "127.0.0.1", port => $aport, blocking => 0, transaction_timeout => 1]);
$client->marker(callback => $callback);
$client->connect(callback => $callback);
$client->marker(callback => $callback);
$client->version(callback => $callback);
$client->marker(callback => $callback);
mainloop();
#dumper(\@results);
is_deeply(\@results, [
    [ 0, undef ],
    [ 1, undef, {
        "bind_addr" => $results[1][2]{bind_addr},
        "bind_addr0" => $results[1][2]{bind_addr0},
        "bind_ip" => "127.0.0.1",
        "bind_port" => $aport,
        "connect_addr" => $results[1][2]{connect_addr},
        "connect_ip" => "127.0.0.1",
        "connect_port" => $aport,
        "connected" => $results[1][2]{connected},
        "family" => 2
    }],
    [ 1, undef ],
    [ 0, "Operation timed out" ],
], "Expected connection results");
ok($results[1][2]{connected}, "Is connected after connect");

# Connect to something that listens but does not accept
# (but the OS will still accept for you)
@results = ();
my $rport = adb_unreachable();
$client = new_ok("ADB::Client" =>
                    [host => "127.0.0.1", port => $rport, blocking => 0]);
$client->marker(callback => $callback);
$client->connect(callback => $callback);
$client->marker(callback => $callback);
$client->version(callback => $callback);
$client->marker(callback => $callback);
mainloop();
#dumper(\@results);
is_deeply(\@results, [
    [ 0, undef ],
    [ 0, $results[1][1]]
], "Expected connection error");
like($results[1][1], qr{^ADB server 127\.0\.0\.1 port $rport: Connect error: },
    "Expected connection error");

# Connect to something that shouldn't answer
# 192.0.2.0/24 is assigned to TEST-NET-1
# Hopefully that gives us an unreachable IP address, but maybe firewall rules
# will fake a connection refused
@results = ();
$client = new_ok("ADB::Client" =>
                    [host => "192.0.2.1", blocking => 0,
                     connection_timeout => 1]);
$client->marker(callback => $callback);
$client->connect(callback => $callback);
$client->marker(callback => $callback);
mainloop();
# dumper(\@results);
is_deeply(\@results, [
    [ 0, undef ],
    [ 0, $results[1][1]]
], "Expected connection error");
like($results[1][1], qr{^ADB server 192\.0\.2\.1 port 5037: Connect error: },
    "Expected connection error");
