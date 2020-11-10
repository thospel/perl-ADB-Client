#!/usr/bin/perl -w
# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl 20_ipv6.t'
#########################
## no critic (UselessNoCritic MagicNumbers)

use strict;
use warnings;

our $VERSION = "1.000";

use FindBin qw($Bin);
use lib $Bin;
use Test::More;
use Errno qw(EPROTONOSUPPORT);
use Socket qw(AF_INET AF_INET6 SOCK_STREAM IPPROTO_TCP);
use IO::Socket::IP qw();

BEGIN {
    if (!socket(my $s, AF_INET6, SOCK_STREAM, IPPROTO_TCP)) {
        $! == EPROTONOSUPPORT || die "Cannot create socket: $^E";
        plan skip_all => "IPv6 not supported on this host";
        exit;
    }
    plan tests => 77;
}

use TestDrive qw(adb_start adb_version6 adb_unreachable6 addr_filter dumper);

# We already checked loading in 02_adb_client.t
use ADB::Client;
use ADB::Client::Utils qw(ip_port_from_addr addr_from_ip_port is_listening
                          IPV6 addr_info);

is(IPV6, 1, "We know we support IPv6");
my (@result, $client, $addr_info, $_addr_info);

# Test ip_port_from_addr
my $socket = IO::Socket::IP->new(
    LocalHost => "::1",
    LocalPort => 0) || die "Could not create IO::Socket::IP: $@";
my ($ip, $port, $family, @rest) = ip_port_from_addr(getsockname($socket));
is($ip, "::1", "Decoded host");
is($port, $socket->sockport, "Decoded Port");
is($family, AF_INET6, "Decoded family");
is_deeply(\@rest, [0, 0], "Expected rest");
my $addr = addr_from_ip_port($ip, $port, 1, 2);
is_deeply([ip_port_from_addr($addr)], [$ip, $port, AF_INET6, 1, 2],
          "Can roundtrip");


# Test is_listening

my $socket_listen = IO::Socket::IP->new(
    LocalHost	=> "::1",
    LocalPort	=> 0,
    Listen	=> 5) || die "Could not create listening socket: $@";
socket(my $socket_plain, AF_INET6, SOCK_STREAM, IPPROTO_TCP);

my $socket_listen0 = IO::Socket::IP->new(
    LocalHost	=> "::0",
    LocalPort	=> 0,
    Listen	=> 5) || die "Could not create listening socket: $@";

my $socket_bind = IO::Socket::IP->new(
    LocalHost	=> "::1",
    LocalPort	=> 0) || die "Could not create bound socket: $@";

my $socket_connected = IO::Socket::IP->new(
    PeerHost	=> "::1",
    PeerPort	=> $socket_listen->sockport) ||
    die "Could not create connected socket: $@";
for my $_soacc ($ADB::Client::Utils::SO_ACCEPTCONN, undef) {
    my $soacc = $_soacc;
    local $ADB::Client::Utils::SO_ACCEPTCONN = $soacc;

    is(is_listening($socket_listen), 1, "Socket is listening");
    is(is_listening($socket_listen0), 1, "Socket is listening");
    is(is_listening($socket_plain), 0, "Plain Socket");
    is(is_listening($socket_bind), defined $soacc ? 0 : 1, "Socket is bound");
    is(is_listening($socket_plain), 0, "Socket is connected");

    if (0) {
        my $client = ADB::Client->new(adb_socket => $socket_listen0);
        is($client->host, "::", "Can decode host");
        is($client->port, $socket_listen0->sockport, "Can decode host");

        eval { ADB::Client->new(adb_socket => $socket_plain) };
        like($@, qr{^adb_socket is not listening at },
             "Cannot use plain socket as adb_socket");
    }
}

$port = adb_start();
# Keep this default one alive
my $port6 = adb_version6();
# These you are allowed to kill
my $port_10 = adb_version6(10);
my $port_20 = adb_version6(20);
# Occupy a port that rejects connections
my $rport = adb_unreachable6();

# Simple IPV6 connect
$client = new_ok("ADB::Client" =>
                    [host => "::1", port => $port6]);
@result = $client->connect;
is_deeply(addr_filter(\@result), [
  {
    "bind_ip" => "::1",
    "bind_port" => $port6,
    "connect_ip" => "::1",
    "connect_port" => $port6,
    "connected" => 1,
    "family" => AF_INET6,
  }
], "Can connect with IPv6") || dumper(addr_filter(\@result));
is($client->version, 39, "Expected version");

# Connect to :: is like connect to ::1
$client = new_ok("ADB::Client" =>
                    [host => "::0", port => $port6]);
@result = $client->connect;
is_deeply(addr_filter(\@result), [
  {
    "bind_ip" => "::",
    "bind_port" => $port6,
    "connect_ip" => "::1",
    "connect_port" => $port6,
    "connected" => 1,
    "family" => AF_INET6,
  }
], "Can connect with IPv6") || dumper(addr_filter(\@result));
is($client->version, 39, "Expected version");

# Version scan without kill.
# Not everything is occupied and have no good version
# Use any address
$addr_info =
    [@{addr_info("::", $rport)}, 	# Reject connections
     @{addr_info("::1", $port_10)},	# OK, version 10
     @{addr_info("::", $port_20)},	# OK, version 20
 ];

for my $adb_socket (0, 1, -1) {
    is(ADB::Client->new(host => "::1", port => $port_10)->version, 10,
       "Keep adb_fake alive");

    $client = new_ok("ADB::Client" =>
                     [host => "127.0.0.1", port => $port,
                      adb_socket => $adb_socket,
                      addr_info => $addr_info, blocking => 1]);
    @result = eval { $client->spawn(version_min => 25) };
    is($@, "", "We can occupy the first connection [$adb_socket]");
    is_deeply(\@result, [$client->connection_data],
              "We are connected [$adb_socket]");
    $_addr_info = $client->_addr_info;
    is_deeply(addr_filter($_addr_info), [
        {
            "bind_ip" => "::",
            "bind_port" => $rport,
            "connect_ip" => "::1",
            "connect_port" => $rport,
            "connected" => 1,
            "family" => AF_INET6,
            "last_connect_error" => "Connect error: Connection refused",
            "pid" => 1,
            "version" => 30,
        },
        {
            "bind_ip" => "::1",
            "bind_port" => $port_10,
            "connect_ip" => "::1",
            "connect_port" => $port_10,
            "connected" => 1,
            "family" => AF_INET6,
            "last_connect_error" => "Version '10' is below '25'",
            "version" => 10,
        },
        {
            "bind_ip" => "::",
            "bind_port" => $port_20,
            "connect_ip" => "::1",
            "connect_port" => $port_20,
            "connected" => 1,
            "family" => AF_INET6,
            "last_connect_error" => "Version '20' is below '25'",
            "version" => 20,
        }
    ], "Expected history [$adb_socket]") || dumper(addr_filter($_addr_info));
    is($client->pid, $result[0]{pid}, "Proper pid [$adb_socket]");
    is($client->version, 30, "Expected version [$adb_socket]");
    my $argv = join(" ", $client->argv);
    like($argv,
         $adb_socket ?
         qr{^-L acceptfd:\d+ fork-server server --reply-fd \d+\z}a :
         qr{^-a -L tcp:$rport fork-server server --reply-fd \d+\z}a,
         "Expected commandline");

    # We can also reach it using IPv4
    my $client2 = new_ok("ADB::Client" =>
                     [host => "127.0.0.1", port => $rport,
                      adb_socket => $adb_socket]);
    my $result = eval { $client2->version };
    if ($adb_socket == -1) {
        like($@, qr{^\QADB server 127.0.0.1 port $rport: Connect error: },
             "IPv4-mapped is not allowed [$adb_socket]");
        is($result, undef, "Expected version [$adb_socket]");
    } else {
        is($@, "", "No error [$adb_socket]");
        is($result, 30, "Expected version [$adb_socket]");
    }

    is($client->kill, "", "Kill what we just created [$adb_socket]");
}

#my $s = IO::Socket::IP(
#    LocalHost	=> "::1",
#    LocalPort	=> 0,
#    Reuseaddr	=> 1) || die "Cannot create socket: $@";
#ADB::Client->new(
#V6Only
