#!/usr/bin/perl -w
# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl 21_non_loopback.t'
#########################
## no critic (UselessNoCritic MagicNumbers)

use strict;
use warnings;

our $VERSION = "1.000";

use FindBin qw($Bin);
use lib $Bin;
use Errno qw(EADDRNOTAVAIL);
use Socket qw(inet_ntoa inet_aton pack_sockaddr_in unpack_sockaddr_in
              AF_INET SOCK_DGRAM IPPROTO_UDP);
use IO::Socket::IP qw();

use Test::More;

my ($example_host, $example_ip, $remote_ip, $local_ip);
BEGIN {
    $example_host = $ENV{ADB_CLIENT_TEST_REMOTE} || "www.example.com";
    if (my $addr = inet_aton($example_host)) {
        $example_ip = inet_ntoa($addr);
        socket(my $udp, AF_INET, SOCK_DGRAM, IPPROTO_UDP) ||
            die "Could not create AF_INET udp socket: $^E";
        if (!bind($udp, pack_sockaddr_in(0, $addr))) {
            $! == EADDRNOTAVAIL ||
                die "Could not bind UDP socket to $example_ip: $^E";
            $remote_ip = $example_ip;
        }
    } else {
        $example_ip = "";
    }
    if (!$remote_ip) {
        my $addr = inet_aton("1.2.3.4");
        for (0..999) {
            my $ip = inet_ntoa($addr) ||
                die "Assertion: Invalid packed IP ", unpack("N", $addr);
            socket(my $udp, AF_INET, SOCK_DGRAM, IPPROTO_UDP) ||
                die "Could not create AF_INET udp socket: $^E";
            if (!bind($udp, pack_sockaddr_in(0, $addr))) {
                $! == EADDRNOTAVAIL ||
                    die "Could not bind UDP socket to $ip: $^E";
                $remote_ip = $ip;
                last;
            }
            $addr = pack("N", rand((224-1)*2**24)+2**24);
        }
        if (!$remote_ip) {
            plan skip_all => "Could not determine any non local IP. It seems I can bind to anything";
            exit;
        }
    }
    socket(my $udp, AF_INET, SOCK_DGRAM, IPPROTO_UDP) ||
        die "Could not create AF_INET udp socket: $^E";
    connect($udp, pack_sockaddr_in(1, inet_aton($remote_ip))) ||
        die "Could not dummy connect UDP socket to $remote_ip:80: $^E";
    my $addr = getsockname($udp) ||
        die "Could not getsockname: $^E";
    (undef, $addr) = unpack_sockaddr_in($addr);
    $local_ip = inet_ntoa($addr);

    plan tests => 22;
}
# diag("$example_host, $example_ip, $remote_ip, $local_ip");

use TestDrive qw(adb_start addr_filter dumper);

use ADB::Client;
use ADB::Client::Utils qw(addr_info);

# Cannot bind to an already connected remote IP
SKIP: {
    skip "Cannot resolve $example_host", 2 unless $example_ip;
    # Use a pretty low timeout. It's more important that I can run this
    # test on my development system than that the user can
    # (with the default timeout adb_fake will autoquit in the mean time)
    my $client = new_ok("ADB::Client" =>
                        [host => $example_ip, port => 80,
                         connection_timeout => 0.5, blocking => 1]);
    eval { $client->connect() };
  SKIP: {
        skip $@, 1 if $@;
        skip "Huh, $example_host is local",1 unless $example_ip eq $remote_ip;
        eval { $client->spawn() };
        like($@, qr{^\QCould not bind to $example_ip ($example_ip): },
         "Can connect but not bind to $example_host ($example_ip)");
    }
}

my $port = adb_start();

# Cannot bind to a remote IP
my $addr_info =
    [@{addr_info($remote_ip, 1)},	# Remote
     @{addr_info("127.0.0.1", $port)},	# OK
     ];
is(@$addr_info, 2, "Two addresses");

my $client = new_ok("ADB::Client" =>
                    [host => "127.0.0.1", port => $port,
                     addr_info => $addr_info, blocking => 1]);
my @result = eval { $client->spawn() };
like($@, qr{^\QCould not bind to $remote_ip (127.0.0.1): }, "Cannot bind to remote ip");

# Can spawn on a local IP
my $s = IO::Socket::IP->new(
    LocalHost	=> $local_ip,
    LocalPort	=> 0,
    ReuseAddr	=> 1) || die "Could not create bound socket: $@";
my $free_port = $s->sockport;

$client = new_ok("ADB::Client" =>
                    [host => $local_ip, port => $free_port, adb_socket => 1]);
@result = $client->spawn;
is_deeply(addr_filter(\@result), [
  {
    "bind_ip" => $local_ip,
    "bind_port" => $free_port,
    "connect_ip" => $local_ip,
    "connect_port" => $free_port,
    "connected" => 1,
    "family" => AF_INET,
    "last_connect_error" => "Connect error: Connection refused",
    "pid" => 1
  }
], "Expected history") || dumper(addr_filter(\@result));
my $client2 = new_ok("ADB::Client" => [port => $free_port]);
eval { $client2->connect };
like($@, qr{^\QADB server 127.0.0.1 port $free_port: Connect error: },
     "Not running on 127.0.0.1");

is($client->kill, "", "Kill the just started server");
