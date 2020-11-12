#!/usr/bin/perl -w
# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl 12_spawn_socket.t'
#########################
## no critic (UselessNoCritic MagicNumbers)

use strict;
use warnings;

our $VERSION = "1.000";

use FindBin qw($Bin);
use lib $Bin;
use Socket qw(AF_INET SOCK_STREAM IPPROTO_TCP);
use IO::Socket::IP qw();

use Test::More tests => 38;

use TestDrive qw(addr_filter dumper);

use ADB::Client qw(mainloop $ADB);

my ($spawns, $spawns0, $spawns1, $argv, $connection_data);

my $socket = IO::Socket::IP->new(
    LocalHost	=> "127.0.0.1",
    LocalPort	=> 0,
    Listen	=> 5) || die "Could not create listening socket: $@";

# Non blocking
$spawns0 = ADB::Client::Spawn->spawns;
my ($client, @result);
$client = ADB::Client->spawn_socket($socket, blocking => 0, callback => sub {
    cmp_ok(shift, "==", $client, "callback first argument is the client");
    push @result, [@_];
});
isa_ok($client, "ADB::Client", "spawn_socket returns an ADB::Client object");
is($client->host, "127.0.0.1", "Derived proper host");
is($client->port, $socket->sockport, "Derived proper port");
mainloop();
$spawns1 = ADB::Client::Spawn->spawns;
$spawns = $spawns1 - $spawns0;
$spawns0 = $spawns1;
is($spawns, 1, "Started one server");
is($client->connected, 1, "Connected after spawn");
$connection_data = $client->connection_data;
is_deeply(addr_filter($connection_data), {
  "bind_ip" => "127.0.0.1",
  "bind_port" => $socket->sockport,
  "connect_ip" => "127.0.0.1",
  "connect_port" => $socket->sockport,
  "connected" => 1,
  "family" => 2,
  "pid" => 1
}, "Expected history") || dumper(addr_filter($connection_data));
is(ref $connection_data->{bind_addr}, "", "Plain bind_addr");
is_deeply(\@result, [[undef, $connection_data]], "Callback returns connection_data") || dumper(\@result, $connection_data);
$argv = join(" ", $client->argv(blocking => 1));
like($argv,
     qr{^-L acceptfd:\d+ fork-server server --reply-fd \d+\z}a,
     "Expected commandline");
is($client->kill(blocking => 1), "", "Kill what we just started");

# Blocking
$client = ADB::Client->spawn_socket($socket);
isa_ok($client, "ADB::Client", "spawn_socket returns an ADB::Client object");
is($client->host, "127.0.0.1", "Derived proper host");
is($client->port, $socket->sockport, "Derived proper port");
$spawns1 = ADB::Client::Spawn->spawns;
$spawns = $spawns1 - $spawns0;
$spawns0 = $spawns1;
is($spawns, 1, "Started one server");
is($client->connected, 1, "Connected after spawn");
$connection_data = $client->connection_data;
is_deeply(addr_filter($connection_data), {
  "bind_ip" => "127.0.0.1",
  "bind_port" => $socket->sockport,
  "connect_ip" => "127.0.0.1",
  "connect_port" => $socket->sockport,
  "connected" => 1,
  "family" => 2,
  "pid" => 1
}, "Expected history") || dumper(addr_filter($connection_data));
is(ref $connection_data->{bind_addr}, "", "Plain bind_addr");
$argv = join(" ", $client->argv);
like($argv,
     qr{^-L acceptfd:\d+ fork-server server --reply-fd \d+\z}a,
     "Expected commandline");
is($client->kill, "", "Kill what we just started");

# Some errors
# Call on a non-handle
eval { $client = ADB::Client->spawn_socket("Boem") };
like($@, qr{^Socket is not an IO handle at }, "Cannot spawn a scalar");

# Call on a non-socket
eval { $client = ADB::Client->spawn_socket(\*DATA) };
like($@,
     qr{^Cannot getsockname: Socket operation on non-socket at },
     "Cannot spawn a filehandle");

my $non_socket = IO::Socket::IP->new() ||
    die "Could not create non socket: $@";

socket(my $socket_plain, AF_INET, SOCK_STREAM, IPPROTO_TCP);

my $socket_bind = IO::Socket::IP->new(
    LocalHost	=> "127.0.0.1",
    LocalPort	=> 0) || die "Could not create bound socket: $@";

my $espawns = 0;
for my $_soacc ($ADB::Client::Utils::SO_ACCEPTCONN, undef) {
    my $soacc = $_soacc;
    local $ADB::Client::Utils::SO_ACCEPTCONN = $soacc;

    $client = eval { ADB::Client->spawn_socket($non_socket) };
    like($@,
         qr{^Socket is not an IO handle at },
         "Cannot spawn a non-socket");

    $client = eval { ADB::Client->spawn_socket($socket_plain) };
    like($@,
         qr{^Socket is not listening at },
         "Cannot spawn a plain socket");

    $client = eval { ADB::Client->spawn_socket($socket_bind) };
    if (defined $soacc) {
        like($@,
             qr{^Socket is not listening at },
             "Cannot spawn a bound socket");
    } else {
        my $port = $socket_bind->sockport;
        like($@,
             qr{^\QADB server 127.0.0.1 port $port: Connect error: },
             "Cannot spawn a bound socket");
    }
}
$spawns1 = ADB::Client::Spawn->spawns;
$spawns = $spawns1 - $spawns0;
$spawns0 = $spawns1;
is($spawns, 0, "Nothing more started");

eval { ADB::Client->spawn_socket };
like($@, qr{^\QOdd number of arguments at }, "Must have even arguments");
eval { ADB::Client->spawn_socket($socket, Foo => 5) };
like($@, qr{^\QUnknown argument Foo at }, "Must have even arguments");

__END__
