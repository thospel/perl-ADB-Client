#!/usr/bin/perl -w
# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl 03_is_listening.t'
#########################
## no critic (UselessNoCritic MagicNumbers)

use strict;
use warnings;

our $VERSION = '1.000';

use FindBin qw($Bin);
use lib $Bin;
use Socket qw(AF_INET SOCK_STREAM IPPROTO_TCP);
use IO::Socket::IP qw();
use Errno qw(ENOTSOCK);
my $ENOTSOCK = $! = ENOTSOCK;

use Test::More tests => 14;

use ADB::Client::Utils qw(is_listening);
use ADB::Client;

my $socket_listen = IO::Socket::IP->new(
    LocalHost	=> "127.0.0.1",
    LocalPort	=> 0,
    Listen	=> 5) || die "Could not create listening socket: $@";
socket(my $socket_plain, AF_INET, SOCK_STREAM, IPPROTO_TCP);

my $socket_listen0 = IO::Socket::IP->new(
    LocalHost	=> "0.0.0.0",
    LocalPort	=> 0,
    Listen	=> 5) || die "Could not create listening socket: $@";

my $non_socket = IO::Socket::IP->new() ||
    die "Could not create non socket: $@";

my $socket_bind = IO::Socket::IP->new(
    LocalHost	=> "127.0.0.1",
    LocalPort	=> 0) || die "Could not create bound socket: $@";

my $socket_connected = IO::Socket::IP->new(
    PeerHost	=> "127.0.0.1",
    PeerPort	=> $socket_listen->sockport) ||
    die "Could not create connected socket: $@";

for my $_soacc ($ADB::Client::Utils::SO_ACCEPTCONN, undef) {
    my $soacc = $_soacc;
    local $ADB::Client::Utils::SO_ACCEPTCONN = $soacc;

    is(is_listening($socket_listen), 1, "Socket is listening");
    is(is_listening($socket_listen0), 1, "Socket is listening on any address");
    is(is_listening($socket_plain), 0, "Plain Socket");
    eval { is_listening($non_socket) };
    like($@, qr{^Not a filehandle at }, "Not even a socket");
    is(is_listening($socket_bind), defined $soacc ? 0 : 1, "Socket is bound");
    is(is_listening($socket_plain), 0, "Socket is connected");
    eval {is_listening(\*DATA) };
    like($@, defined $soacc ? qr{^\QCould not getsockopt(SOL_SOCKET, SO_ACCEPTCONN): $ENOTSOCK at } : qr{^\QCould not getpeername: $ENOTSOCK at }, "Real filehandle");

    if (0) {
        my $client = ADB::Client->new(adb_socket => $socket_listen0);
        is($client->host, "0.0.0.0", "Can decode host");
        is($client->port, $socket_listen0->sockport, "Can decode host");

        eval { ADB::Client->new(adb_socket => $socket_plain) };
        like($@, qr{^adb_socket is not listening at },
             "Cannot use plain socket as adb_socket");

        eval { ADB::Client->new(adb_socket => \*DATA) };
        like($@, qr{^\QCannot getsockname(adb_socket): $ENOTSOCK at },
             "Cannot use plain socket as adb_socket");
    }
}

__END__
