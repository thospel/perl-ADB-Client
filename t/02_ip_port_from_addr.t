#!/usr/bin/perl -w
# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl 02_ip_port_from_addr.t'
#########################
## no critic (UselessNoCritic MagicNumbers)

use strict;
use warnings;

our $VERSION = '1.000';

use FindBin qw($Bin);
use lib $Bin;
use Socket qw(AF_INET);
use IO::Socket::IP qw();

use Test::More tests => 4;

use ADB::Client::Utils qw(ip_port_from_addr addr_from_ip_port);
use Socket qw(AF_INET);

my $socket = IO::Socket::IP->new(
    LocalHost => "127.0.0.1",
    LocalPort => 0) || die "Could not create IO::Socket::IP: $@";
my ($ip, $port, $family) = ip_port_from_addr(getsockname($socket));
is($ip, "127.0.0.1", "Decoded host");
is($port, $socket->sockport, "Decoded Port");
is($family, AF_INET, "Decoded family");

my $addr = addr_from_ip_port($ip, $port);
is_deeply([ip_port_from_addr($addr)], [$ip, $port, AF_INET], "Can roundtrip");
