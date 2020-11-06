#!/usr/bin/perl -w
# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl 98_fatal.t'
#########################
## no critic (UselessNoCritic MagicNumbers)

# Check some implementation details
# Nothing in here implies a supported API

use strict;
use warnings;

our $VERSION = "1.000";

use FindBin qw($Bin);
use lib $Bin;
use Storable qw(dclone);

use Test::More tests => 29;
use TestDrive qw(adb_start dumper);

# We already checked loading in 02_adb_client.t
use ADB::Client qw(mainloop);

my $port = adb_start();
my $client = new_ok("ADB::Client" =>
                    [host => "127.0.0.1", port => $port]);
is($client->version(), 39, "Expected Version");
ok($client->connect(), "Can connect");
eval { $client->fatal("Bullet") };
my $err = $@;
like($err, qr{^Fatal: Assertion: Bullet at }, "Expected fatal error");
for (1..2) {
    eval { $client->activate };
    $err = $@;
    like($err, qr{^Attempt to restart a dead ADB::Client at },
         "Expected fatal error");
}

# Ok, mount a scratch ADB::Client
$client = new_ok("ADB::Client" =>
                    [host => "127.0.0.1", port => $port, blocking => 0]);

eval { $client->_fatal(foo => 9) };
$err = $@;
like($err, qr{^Unknown argument foo at }, "Fatal needs proper arguments");
eval { $client->client_ref->_fatal({}, undef, 1000000) };
$err = $@;
like($err, qr{^No command at index '1000000' at }, "Fatal needs proper arguments");

my @results;
my $callback = sub { push @results, [shift->connected, @{dclone(\@_)}] };
$client->version(callback => $callback);
$client->_fatal(callback => $callback);
$client->version(callback => $callback);
eval { mainloop() };
$err = $@;
like($err, qr{^Attempt to restart a dead ADB::Client at },
     "Expected dead ADB::Client");
# dumper(\@results);
is_deeply(\@results, [ [ 0, undef, 39 ]], "The commands before _fatal do run");
for (1..2) {
    eval { $client->activate };
    $err = $@;
    like($err, qr{^Attempt to restart a dead ADB::Client at },
         "Expected fatal error");
}

# Ok, mount a scratch ADB::Client
@results = ();
$client = new_ok("ADB::Client" =>
                    [host => "127.0.0.1", port => $port, blocking => 0]);
$client->_fatal(callback => $callback);
$client->version(callback => $callback);
$client->_fatal(callback => $callback);
$client->version(callback => $callback);
eval { mainloop() };
$err = $@;
like($err, qr{^Attempt to restart a dead ADB::Client at },
     "Expected dead ADB::Client");
# dumper(\@results);
is_deeply(\@results, [], "The commands before _fatal do run");
for (1..2) {
    eval { $client->activate };
    $err = $@;
    like($err, qr{^Attempt to restart a dead ADB::Client at },
         "Expected fatal error");
}
