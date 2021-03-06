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

use Test::More tests => 36;
use TestDrive qw(adb_start dumper);

# We already checked loading in 04_adb_client.t
use ADB::Client qw(mainloop);

my $port = adb_start();
my $client = new_ok("ADB::Client" =>
                    [host => "127.0.0.1", port => $port]);
ok(!$client->is_fatal, "We know the client isn't fatal");
is($client->version(), 39, "Expected Version");
ok($client->_connect(), "Can connect");
ok(!$client->is_fatal, "We know the client still isn't fatal");
eval { $client->fatal("Bullet") };
like($@, qr{^Fatal: Assertion: Bullet at }, "Expected fatal error");
ok($client->is_fatal, "We know the client is fatal");
for my $i (1..2) {
    eval { $client->activate(0) };
    like($@, qr{^Attempt to restart a dead ADB::Client at },
         "Expected fatal error [$i]");
}
ok($client->is_fatal, "We know the client is fatal");

# Ok, mount a scratch ADB::Client
$client = new_ok("ADB::Client" =>
                    [host => "127.0.0.1", port => $port, blocking => 0]);

eval { $client->_fatal(foo => 9) };
like($@, qr{^Unknown argument foo at }, "Fatal needs proper arguments");
eval { $client->_special__fatal({}, undef, 1000000) };
like($@, qr{^No command at index '1000000' at }, "Fatal needs proper arguments");

eval { $client->_fatal(blocking => 1) };
like($@, qr{^\QAttempt to restart a dead ADB::Client at }, "Blocking fatal");

# Ok, mount a scratch ADB::Client
$client = new_ok("ADB::Client" =>
                    [host => "127.0.0.1", port => $port, blocking => 0]);

my @results;
my $callback = sub { push @results, [shift->connected, @{dclone(\@_)}] };
$client->version(callback => $callback);
$client->_fatal(callback => $callback);
$client->version(callback => $callback);
# diag(__LINE__);
eval { mainloop() };
like($@, qr{^Attempt to restart a dead ADB::Client at },
     "Expected dead ADB::Client");
ok($client->is_fatal, "We know the client is fatal");
# diag(__LINE__);

# dumper(\@results);
is_deeply(\@results, [ [ 0, undef, 39 ]], "The commands before _fatal do run");
for my $i (1..2) {
    eval { $client->activate(0) };
    like($@, qr{^Attempt to restart a dead ADB::Client at },
         "Expected fatal error [$i]");
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
like($@, qr{^Attempt to restart a dead ADB::Client at },
     "Expected dead ADB::Client");
# dumper(\@results);
is_deeply(\@results, [], "The commands before _fatal do run");
for (1..2) {
    eval { $client->activate(0) };
    like($@, qr{^Attempt to restart a dead ADB::Client at },
         "Expected fatal error");
}
