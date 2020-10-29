#!/usr/bin/perl -w
# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl 05_marker.t'
#########################
## no critic (UselessNoCritic MagicNumbers)

use strict;
use warnings;

our $VERSION = "1.000";

use FindBin qw($Bin);
use lib $Bin;
use Test::More tests => 17;
use TestDrive qw(adb_start);
use Storable qw(dclone);

# We already checked loading in 02_adb_client.t
use ADB::Client qw(mainloop);

$SIG{__DIE__} = sub {
    BAIL_OUT("Unexpected exception: @_");
};

my $port = adb_start();
# $port = 5037;

my @results;
my $callback = sub { shift; push @results, dclone(\@_) };

my $client = new_ok("ADB::Client" =>
                    [host => "127.0.0.1", port => $port, blocking => 0]);

$client->marker(callback => $callback);
$client->version(callback => $callback);
$client->marker(callback => $callback);
$client->version(callback => $callback);
$client->marker(callback => $callback);
$client->failer(callback => $callback);
$client->marker(callback => $callback);
is_deeply(\@results, [], "Nothing started yet") ||
    BAIL_OUT("Stuff started executing before mainloop");
mainloop();
is_deeply(\@results, [
  [ undef ],
  [ undef, 39 ],
  [ undef ],
  [ undef, 39 ],
  [ undef ],
  [ "unknown host service" ]
], "Proper sequencing") ||
    BAIL_OUT("Improper sequencing");


$client = new_ok("ADB::Client" =>
                 [host => "127.0.0.1", port => $port]);
@results = ();
$client->marker(blocking => 0, callback => $callback);
$client->version(blocking => 0, callback => $callback);
$client->marker(blocking => 0, callback => $callback);
$client->marker(blocking => 0, callback => $callback);
$client->version(blocking => 0, callback => $callback);
$client->marker(blocking => 0, callback => $callback);
$client->marker(blocking => 0);
$client->failer(blocking => 0, callback => $callback);
$client->marker(blocking => 0, callback => $callback);
$client->version(blocking => 0, callback => $callback);
is_deeply(\@results, [], "Nothing started yet") ||
    BAIL_OUT("Stuff started executing before mainloop");
my $dummy = eval {
    $SIG{__DIE__} = undef;
    $client->marker;
};
my $err = $@;
ok($err, "Marker unexpectedly succeeded") ||
    BAIL_OUT("Marker unexpectedly succeeded");
like($err, qr{^A previous command in the queue failed at },
     "Proper failed marker result") ||
    BAIL_OUT("Unexpected marker error");
is_deeply(\@results, [
  [ undef ],
  [ undef, 39 ],
  [ undef ],
  [ undef ],
  [ undef, 39 ],
  [ undef ],
  [ "unknown host service" ]
], "Everything upto failer worked") ||
    BAIL_OUT("Improper sequencing");
@results = ();
$client->activate;
my $result = $client->marker;
is($result, undef, "Proper marker result") ||
    BAIL_OUT("Improper marker result");
is_deeply(\@results, [
  [ undef ],
  [ undef, 39 ],
], "Everything upto failer worked") ||
    BAIL_OUT("Improper sequencing");
