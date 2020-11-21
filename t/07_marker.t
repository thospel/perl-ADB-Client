#!/usr/bin/perl -w
# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl 07_marker.t'
#########################
## no critic (UselessNoCritic MagicNumbers)

use strict;
use warnings;

our $VERSION = "1.000";

use FindBin qw($Bin);
use lib $Bin;
use Test::More tests => 22;
use TestDrive qw(adb_start adb_server $developer);
use Storable qw(dclone);

# We already checked loading in 04_adb_client.t
use ADB::Client qw(mainloop $ADB_HOST $ADB_PORT);

$SIG{__DIE__} = sub {
    BAIL_OUT("Unexpected exception: @_");
};

my $version = 39;
SKIP : {
    if ($developer) {
        $version = adb_server();
        skip "Developer mode doesn't start a fake adb server", 5;
    }
    my $port_10 = adb_start($version);

    $ADB_HOST = "127.0.0.1";
    $ADB_PORT = $port_10;
}

# $port = 5037;

my @results;
my $callback = sub { shift; push @results, dclone(\@_) };

my $client = new_ok("ADB::Client" => [blocking => 0]);

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
  [ undef, $version ],
  [ undef ],
  [ undef, $version ],
  [ undef ],
  [ "device offline (no transport)" ]
], "Proper sequencing") ||
    BAIL_OUT("Improper sequencing");


$client = new_ok("ADB::Client");
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
like($err, qr{^\QA previous command in the queue failed at },
     "Proper failed marker result") ||
    BAIL_OUT("Unexpected marker error");
is_deeply(\@results, [
  [ undef ],
  [ undef, $version ],
  [ undef ],
  [ undef ],
  [ undef, $version ],
  [ undef ],
  [ "device offline (no transport)" ]
], "Everything upto failer worked") ||
    BAIL_OUT("Improper sequencing");
@results = ();
$client->activate;
my $result = $client->marker;
is($result, undef, "Proper marker result") ||
    BAIL_OUT("Improper marker result");
is_deeply(\@results, [
  [ undef ],
  [ undef, $version ],
], "Everything upto failer worked") ||
    BAIL_OUT("Improper sequencing");
