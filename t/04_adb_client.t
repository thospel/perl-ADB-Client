#!/usr/bin/perl -w
# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl 04_adb_client.t'
#########################
## no critic (UselessNoCritic MagicNumbers)

use strict;
use warnings;

our $VERSION = "1.000";

use Test::More tests => 29;

use Scalar::Util qw(weaken);

BEGIN {
    $ENV{ADB_CLIENT_ENV} = 0;
    # delete @ENV{grep /^ANDROID_/, sort keys %ENV};

    use_ok("ADB::Client",
           qw(mainloop event_init unloop loop_levels timer immediate
              string_from_value
              $ADB_HOST $ADB_PORT $ADB $DEBUG $VERBOSE $QUIET
              $TRANSACTION_TIMEOUT $CONNECTION_TIMEOUT $SPAWN_TIMEOUT
              $BLOCK_SIZE
              :events :other)) ||
        BAIL_OUT("Cannot even use ADB::Client");
    use_ok("ADB::Client::Utils",
           qw(addr_info info caller_info callers dumper
              string_from_value display_string adb_check_response
              realtime clocktime realtime_running clocktime_running
              $BASE_REALTIME $BASE_CLOCKTIME $CLOCK_TYPE
              $DEBUG $VERBOSE $QUIET $ADB_HOST $ADB_PORT
              OKAY FAIL SUCCEEDED FAILED BAD_ADB ASSERTION INFINITY
              DISPLAY_MAX)) ||
                  BAIL_OUT("Cannot even use ADB::Client::Utils");
    use_ok("ADB::Client::Command", qw(EXPECT_EOF)) ||
        BAIL_OUT("Cannot even use ADB::Client::Command");
}

my $failed = 0;

$SIG{__DIE__} = sub {
    BAIL_OUT("Unexpected exception: @_");
};

eval {
    ADB::Client->command_add(["failer" => "Wee", 0, 1]);
    ADB::Client->command_add(["echo" => "internal:echo:%s", -1, 1]);
};
$failed += !is($@, "", "We can add commands");

my $client = new_ok("ADB::Client");
$failed += !isa_ok($client, "ADB::Client", "Proper class");
$failed += !is($client->host, "127.0.0.1", "Default ADB host");
$failed += !is($client->port, "5037", "Default ADB port");
$failed += !ok(!$client->connected, "We start unnconnected");

# Test basic argument handling
my $host = "1.2.3.4";
$client = new_ok("ADB::Client" => [
    host => $host,
    port => 1,
    # Al kinds of other arguments
    blocking		=> 1,
    reresolve		=> 5,
    connection_timeout	=> 12,
    transaction_timeout => 14,
    block_size		=> 1234,
    adb			=> "./Zoef",
    adb_socket		=> 8,
]);
$failed += !isa_ok($client, "ADB::Client", "Proper class");
$failed += !is($client->host, $host, "Can set ADB host");
$failed += !is($client->port, 1, "Can set ADB port");

# Test some model argument handling
my $c = new_ok("ADB::Client" => [model => $client]);
$failed += !isa_ok($c, "ADB::Client", "Proper class");
$failed += !is($c->host, $host, "Can set ADB host");
$failed += !is($c->port, 1, "Can set ADB port");
$failed += !cmp_ok($c, '!=', $client,
                   "Model client differs from original client");

$c = new_ok("ADB::Client" => [model => $client, port => 2]);
$failed += !isa_ok($c, "ADB::Client", "Proper class");
$failed += !is($c->host, $host, "Can set ADB host");
$failed += !is($c->port, 2, "Can set ADB port");

my $dummy = eval {
    local $SIG{__DIE__} = undef;
    ADB::Client->new(port => 1, Zorro => 8);
};
my $err = $@;
$failed += !like($@, qr{^Unknown argument Zorro at },
                 "Proper error for bad argument");

# Test object cleanup
$failed += !is(ADB::Client::Command->objects, 0, "No command objects are left");
$failed += !is(ADB::Client->objects, 2, "Both client objects are left");
$c = undef;
$failed += !is(ADB::Client->objects, 1, "Only the client object is left");
$client = undef;
$failed += !is(ADB::Client->objects, 0, "No objects are left");
$failed += !is(ADB::Client->objects, 0, "No objects are left");
$failed += !is(ADB::Client::Command->objects, 0, "Still no command objects are left");

#BAIL_OUT("Basic tests already fail") if $failed;
# We already got the object counts ourselves
$QUIET = 1;
