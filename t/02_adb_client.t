#!/usr/bin/perl -w
# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl 02_adb_client.t'
#########################
## no critic (UselessNoCritic MagicNumbers)

use strict;
use warnings;

our $VERSION = "1.000";

use Test::More tests => 36;

use Scalar::Util qw(weaken);

BEGIN {
    use_ok("ADB::Client",
           qw(mainloop event_init unloop loop_levels timer immediate
              string_from_value
              $CALLBACK_DEFAULT $ADB_HOST $ADB_PORT $ADB $DEBUG $VERBOSE $QUIET
              :events :other)) ||
        BAIL_OUT("Cannot even use ADB::Client");
}

my $failed = 0;

$SIG{__DIE__} = sub {
    BAIL_OUT("Unexpected exception: @_");
};

eval {
    ADB::Client->add_command(["failer" => "Wee", 0, 1]);
    ADB::Client->add_command(["echo" => "host:echo:%s", -1, 1]);
};
$failed += !is($@, "", "We can add commands");

my $client = new_ok("ADB::Client");
$failed += !isa_ok($client, "ADB::Client", "Proper class");
$failed += !is($client->host, "127.0.0.1", "Default ADB host");
$failed += !is($client->port, "5037", "Default ADB port");
$failed += !cmp_ok($client->client_ref->client, "==", $client, "Check client_ref loop");
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
$failed += !cmp_ok($c->client_ref, '!=', $client->client_ref,
                   "Model client_ref differs from original client");

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
$failed += !is(ADB::Client::Ref->objects, 2, "Both client objects are left");
$failed += !is(ADB::Client->objects, 2, "Both client objects are left");
$c = undef;
$failed += !is(ADB::Client::Ref->objects, 1, "Only the client object is left");
$failed += !is(ADB::Client->objects, 1, "Only the client object is left");
my $ref = $client->client_ref;
weaken(my $client_ref = $ref);
$failed += ok($client_ref, "There is a client ref even for a failed client");
$client = undef;
$failed += !is(ADB::Client::Ref->objects, 1, "Ref keeps ADB::Client::Ref alive");
$failed += !is(ADB::Client->objects, 0, "No objects are left, ref does not keep ADB::Client alive");
$failed += !is(ADB::Client::Command->objects, 0, "Still no command objects are left");
$ref = undef;
$failed += !is(ADB::Client::Ref->objects, 0, "No objects are left");
$failed += !is(ADB::Client->objects, 0, "No objects are left, ref does not keep ADB::Client alive");
$failed += is($client_ref, undef, "Weak ref is gone");
$failed += !is(ADB::Client::Command->objects, 0, "Still no command objects are left");

#BAIL_OUT("Basic tests already fail") if $failed;
# We already got the object counts ourselves
$QUIET = 1;
