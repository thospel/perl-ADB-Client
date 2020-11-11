#!/usr/bin/perl -w
# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl 05_adb_fake.t'
#########################
## no critic (UselessNoCritic MagicNumbers)

use strict;
use warnings;

our $VERSION = "1.000";

use FindBin qw($Bin);
use lib $Bin;
use Socket qw(AF_INET);

use Test::More tests => 102;
use TestDrive qw(adb_start adb_stop adb_unacceptable adb_unreachable addr_filter
                 dumper $UNREACHABLE);

# We already checked loading in 02_adb_client.t
use ADB::Client qw(mainloop immediate);

my $obj_failed = 0;
my $failed;

$SIG{__DIE__} = sub {
    BAIL_OUT("Unexpected exception: @_");
};

immediate(sub {});

my $port = adb_start();
# $port = 5037;

# First try a basic unblocking call
# Also make sure we are talking to the fake adb server
# Also tests a command with arguments
for (1..2) {
    my $client = new_ok("ADB::Client" => [host => "127.0.0.1", port => $port, blocking => 0]);
    my ($echo, $c);
    my $err = "No error";
    $client->echo("Foo", callback => sub {
                      # The command is still stored in retired
                      $obj_failed += !is(ADB::Client::Command->objects, 1, "Command objects are gone");
                      ($c, $err, $echo) = @_;
                  });
    isa_ok($client->addr_info, "ARRAY", "Can fetch addr_info clone");
    isa_ok($client->_addr_info, "ARRAY", "Can fetch internal addr_info");
    cmp_ok($client->addr_info, '!=', $client->_addr_info,
           "Clone and internal addr_info are different");
    is_deeply($client->addr_info, $client->_addr_info,
              "Clone and internal addr_info are identical");
    mainloop();
    is($err, undef, "No error (loop $_)") ||
        BAIL_OUT("Error while getting echo from fake adb server: $err");
    is($echo, "Foo", "Correct version (loop $_)") ||
        BAIL_OUT("Bad echo from fake adb server");
    cmp_ok($c, "==", $client, "Expected client in callback") ||
        BAIL_OUT("Callback does not return original ADB::Client object");
    $obj_failed += !is(ADB::Client::Command->objects, 0, "Command objects are gone");
}

# Next try a real (simulated) command
# First try a basic unblocking call
for (1..2) {
    my $client = new_ok("ADB::Client" => [host => "127.0.0.1", port => $port, blocking => 0]);
    my ($version, $c);
    my $err = "No error";
    $client->version(callback => sub {
                         # The command is still stored in retired
                         $obj_failed += !is(ADB::Client::Command->objects, 1, "Command objects are gone");
                         ($c, $err, $version) = @_;
                     });
    mainloop();
    is($err, undef, "No error (loop $_)") ||
        BAIL_OUT("Error while getting version from fake adb server: $err");
    is($version, 39, "Correct version (loop $_)") ||
        BAIL_OUT("Bad version from fake adb server");
    cmp_ok($c, "==", $client, "Expected client in callback") ||
        BAIL_OUT("Callback does not return original ADB::Client object");
    $obj_failed += !is(ADB::Client::Command->objects, 0, "Command objects are gone");

    # Test the default callback
    $client->version();
    mainloop();
    # Well, the default callback doesn't do anything on success,
    # so there is no result to check...
    $obj_failed += !is(ADB::Client::Command->objects, 0, "Command objects are gone");

    is(scalar $client->version(blocking => 1), 39, "Expected version") ||
        BAIL_OUT("Cannot do a blocking command on a non-blocking ADB::Client");
}

# Next try a basic blocking call
for (1..2) {
    my $client = new_ok("ADB::Client" => [host => "127.0.0.1", port => $port]);
    my $version = eval { $client->version };
    is($@, "", "No version error (loop $_)") ||
        BAIL_OUT("Error while getting version from fake adb server");
    is($version, 39, "Correct version (loop $_)") ||
        BAIL_OUT("Bad version from fake adb server");
    $obj_failed += !is(ADB::Client::Command->objects, 0, "Command objects are gone");
}

my $client = new_ok("ADB::Client" => [host => "127.0.0.1", port => $port]);
my $result = $client->connect;
is_deeply(addr_filter($result), {
   "bind_ip" => "127.0.0.1",
   "bind_port" => $port,
   "connect_ip" => "127.0.0.1",
   "connect_port" => $port,
   "connected" => 1,
   "family" => AF_INET,
 }, "Get a proper connection") ||
    BAIL_OUT("Didn't get a proper connection to the fake adb server");
is($client->connected, 1, "Succesfully connected") ||
    BAIL_OUT("Connection to the fake adb server without socket");

# Test that we get a proper error for unknown commands
# First try a basic blocking call
for (1..2) {
    my $client = new_ok("ADB::Client" => [host => "127.0.0.1", port => $port, blocking => 0]);
    my ($dummy, $c);
    my $err = "No error";
    $client->failer(callback => sub {
                        # The command is still stored in retired
                        $obj_failed += !is(ADB::Client::Command->objects, 1, "Command objects are gone");
                        ($c, $err, $dummy) = @_;
                    });
    mainloop();
    is($err, "unknown host service", "Error (loop $_)") ||
        BAIL_OUT("Bad error when sending an unknown command to fake adb server");
    is($dummy, undef, "Correct echo (loop $_)") ||
        BAIL_OUT("Bad version from fake adb server");
    cmp_ok($c, "==", $client, "Expected client in callback") ||
        BAIL_OUT("Callback does not return original ADB::Client object");
    $obj_failed += !is(ADB::Client::Command->objects, 0, "Command objects are gone");

    # Test the default callback
    $client->failer();
    eval {
        local $SIG{__DIE__} = undef;
        mainloop();
    };
    like($@, qr{^unknown host service at }, "Error (loop $_)") ||
        BAIL_OUT("Bad error when sending an unknown command to fake adb server");
    $obj_failed += !is(ADB::Client::Command->objects, 0, "Command objects are gone");
}

$result = $client->connect;
is_deeply(addr_filter($result), {
  "bind_ip" => "127.0.0.1",
  "bind_port" => $port,
  "connect_ip" => "127.0.0.1",
  "connect_port" => $port,
  "connected" => 1,
  "family" => AF_INET,
}, "Can still do a connect and get connection properties") ||
    BAIL_OUT("Cannot do repeat connect on connected client");
cmp_ok($result->{connected}, '>', 0, "Connection time is positive");

# Before testing the blocking variant of the call, set up some other

# ADB:Client objects that we keep around. This checks that we can have
# more than one object at a time

# Check a port we can't connect to
my $rport = adb_unreachable();
$client = new_ok("ADB::Client" => [host => "127.0.0.1", port => $rport]);
my $dummy = eval {
    local $SIG{__DIE__} = undef;
    $client->connect;
};
my $err = $@;
ok($err) ||
    BAIL_OUT("Could connect to non listening port: $err");
like($err, qr{^ADB server 127.0.0.1 port $rport: Connect error: },
     "Expeced error on connection to non listening port") ||
    BAIL_OUT("Could connect to non listening port");
is($client->connected, 0, "Client is not connected") ||
    BAIL_OUT("Have connection to non listening port");

# Check a port that doesn't answer
# Implicitely checks basic connect and that adb_fake supports multiple ports
my $aport = adb_unacceptable();
$client = new_ok("ADB::Client" => [host => "127.0.0.1", port => $aport]);
my @result = $client->connect();
is_deeply(addr_filter(\@result), [{
     "bind_ip" => "127.0.0.1",
     "bind_port" => $aport,
     "connect_ip" => "127.0.0.1",
     "connect_port" => $aport,
     "connected" => 1,
     "family" => AF_INET,
   }], "We can still connect to a port not doing accepts") ||
    BAIL_OUT("Cannot connect to listening but unresponsive port");
# We leave the connection open
is($client->connected, 1, "Client is still connected") ||
    BAIL_OUT("Lost connection to listening but unresponsive port");

# Next try a basic blocking call
for (1..2) {
    my $client = new_ok("ADB::Client" => [host => "127.0.0.1", port => $port]);
    my $dummy = eval {
        local $SIG{__DIE__} = undef;
        $client->failer;
    };
    my $err = $@;
    like($err, qr{^unknown host service at }, "Expected error from unknown command") ||
        BAIL_OUT("Bad error when sending an unknown command to fake adb server: $err");
    $obj_failed += !is(ADB::Client::Command->objects, 0, "Command objects are gone");
}

adb_stop(0) eq "" ||
    BAIL_OUT("Fake adb server already stopped. Something in TestDrive.pm is broken");
adb_stop(1) ||
    BAIL_OUT("Unexpected final status from Fake adb server");
$client = new_ok("ADB::Client" => [host => "127.0.0.1", port => $port]);
$dummy = eval {
    local $SIG{__DIE__} = undef;
    $client->version;
};
$err = $@;
like($err, qr{^ADB server 127\.0\.0\.1 port $port: Connect error: },
     "Must have a connection error") ||
    BAIL_OUT("Stopped fake adb server does not lead to the proper error message: $err");
$obj_failed += !is(ADB::Client::Ref->objects, 1, "Client object still exists");
$obj_failed += !is(ADB::Client->objects, 1, "Client objects still exists");
$obj_failed += !is(ADB::Client::Command->objects, 0, "Command objects are gone");
$client = undef;
$obj_failed += !is(ADB::Client::Ref->objects, 0, "Client object is gone");
$obj_failed += !is(ADB::Client->objects, 0, "Client objects is gone");
$obj_failed += !is(ADB::Client::Command->objects, 0, "Command objects are gone");
BAIL_OUT("Object management is broken") if $obj_failed;
