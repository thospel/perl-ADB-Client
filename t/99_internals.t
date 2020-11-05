#!/usr/bin/perl -w
# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl 99_internals.t'
#########################
## no critic (UselessNoCritic MagicNumbers)

# Check some implementation details
# Nothing in here implies a supported API

use strict;
use warnings;

our $VERSION = "1.000";

my $client_name = my $ref_name = my $cmd_name = "dummy";
my (@info_command, @info_client, @info_ref, @info_events, $socket_fd);

use FindBin qw($Bin);
use lib $Bin;
use Test::More tests => 100;

# END must come before ADB::Client gets imported so we can catch the END blocks
# from ADB::Client and its helper modules
# However it must come AFTER Test::More so done_testing doesn't get confused
END {
    # dumper(\@info_command);
    is_deeply(\@info_command, [
        "DESTROY $cmd_name",
        "Still have -1 ADB::Client::Command objects at program end",
    ], "Expected ADB::Client::Command object counter complaint");
    # dumper(\@info_ref);
    is_deeply(\@info_ref, [
        "DESTROY $ref_name",
        "Still have -1 ADB::Client::Ref objects at program end",
    ], "Expected ADB::Client::Command object counter complaint");
    # dumper(\@info_client);
    is_deeply(\@info_client, [
        "DESTROY $client_name",
        "Still have -1 ADB::Client objects at program end",
    ], "Expected ADB::Client::Command object counter complaint");
    # dumper(\@info_events);
    is_deeply(\@info_events, [
        [ "add_read $socket_fd" ],
        [ "delete_read $socket_fd" ],
        [ "add_write $socket_fd" ],
        [ "delete_write $socket_fd" ],
        [ "add_error $socket_fd" ],
        [ "delete_error $socket_fd"],
        "Entering mainloop (level 0)",
        "Exiting mainloop (level 0)",
        "Entering mainloop (level 1)",
        "Exiting mainloop (level 1)",
    ], "Expected add and delete info lines");
}

use TestDrive qw(adb_start adb_unreachable dumper
                 collect_stderr collected_stderr uncollect_stderr
                 %expect_objects);
use ADB::Client qw(mainloop event_init unloop timer immediate);
use ADB::Client::Events qw($IGNORE_PIPE_LOCAL);
use ADB::Client::Utils qw(callers info caller_info addr_info
                          realtime_running clocktime_running);
use IO::Socket::IP;

my $port  = adb_start();
my $rport = adb_unreachable();

collect_stderr();
info("Test1");
uncollect_stderr();
like(collected_stderr(),
     qr{^\d+-\d+-\d+ \d+:\d+:\d+.\d+ [+-]\d+: Test1\n\z}a,
     "Test single argument info");

collect_stderr();
info("T%s", "Test2");
uncollect_stderr();
like(collected_stderr(),
     qr{^\d+-\d+-\d+ \d+:\d+:\d+.\d+ [+-]\d+: TTest2\n\z}a,
     "Test single argument info");

collect_stderr();
info();
uncollect_stderr();
like(collected_stderr(),
     qr{^\d+-\d+-\d+ \d+:\d+:\d+.\d+ [+-]\d+: t/99_internals\.t \d+\n\z}a,
     "Test single argument info");

collect_stderr();
caller_info("Test3");
uncollect_stderr();
like(collected_stderr(),
     qr{^\d+-\d+-\d+ \d+:\d+:\d+.\d+ [+-]\d+: Test3 \[99_internals\.t:\d+\]\n\z}a,
     "Test single argument info");

collect_stderr();
caller_info("T%s", "Test4");
uncollect_stderr();
like(collected_stderr(),
     qr{^\d+-\d+-\d+ \d+:\d+:\d+.\d+ [+-]\d+: TTest4 \[99_internals\.t:\d+\]\n\z}a,
     "Test single argument info");

collect_stderr();
ADB::Client::Utils::dumper(["ab", { "a" => 4}]);
uncollect_stderr();
like(collected_stderr(),
     # qr{^\[}a,
     qr{^\[\n  "ab",\n  \{\n    "a" => 4\n  \}\n\]\n\z}a,
     "Test single argument info");

my $running = realtime_running();
cmp_ok($running, '>',  0, "Realtime running is > 0");
cmp_ok($running, '<', 10, "Realtime running is < 10 (unless computer is really slow)");

$running = clocktime_running();
cmp_ok($running, '>',  0, "Clocktime running is > 0");
cmp_ok($running, '<', 10, "Clocktime running is < 10 (unless computer is really slow)");

like(sub { sub { callers() }->() }->(),
     qr{^99_internals\.t:\d+ 99_internals\.t:\d+\z}a,
     "Can do callers");


eval { addr_info("zzzz.www.example.com", 80) };
like($@, qr{^Could not resolve\(zzzz\.www\.example\.com, 80\): },
     "Cannot resolve non-existing address");

my $err = addr_info("zzzz.www.example.com", 80, 1);
like($err, qr{^Could not resolve\(zzzz\.www\.example\.com, 80\): },
     "Cannot resolve non-existing address");

my $socket = IO::Socket::IP->new(LocalHost => "127.0.0.1") ||
    die "Could not create socket: $@";
$socket_fd = fileno($socket);
#fileno($socket) // die "Socket without filedescriptor";

my $socket1 = IO::Socket::IP->new(LocalHost => "127.0.0.1") ||
    die "Could not create socket: $@";
#fileno($socket1) // die "Socket without filedescriptor";

# To improve coverage
$IGNORE_PIPE_LOCAL = undef;

# Trigger EVENT_INITER in mainloop()
mainloop();
event_init();

my $read = 0;
$socket->add_read(sub { $socket->delete_read; return $read = 1 });

my $timed = 0;
my $immed = 0;

cmp_ok(\&timer, '!=', \&ADB::Client::Events::timer,
       "timer is the placeholder");
cmp_ok(\&immediate, '!=', \&ADB::Client::Events::immediate,
       "immediate is the placeholder");

package Foo;
my @timers = (
    ::timer(0,  sub { ++$timed }),			# 0
    ::immediate(sub { ++$immed }),			# 1
    ::immediate(sub { ++$immed }),
    ADB::Client::Timer->new(-1, sub { ++$timed }),
    ADB::Client::Timer->new(-3, sub { ++$timed }),
    ADB::Client::Timer->new(-6, sub { ++$timed }),
    ADB::Client::Timer->new(-2, sub { ++$timed }),
    ADB::Client::Timer->new(-4, sub { ++$timed }),
    ADB::Client::Timer->new(-5, sub { ++$timed }),
    ADB::Client::Timer->new(1e-6, sub { ++$timed }),
    ADB::Client::Timer->new(2e-6, sub { ++$timed }),
    ADB::Client::Timer->new(3e-6, sub { ++$timed }),
    ADB::Client::Timer->new(4e-6, sub { ++$timed }),
    ADB::Client::Timer->new(2, sub { ++$timed }),
    ADB::Client::Timer->new(4, sub { ++$timed }),
    ADB::Client::Timer->new(3, sub { ++$timed }),
);
$timers[2] = undef;
$timers[5] = undef;
$timers[6] = undef;
$timers[7] = undef;
$timers[4] = undef;
$timers[-5] = undef;
$timers[-7] = undef;
$timers[-2] = undef;
$timers[-1] = undef;
$timers[-3] = undef;

sub client_ref {
    return undef;
}

package main;
is($timed, 0, "Timed not set yet");
is($immed, 0, "Immed not set yet");
mainloop();
is($read, 1, "Read triggered");

is(Foo->can("timer"),     undef, "Did not parachute timer");
is(Foo->can("immediate"), undef, "Did not parachute immediate");

cmp_ok(\&timer, '!=', \&ADB::Client::Events::timer,
       "timer is the placeholder");
cmp_ok(\&immediate, '!=', \&ADB::Client::Events::immediate,
       "immediate is the placeholder");
push @timers, timer(0, sub { ++$timed });
push @timers, immediate(sub { ++$immed });
cmp_ok(\&timer, '==', \&ADB::Client::Events::timer,
       "timer is replaced");
cmp_ok(\&immediate, '==', \&ADB::Client::Events::immediate,
       "immediate is replaced");
is($timed, 5, "Timed not set yet");
is($immed, 1, "Immed not set yet");
mainloop();
is($timed, 6, "Timed set");
is($immed, 2, "Immed set");
mainloop();
is($timed, 6, "Timed did not restart");
is($immed, 2, "Immed did not restart");

cmp_ok(\&timer, '==', \&ADB::Client::Events::timer,
       "timer is still replaced");
cmp_ok(\&immediate, '==', \&ADB::Client::Events::immediate,
       "immediate is still replaced");

for my $name (qw(read write error)) {
    my $err;
    my $add_name    = "add_$name";
    my $delete_name = "delete_$name";
    my $add_fun    = ADB::Client::Events->can($add_name);
    my $delete_fun = ADB::Client::Events->can($delete_name);

    $socket1->$add_name(sub {});

    eval { $add_fun->(0) };
    like($@, qr{^Not a filehandle at },
         "Proper eror from $add_name call on non fh");

    eval { $delete_fun->(0) };
    like($@, qr{^Not a filehandle at },
         "Proper eror from $add_name call on non fh");

    $socket->$add_name(sub {});
    eval { $socket->$add_name(sub {}) };
    like($@, qr{^Descriptor \d+ already selected for $name at },
         "Proper eror from duplicate $add_name call");
    $socket->$delete_name($socket);
    eval { $socket->$delete_name(sub {}) };
    like($@, qr{^Descriptor \d+ wasn't selected for $name at }a,
         "Proper eror from duplicate $add_name call");

    $socket1->$delete_name(sub {});
}

eval { ADB::Client->add_command(["space space" => "Wee", 0, 1]) };
like($@, qr{^Illegal declaration of subroutine ADB::Client::space at }, "");

# Try some bad ADB::Client->new
eval { ADB::Client->new(undef) };
like($@, qr{^Odd number of arguments at }, "");

eval { ADB::Client->new(model => bless [], "Foo") };
like($@, qr{^Model without client_ref at }, "");

# Try some bad ADB::Client::Ref commands
my $client = new_ok("ADB::Client", [port => $rport, blocking => 0]);

eval { $client->post_activate() };
like($@, qr{^Missing post_activate argument at },
     "Expected error from version");
eval { $client->post_activate(undef) };
like($@, qr{^Missing post_activate argument at },
     "Expected error from version");
eval { $client->post_activate(0) };
like($@, qr{^post_activate outside success or error callback at },
     "Expected error from version");

is($client->connection_data, undef, "connection_data starts undef");
eval { $client->version(foo => 9) };
like($@, qr{^Unknown argument foo at }, "Expected error from version");
eval { $client->client_ref->command_simple({}, undef, 1000000) };
like($@, qr{^No command at index '1000000' at },
     "Expected error from command_simple");

eval { $client->marker(foo => 9) };
like($@, qr{^Unknown argument foo at }, "Expected error from special_simple");
eval { $client->client_ref->special_simple({}, undef, 1000000) };
like($@, qr{^No command at index '1000000' at },
     "Expected error from command_simple");

eval { ADB::Client::Ref->command_get(1000000) };
like($@, qr{^No command at index '1000000' at },
     "Expected error from command_get");
eval {
    local $ADB::Client::Ref::COMMANDS[0] = [];
    ADB::Client::Ref->command_get(0);
};
like($@, qr{^Assertion: No COMMAND_NAME at },
     "Expected error from command_get");

eval { $client->connect(foo => 9) };
like($@, qr{^Unknown argument foo at }, "Expected error from connect");
eval { $client->client_ref->connect({}, undef, 1000000) };
like($@, qr{^No command at index '1000000' at },
     "Expected error from version");

eval { $client->spawn(foo => 9) };
like($@, qr{^Unknown argument foo at }, "Expected error from spawn");
eval { $client->client_ref->spawn({}, undef, 1000000) };
like($@, qr{^No command at index '1000000' at },
     "Expected error from version");

eval { $client->echo("a" x 100000, blocking => 1) };
like($@, qr{^Command too long: "internal:echo:a+"\.\.\. at },
     "Expected error from version");

is($client->resolve(port => 1, host => "127.0.0.2", blocking => 1), undef,
   "Can set to nonsense host/port");
is($client->host, "127.0.0.2", "Host was set");
is($client->port, 1, "Port was set");
is($client->resolve(host => undef, port => undef, blocking => 1), undef,
   "Can revert to default host/port");
is($client->host, "127.0.0.1", "Host was set");
is($client->port, 5037, "Port was set");
{
    local $ADB::Client::Ref::ADB_HOST = "";
    local $ADB::Client::Ref::ADB_PORT = 0;

    is($client->resolve(host => undef, port => undef,
                        addr_info => addr_info("1.2.3.4", 2),
                        blocking => 1), undef,
       "Can revert to default host/port");
    is($client->host, "", "Host was set");
    is($client->port, 0, "Port was set");
}
eval { $client->client_ref->_resolve(blocking => 1) };
like($@, qr{^Fatal: Assertion: No command at }, "Resolve without commands");
# This broke $client. Create a new one
$client = new_ok("ADB::Client", [port => $rport, blocking => 0]);

my $hash = {};
my ($result, $retired);
$client->resolve(
    host => "Waffle",
    callback => sub {
        my ($client, $err) = @_;
        $retired = $client->command_retired;
        $result = $err;
        $client->post_activate(1);
    });
{
    no warnings "redefine";
    local *ADB::Client::Ref::utils_addr_info = sub { return $hash};
    is_deeply([$client->client_ref->_resolve], [],
              "Bad _resolv returns nothing");
};
cmp_ok($result, "==", $hash, "Bad addr_info");
isa_ok($retired, "ADB::Client::Command", "Can get command");
is_deeply($retired->arguments, { host => "Waffle" },
          "Arguments are in the old command");
isa_ok($retired->ref, "ARRAY", "Commandref is an ARRAY reference");
is($retired->command_name, "resolve", "Can get command name");
$retired = undef;

eval { $client->client_ref->error("Boem") };
like($@, qr{^Fatal: Assertion: error without command at },
     "Expected error from version");
# This broke $client. Create a new one
$client = new_ok("ADB::Client", [port => $rport, blocking => 0]);

eval { $client->client_ref->success("Success") };
like($@, qr{^Fatal: Assertion: success without command at },
     "Expected error from version");
# This broke $client. Create a new one
$client = new_ok("ADB::Client", [port => $rport, blocking => 0]);

# Finally trigger object count errors
$client = undef;
@timers = ();
$ADB::Client::Command::DEBUG = 1;
{
    no warnings "redefine";
    *ADB::Client::Command::info = sub {
        push @info_command, @_ == 1 ? shift : sprintf(shift, @_);
    };
    *ADB::Client::Ref::info = sub {
        push @info_ref, @_ == 1 ? shift : sprintf(shift, @_);
    };
    *ADB::Client::info = sub {
        push @info_client, @_ == 1 ? shift : sprintf(shift, @_);
    };
    *ADB::Client::Events::info = sub {
        push @info_events, @_ == 1 ? shift : sprintf(shift, @_);
    };

    *ADB::Client::Ref::caller_info    = sub { push @info_ref,    \@_ };
    *ADB::Client::Events::caller_info = sub { push @info_events, \@_ };
}
for my $name (qw(read write error)) {
    my $add_name    = "add_$name";
    my $delete_name = "delete_$name";
    $socket->$add_name(sub {});
    $socket->$delete_name;
}
$ADB::Client::Events::VERBOSE = 1;
mainloop();
$ADB::Client::Events::VERBOSE = 0;
unloop(0, 1);
mainloop();

{
    no warnings "redefine";
    local *ADB::Client::Events::caller_info = sub {};
    local *ADB::Client::Events::info        = sub {};
    $socket->add_read(sub {});
    close($socket);
    eval { mainloop() };
    like($@, qr{^Select failed: }, "Proper error on bad filedescriptor");
}

my $cmd = bless [], "ADB::Client::Command";
my $ref = bless {}, "ADB::Client::Ref";
$client = bless \$ref, "ADB::Client";

$cmd_name = "$cmd";
$ref_name = "$ref";
$client_name = "$client";
--$expect_objects{$_} for qw(ADB::Client::Command ADB::Client::Ref ADB::Client);
