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
my (@info_command, @info_client, @info_ref, @info_events);
my $socket_fd = -1;

use FindBin qw($Bin);
use lib $Bin;

use IO::Socket::IP qw();

use Test::More tests => 753;

# END must come before ADB::Client gets imported so we can catch the END blocks
# from ADB::Client and its helper modules
# However it must come AFTER Test::More so done_testing doesn't get confused
END {
    # dumper(\@info_command);
    is_deeply(\@info_command, [
        "DESTROY $cmd_name",
        "Still have -1 ADB::Client::Command objects at program end",
    ], "Expected ADB::Client::Command object counter complaint $cmd_name") ||
        dumper(\@info_command);
    # dumper(\@info_ref);
    is_deeply(\@info_ref, [
        "DESTROY $ref_name",
        "Still have -1 ADB::Client::Ref objects at program end",
    ], "Expected ADB::Client::Command object counter complaint") ||
        dumper(\@info_ref);
    # dumper(\@info_client);
    is_deeply(\@info_client, [
        "DESTROY $client_name",
        "Still have -1 ADB::Client objects at program end",
    ], "Expected ADB::Client::Command object counter complaint") ||
        dumper(\@info_client);
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
    ], "Expected add and delete info lines") ||
        dumper(\@info_events);
}

BEGIN { $ENV{ADB_CLIENT_ENV} = 0 };
use TestDrive qw(adb_start adb_unreachable dumper
                 collect_stderr collected_stderr uncollect_stderr
                 %expect_objects);

use ADB::Client qw(mainloop event_init unloop timer immediate);
use ADB::Client::Events qw($IGNORE_PIPE_LOCAL);
use ADB::Client::Utils qw(callers info caller_info addr_info
                          realtime_running clocktime_running get_home);
use ADB::Client::Command qw(EXPECT_EOF SERIAL);

ok(get_home(), "get_home returns something");
$ENV{HOME} = "/foo";
is(get_home(), "/foo", "can set HOME environment variable");
$ENV{HOME} = "/foo/";
is(get_home(), "/foo", "get_home removes trailing /");
$ENV{HOME} = "/";
is(get_home(), "/", "get_home does not removes trailing /");
delete $ENV{HOME};
my $home = get_home();
ok($home, "get_home still returns something");
is($ENV{HOME}, $home, "HOME environment variable gets set");

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
# So this should do nothing (do this for better test coverage)
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

my $client = new_ok("ADB::Client", [port => $port]);

# Trigger some more internal stuff
my $client2 = $client->new;
is($client2->port, $port, "We copied some stuff");
$client2 = $client->new(model => ADB::Client->new(port => 1234));
is($client2->port, 1234, "We copied some stuff");

# Cause a die during the mainloop of a blocking command
# See if we properly recover
is($client->version, 39, "Sanity check. We can run");
is($client->discard("Some text"), "discarded", "Can run discard comand");
$client2 = new_ok("ADB::Client", [port => $port, blocking => 0]);
$client2->marker(callback => sub {
    # While we are here, check the tests for queueing while blocking
    eval { $client->version };
    like($@, qr{^\QAlready have a blocking command pending at },
         "Test duplicate block");
    eval { $client->version(blocking => 0) };
    like($@, qr{^\QAlready have a blocking command pending at },
         "Test duplicate block");
    eval { $client->_fatal(blocking => 0) };
    like($@, qr{^\QAlready have a blocking command pending at },
         "Test duplicate block");
    eval { $client->_connect(blocking => 0) };
    like($@, qr{^\QAlready have a blocking command pending at },
         "Test duplicate block");
    eval { $client->spawn(blocking => 0) };
    like($@, qr{^\QAlready have a blocking command pending at },
         "Test duplicate block");
    # Ok, finally the real point: cause a die with a pending blocking command
    die "Killer\n";
});
eval { $client->version };
is($@, "Killer\n", "Expected error exit");
is($client->version, 39, "We can still do blocking commands");

# Cause errors during the PROCESS callback
ADB::Client->add_command([version0 => "host:version", -1, EXPECT_EOF, sub { return {} }]);
eval { ADB::Client->add_command([version0 => "host:version", -1, EXPECT_EOF, sub { return {} }]) };
like($@, qr{^Attempt to redefine already existing command 'version0' at },
     "Cannot create command twice");
eval { $client->version0 };
like($@, qr{^Fatal: Assertion: Could not process host:version output: Neither a string nor an ARRAY reference at}, "process must not return HASH");
# This broke $client. Create a new one
$client = new_ok("ADB::Client", [port => $port]);

eval {
    local @ADB::Client::Ref::BUILTINS = [foo => "foo", -1, SERIAL];
    ADB::Client->add_commands;
};
like($@, qr{^No : in command 'foo' at },
     "Cannot create a serial command without :");

eval { ADB::Client->add_command(["foo"]) };
like($@, qr{^No COMMAND in command 'foo' at },
     "Cannot create a normal command without actually having an ADB command");

# Try an out of order wait
$client2->marker(callback => sub {
    eval { $client->client_ref->wait };
    like($@,
         qr{^\QFatal: Assertion: Already have a blocking command pending at },
         "Test out of order wait");
});
eval { $client->version };
like($@,
     qr{^\QFatal: Assertion: ADB::Client is dead but something caught the exception at },
     "We very much confused ADB::Client internals");
# This broke $client. Create a new one
$client = new_ok("ADB::Client", [port => $port]);

ADB::Client->add_command([version1 => "host:version", -1, EXPECT_EOF, sub { die "Boem\n" }]);
eval { $client->version1 };
like($@, qr{Assertion: Could not process host:version output "0027": Boem at },
     "process must not die");

ADB::Client->add_command([version2 => "host:version", -1, EXPECT_EOF, sub { return "Bad" }]);
my @result;
$client->version2(blocking => 0, callback => sub { shift; push @result, [@_] });
mainloop();
is_deeply(\@result, [["Bad"]], "Process can return an error");
eval { $client->version2 };
like($@, qr{^\QBad at }, "Blocking bad process");

# Add a command that is invalid as a perl identifier
eval { ADB::Client->add_command(["space space" => "Wee", 0, EXPECT_EOF]) };
like($@, qr{^\QCommand_name 'space space' is invalid as a perl identifier at },
     "Cannot use invalid perl identifiers");
eval { ADB::Client->add_command(["0space" => "Wee", 0, EXPECT_EOF]) };
like($@, qr{^\QCommand_name '0space' is invalid as a perl identifier at },
     "Cannot use invalid perl identifiers");
eval { ADB::Client->add_command(["space" => "%%", 0, EXPECT_EOF]) };
like($@, qr{^\QInvalid format in command 'space': %% at },
     "Cannot use invalid perl identifiers");

# Check some bad ways to call autogenerated commands
# Don't define new commands below this point
my $index = -1;
while (1) {
    my ($command_name, $nr_vars, $special) = eval { ADB::Client::Ref->command_get(++$index) } or last;
    # diag("$command_name $nr_vars $special");
    my $code = ADB::Client->can($command_name);
    ok($code, "Command $command_name exists");

    eval { $code->() };
    like($@, qr{^\QToo few arguments at},
         "$command_name too few arguments error");

    eval { $code->($client, 1..$nr_vars, "Foo") };
    like($@, qr{^\QOdd number of arguments at},
         "$command_name odd arguments error");

    $client->blocking(0);
    eval { $code->($client, 1..$nr_vars, Foo => 5) };
    like($@, qr{^\QUnknown argument Foo at},
         "$command_name unknown argument error");
    is(@{[$code->($client, 1..$nr_vars)]}, 0,
       "Can submit non blocking command and it returns nothing");

    $client->blocking(1);
    eval { $code->("ADB::Client", 1..$nr_vars, Foo => 5) };
    like($@, qr{^\QUnknown argument Foo at},
         "$command_name unknown argument error");
}
cmp_ok($index, ">", 0, "Discovered at least one command");

# Try Command with odd number of arguments
eval { ADB::Client::Command->new("Wee") };
like($@, qr{^Odd nuber of arguments at },
     "ADB::Client::Command->new takes an even number of arguments");

# Try some bad ADB::Client->new
eval { ADB::Client->new(undef) };
like($@, qr{^Odd number of arguments at }, "");

eval { ADB::Client->new(model => bless [], "Foo") };
like($@, qr{^Model without client_ref at }, "");

# Try some bad ADB::Client::Ref commands
$client = new_ok("ADB::Client", [port => $rport, blocking => 0]);

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

eval { $client->_connect(foo => 9) };
like($@, qr{^Unknown argument foo at }, "Expected error from connect");
eval { $client->client_ref->_connect({}, undef, 1000000) };
like($@, qr{^No command at index '1000000' at },
     "Expected error from version");

eval { $client->spawn(foo => 9) };
like($@, qr{^Unknown argument foo at }, "Expected error from spawn");
eval { $client->client_ref->spawn({}, undef, 1000000) };
like($@, qr{^No command at index '1000000' at },
     "Expected error from version");

eval { $client->echo("a" x 1e5, blocking => 1) };
like($@, qr{^Command too long: "internal:echo:a+"\.\.\. at },
     "Expected error from version");

eval { $client->_transport("a" x 1e5, blocking => 1) };
like($@, qr{^Command too long: "host:transport-a+"\.\.\. at },
     "Expected error from version");

is($client->blocking, 0, "Client is currently not blocking");
is($client->blocking(5), 0, "Client was not blocking");
is($client->blocking, 1, "Client is currently blocking");
is($client->blocking(undef), 1, "Client was blocking");
is($client->blocking, 0, "Client is currently not blocking");
is($client->blocking(5), 0, "Client was not blocking");
is($client->blocking(5), 1, "Client was (and still is) blocking");
is($client->blocking, 1, "Client is currently blocking");

is($client->resolve(port => 1, host => "127.0.0.2"), undef,
   "Can set to nonsense host/port");
is($client->host, "127.0.0.2", "Host was set");
is($client->port, 1, "Port was set");
is($client->resolve(host => undef, port => undef), undef,
   "Can revert to default host/port");
is($client->host, "127.0.0.1", "Host was set");
is($client->port, 5037, "Port was set");
{
    local $ADB::Client::Ref::ADB_HOST = "";
    local $ADB::Client::Ref::ADB_PORT = 0;

    is($client->resolve(host => undef, port => undef,
                        addr_info => addr_info("1.2.3.4", 2)), undef,
       "Can revert to default host/port");
    is($client->host, "", "Host was set");
    is($client->port, 0, "Port was set");
}
eval { $client->client_ref->_resolve() };
like($@, qr{^Fatal: Assertion: No command at }, "Resolve without commands");
my $fatal_ref = $client->client_ref->{commands}[0]->command_ref;
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
my $command_ref = $retired->command_ref;
isa_ok($command_ref, "ARRAY", "Commandref is an ARRAY reference");
cmp_ok($command_ref, "!=", $fatal_ref, "Command ref is not FATAL");
is($retired->command_name, "resolve", "Can get command name");
$retired->command_ref($fatal_ref);
is($retired->command_ref, $fatal_ref, "Command ref is now FATAL");
$retired = undef;

eval { $client->client_ref->error("Boem") };
like($@, qr{^Fatal: Assertion: error without command at },
     "Expected error from version");
# This broke $client. Create a new one
$client = new_ok("ADB::Client", [port => $rport, blocking => 0]);

eval { $client->client_ref->success("Success") };
like($@, qr{^Fatal: Assertion: Success without command at },
     "Expected error from version");
# This broke $client. Create a new one
$client = new_ok("ADB::Client", [port => $rport, blocking => 0]);

$client->version;
eval { $client->client_ref->success("Success") };
like($@, qr{^Fatal: Assertion: Active during success at },
     "Expected error from version");
# This broke $client. Create a new one
$client = new_ok("ADB::Client", [port => $rport, blocking => 0]);

# Trigger some internal sanity checks
my $callback = $client->client_ref->callback_blocking;
eval { $callback->(ADB::Client->new) };
like($@, qr{^\QFatal: Assertion: No wait pending at },
     "callback blocking needs result to be set");
eval {
    my $client = ADB::Client->new;
    $client->client_ref->{result} = 5;
    $callback->($client);
};
like($@, qr{^\QFatal: Assertion: Result already set at }, "zz");
eval {
    my $client = ADB::Client->new;
    $client->version(blocking => 0);
    $client->client_ref->{result} = "";
    $callback->($client);
};
like($@, qr{^\QFatal: Assertion: We are not the final command at }, "zz");

# Finally trigger object count errors
$client2 = undef;
$client = undef;
@timers = ();

# Triggers callers
$ADB::Client::Command::DEBUG = 1;
$client = ADB::Client->new;
$ADB::Client::Command::DEBUG = 0;
$client = undef;

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
    # Avoid ADB::Client::Ref::delete creating a final FATAL command object
    *ADB::Client::Command::new        = sub {};
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
