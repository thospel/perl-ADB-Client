#!/usr/bin/perl -w
# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl 11_spawn.t'
#########################
## no critic (UselessNoCritic MagicNumbers)

use strict;
use warnings;

our $VERSION = "1.000";

use FindBin qw($Bin);
use lib $Bin;
use Socket qw(AF_INET);

use Test::More tests => 406;

use TestDrive qw(adb_start adb_version adb_unreachable addr_filter dumper unalarm);

# We already checked loading in 04_adb_client.t
use ADB::Client qw(mainloop $ADB);
use ADB::Client::Utils qw(addr_info);

my ($addr_info, $client, @result, $_addr_info, $callback,
    $spawns, $spawns0, $spawns1);

# Keep this default one alive
my $port = adb_start();
# These you are allowed to kill
my $port_10 = adb_version(10);
my $port_20 = adb_version(20);
# Occupy a port that rejects connections
my $rport = adb_unreachable();

$addr_info =
    [@{addr_info(undef, $port_10)},	# OK, version 10
     @{addr_info(undef, $port_20)},	# OK, version 20
     ];

# Plain spawn. Everything is occupied. Should just connect
for my $pre_connect (0, 1) {
    $client = new_ok("ADB::Client" =>
                     [host => "127.0.0.1", port => $port,
                      addr_info => $addr_info, blocking => 1]);
    $client->connect if $pre_connect;
    @result = eval { $client->spawn() };
    is($@, "", "Spawn simply connects");
    is_deeply(\@result, [$client->connection_data], "We are connected to anything");
    $_addr_info = $client->_addr_info;
    is_deeply(addr_filter($_addr_info), [
        {
            "bind_ip" => "127.0.0.1",
            "bind_port" => $port_10,
            "connect_ip" => "127.0.0.1",
            "connect_port" => $port_10,
            "connected" => 1,
            "family" => AF_INET,
        },
        {
            "bind_ip" => "127.0.0.1",
            "bind_port" => $port_20,
            "connect_ip" => "127.0.0.1",
            "connect_port" => $port_20,
            "family" => AF_INET,
        }
    ], "Expectted history");
    is_deeply($result[0], $_addr_info->[0], "Connect to first server");
    is($client->version, 10, "Expected version");
}

# Version scan without kill. Everything is occupied with bad versions
for my $pre_connect (0, 1) {
    unalarm();
    is(ADB::Client->new(host => "127.0.0.1", port => $port_10)->version, 10,
       "Keep adb_fake alive");

    $client = new_ok("ADB::Client" =>
                     [host => "127.0.0.1", port => $port,
                      addr_info => $addr_info, blocking => 1]);
    $client->connect if $pre_connect;
    @result = eval { $client->spawn(version_min => 40) };
    like($@, qr{^\QADB server 127.0.0.1 port $port_10: Version '10' is below '40' at },
         "No server has version_min [$pre_connect]");
    $_addr_info = $client->_addr_info;
    # dumper(addr_filter($_addr_info));
    if ($pre_connect) {
        is_deeply(addr_filter($_addr_info), [
            {
                "bind_ip" => "127.0.0.1",
                "bind_port" => $port_10,
                "connect_ip" => "127.0.0.1",
                "connect_port" => $port_10,
                "connected" => 1,
                "family" => AF_INET,
                "last_connect_error" => "Version '10' is below '40'",
                "version" => 10
            },
            {
                "bind_ip" => "127.0.0.1",
                "bind_port" => $port_20,
                "connect_ip" => "127.0.0.1",
                "connect_port" => $port_20,
                "family" => AF_INET,
            }
        ], "Expected history [$pre_connect]");
        is_deeply($client->connection_data, $_addr_info->[0], "We aren't connected to the first server");
    } else {
        is($client->connection_data, undef, "We aren't connected to anything [$pre_connect]");
        is_deeply(addr_filter($_addr_info), [
            {
                "bind_ip" => "127.0.0.1",
                "bind_port" => $port_10,
                "connect_ip" => "127.0.0.1",
                "connect_port" => $port_10,
                "connected" => 1,
                "family" => AF_INET,
                "last_connect_error" => "Version '10' is below '40'",
                "version" => 10
            },
            {
                "bind_ip" => "127.0.0.1",
                "bind_port" => $port_20,
                "connect_ip" => "127.0.0.1",
                "connect_port" => $port_20,
                "connected" => 1,
                "family" => AF_INET,
                "last_connect_error" => "Version '20' is below '40'",
                "version" => 20
            }
        ], "Expected history [$pre_connect]");
    }
    is($client->version, 10, "Expected version [$pre_connect]");
}

# Version scan without kill.
# Not everything is occupied and have no good version
# Use any address
$addr_info =
    [@{addr_info("0.0.0.0", $rport)}, 	# Reject connections
     @{addr_info(undef, $port_10)},	# OK, version 10
     @{addr_info("0.0.0.0", $port_20)},	# OK, version 20
 ];

for my $adb_socket (0, 1) {
    unalarm();
    is(ADB::Client->new(host => "127.0.0.1", port => $port_10)->version, 10,
       "Keep adb_fake alive");

    $client = new_ok("ADB::Client" =>
                     [host => "127.0.0.1", port => $port,
                      adb_socket => $adb_socket,
                      addr_info => $addr_info, blocking => 1]);
    @result = eval { $client->spawn(version_min => 25) };
    is($@, "", "We can occupy the first connection [$adb_socket]");
    is_deeply(\@result, [$client->connection_data],
              "We are connected [$adb_socket]");
    $_addr_info = $client->_addr_info;
    is_deeply(addr_filter($_addr_info), [
        {
            "bind_ip" => "0.0.0.0",
            "bind_port" => $rport,
            "connect_ip" => "127.0.0.1",
            "connect_port" => $rport,
            "connected" => 1,
            "family" => AF_INET,
            "last_connect_error" => "Connect error: Connection refused",
            "pid" => 1,
            "version" => 30,
        },
        {
            "bind_ip" => "127.0.0.1",
            "bind_port" => $port_10,
            "connect_ip" => "127.0.0.1",
            "connect_port" => $port_10,
            "connected" => 1,
            "family" => AF_INET,
            "last_connect_error" => "Version '10' is below '25'",
            "version" => 10,
        },
        {
            "bind_ip" => "0.0.0.0",
            "bind_port" => $port_20,
            "connect_ip" => "127.0.0.1",
            "connect_port" => $port_20,
            "connected" => 1,
            "family" => AF_INET,
            "last_connect_error" => "Version '20' is below '25'",
            "version" => 20,
        }
    ], "Expected history [$adb_socket]") || dumper(addr_filter($_addr_info));
    is($client->pid, $result[0]{pid}, "Proper pid [$adb_socket]");
    is($client->version, 30, "Expected version [$adb_socket]");
    my $argv = join(" ", $client->argv);
    like($argv,
         $adb_socket ?
         qr{^-L acceptfd:\d+ fork-server server --reply-fd \d+\z}a :
         qr{^-a -L tcp:$rport fork-server server --reply-fd \d+\z}a,
         "Expected commandline");

    is($client->kill, "", "Kill what we just created [$adb_socket]");
}

$addr_info =
    [@{addr_info(undef, $rport)}, 	# Reject connections
     @{addr_info(undef, $port_10)},	# OK, version 10
     @{addr_info(undef, $port_20)},	# OK, version 20
 ];

for my $adb_socket (0, 1) {
    unalarm();
    is(ADB::Client->new(host => "127.0.0.1", port => $port_10)->version, 10,
       "Keep adb_fake alive");

    # Version scan without kill.
    # Not everything is occupied and have no good version
    $client = new_ok("ADB::Client" =>
                     [host => "127.0.0.1", port => $port,
                      adb_socket => $adb_socket,
                      addr_info => $addr_info, blocking => 1]);
    @result = eval { $client->spawn(version_min => 25) };
    is($@, "", "We can occupy the first connection [$adb_socket]");
    is_deeply(\@result, [$client->connection_data],
              "We are connected [$adb_socket]");
    $_addr_info = $client->_addr_info;
    is_deeply(addr_filter($_addr_info), [
        {
            "bind_ip" => "127.0.0.1",
            "bind_port" => $rport,
            "connect_ip" => "127.0.0.1",
            "connect_port" => $rport,
            "connected" => 1,
            "family" => AF_INET,
            "last_connect_error" => "Connect error: Connection refused",
            "pid" => 1,
            "version" => 30,
        },
        {
            "bind_ip" => "127.0.0.1",
            "bind_port" => $port_10,
            "connect_ip" => "127.0.0.1",
            "connect_port" => $port_10,
            "connected" => 1,
            "family" => AF_INET,
            "last_connect_error" => "Version '10' is below '25'",
            "version" => 10,
        },
        {
            "bind_ip" => "127.0.0.1",
            "bind_port" => $port_20,
            "connect_ip" => "127.0.0.1",
            "connect_port" => $port_20,
            "connected" => 1,
            "family" => AF_INET,
            "last_connect_error" => "Version '20' is below '25'",
            "version" => 20,
        }
    ], "Expected history [$adb_socket]") || dumper(addr_filter($_addr_info));
    is($client->pid, $result[0]{pid}, "Proper pid [$adb_socket]");
    is($client->version, 30, "Expected version [$adb_socket]");
    my $argv = join(" ", $client->argv);
    like($argv,
         $adb_socket ?
         qr{^-L acceptfd:\d+ fork-server server --reply-fd \d+\z}a :
         qr{^-L tcp:$rport fork-server server --reply-fd \d+\z}a,
         "Expected commandline");

    is($client->kill, "", "Kill what we just created [$adb_socket]");
}

$addr_info =
    [@{addr_info(undef, $port_10)},	# OK, version 10
     @{addr_info(undef, $rport)},	# Reject connections
     @{addr_info(undef, $port_20)},	# OK, version 20
     ];

# Version scan without kill. Not everything is occupied and have no good version
$client = new_ok("ADB::Client" =>
                 [host => "127.0.0.1", port => $port,
                  addr_info => $addr_info, blocking => 1]);
@result = eval { $client->spawn(version_min => 25) };
like($@, qr{^\QADB server 127.0.0.1 port $port_10: Version '10' is below '25' at },
     "Error reported on first connection");
is($client->connection_data, undef, "We are not connected");
$_addr_info = $client->_addr_info;
is_deeply(addr_filter($_addr_info), [
  {
    "bind_ip" => "127.0.0.1",
    "bind_port" => $port_10,
    "connect_ip" => "127.0.0.1",
    "connect_port" => $port_10,
    "connected" => 1,
    "family" => AF_INET,
    "last_connect_error" => "Version '10' is below '25'",
    "version" => 10
  },
  {
    "bind_ip" => "127.0.0.1",
    "bind_port" => $rport,
    "connect_ip" => "127.0.0.1",
    "connect_port" => $rport,
    "connected" => undef,
    "family" => AF_INET,
    "last_connect_error" => "Connect error: Connection refused"
  },
  {
    "bind_ip" => "127.0.0.1",
    "bind_port" => $port_20,
    "connect_ip" => "127.0.0.1",
    "connect_port" => $port_20,
    "connected" => 1,
    "family" => AF_INET,
    "last_connect_error" => "Version '20' is below '25'",
    "version" => 20
  }
], "Expected history") || dumper(addr_filter($_addr_info));
is($client->version, 10, "Expected version");

# Version scan without kill. Not everything is occupied but have a good version
for my $pre_connect (0, 1) {
    unalarm();
    is(ADB::Client->new(host => "127.0.0.1", port => $port_10)->version, 10,
       "Keep adb_fake alive");

    $client = new_ok("ADB::Client" =>
                     [host => "127.0.0.1", port => $port,
                      addr_info => $addr_info, blocking => 1]);
    $client->connect(version_min => 15) if $pre_connect;
    @result = eval { $client->spawn(version_min => 15) };
    is($@, "", "No error");
    ok($client->connection_data, "We are connected");
    is_deeply(\@result, [$client->connection_data], "Consistent connection_data");
    is($client->version, 20, "Expected version");
    $_addr_info = $client->_addr_info;
    is_deeply(addr_filter($_addr_info), [
        {
            "bind_ip" => "127.0.0.1",
            "bind_port" => $port_10,
            "connect_ip" => "127.0.0.1",
            "connect_port" => $port_10,
            "connected" => 1,
            "family" => AF_INET,
            "last_connect_error" => "Version '10' is below '15'",
            "version" => 10
        },
        {
            "bind_ip" => "127.0.0.1",
            "bind_port" => $rport,
            "connect_ip" => "127.0.0.1",
            "connect_port" => $rport,
            "connected" => undef,
            "family" => AF_INET,
            "last_connect_error" => "Connect error: Connection refused"
        },
        {
            "bind_ip" => "127.0.0.1",
            "bind_port" => $port_20,
            "connect_ip" => "127.0.0.1",
            "connect_port" => $port_20,
            "connected" => 1,
            "family" => AF_INET,
            "version" => 20
        }
    ], "Expected connection history") || dumper(addr_filter($_addr_info));
}

for my $pre_connect (0, 1) {
    unalarm();
    is(ADB::Client->new(host => "127.0.0.1", port => $port_10)->version, 10,
       "Keep adb_fake alive");

    # Version scan with kill. Not everything is occupied but have a good version
    $client = new_ok("ADB::Client" =>
                     [host => "127.0.0.1", port => $port,
                      addr_info => $addr_info, blocking => 1]);
    $client->connect(version_min => 15) if $pre_connect;
    @result = eval { $client->spawn(version_min => 15, kill => 1) };
    #system("netstat -nt | grep $port_10 1>&2");
    #diag("PORT: $port_10");
    is($@, "", "No error");
    ok($client->connection_data, "We are connected");
    is_deeply(\@result, [$client->connection_data], "Consistent connection_data");
    is($client->version, 20, "We did not do a kill");
    $_addr_info = $client->_addr_info;
    is_deeply(addr_filter($_addr_info), [
        {
            "bind_ip" => "127.0.0.1",
            "bind_port" => $port_10,
            "connect_ip" => "127.0.0.1",
            "connect_port" => $port_10,
            "connected" => 1,
            "family" => AF_INET,
            "last_connect_error" => "Version '10' is below '15'",
            "version" => 10
        },
        {
            "bind_ip" => "127.0.0.1",
            "bind_port" => $rport,
            "connect_ip" => "127.0.0.1",
            "connect_port" => $rport,
            "connected" => undef,
            "family" => AF_INET,
            "last_connect_error" => "Connect error: Connection refused"
        },
        {
            "bind_ip" => "127.0.0.1",
            "bind_port" => $port_20,
            "connect_ip" => "127.0.0.1",
            "connect_port" => $port_20,
            "connected" => 1,
            "family" => AF_INET,
            "version" => 20
        }
    ], "Expected connection history") || dumper(addr_filter($_addr_info));
}

# Version scan with kill. Not everything is occupied but only bad version
# Spawned adb is good
for my $adb_socket (0, 1) {
    for my $pre_connect (0, 1) {
        unalarm();
        is(ADB::Client->new(host => "127.0.0.1", port => $port_10)->version, 10,
           "Keep adb_fake alive");

        $client = new_ok("ADB::Client" =>
                         [host => "127.0.0.1", port => $port,
                          adb_socket => $adb_socket,
                          addr_info => $addr_info, blocking => 1]);
        $client->connect if $pre_connect;
        @result = eval { $client->spawn(version_min => 25, kill => 1) };
        is($@, "", "No error");
        ok($client->connection_data, "We are connected [$pre_connect]");
        $_addr_info = $client->addr_info;
        is_deeply(addr_filter($_addr_info), [
            {
                "bind_ip" => "127.0.0.1",
                "bind_port" => $port_10,
                "connect_ip" => "127.0.0.1",
                "connect_port" => $port_10,
                "connected" => 1,
                "family" => AF_INET,
                "last_connect_error" => "Version '10' is below '25'",
                "pid" => 1,
                "version" => 30
            },
            {
                "bind_ip" => "127.0.0.1",
                "bind_port" => $rport,
                "connect_ip" => "127.0.0.1",
                "connect_port" => $rport,
                "family" => AF_INET,
                $pre_connect ? () : (
                    "connected" => undef,
                    "last_connect_error" => "Connect error: Connection refused"
                ),
            },
            {
                "bind_ip" => "127.0.0.1",
                "bind_port" => $port_20,
                "connect_ip" => "127.0.0.1",
                "connect_port" => $port_20,
                "family" => AF_INET,
                $pre_connect ? () : (
                    "connected" => 1,
                    "last_connect_error" => "Version '20' is below '25'",
                    "version" => 20
                ),
            }
        ], "Expected connection history [$pre_connect]") ||
            dumper(addr_filter($_addr_info));
        is_deeply(\@result, [$client->connection_data],
                  "Consistent connection_data [$pre_connect]");
        is($client->pid, $result[0]{pid}, "Proper pid [$pre_connect]");
        is($client->version, 30,
           "We are now talking to the spawned server [$pre_connect]");
        is($client->kill, "", "Kill what we just created [$pre_connect]");
        # Restore a listener for version 10
        $port_10 = adb_version(10);
        $addr_info =
            [@{addr_info(undef, $port_10)}, # OK, version 10
             @{addr_info(undef, $rport)},   # Reject connections
             @{addr_info(undef, $port_20)}, # OK, version 20
         ];
    }
}

# Version scan with kill. Not everything is occupied but only bad version
# Spawned adb is bad
for my $adb_socket (0, 1) {
    for my $pre_connect (0, 1) {
        unalarm();
        is(ADB::Client->new(host => "127.0.0.1", port => $port_10)->version, 10,
           "Keep adb_fake alive");

        $client = new_ok("ADB::Client" =>
                         [host => "127.0.0.1", port => $port,
                          adb_socket => $adb_socket,
                          addr_info => $addr_info, blocking => 1]);
        $client->connect if $pre_connect;
        @result = eval { $client->spawn(version_min => 35, kill => 1) };
        like($@, qr{^\QADB server 127.0.0.1 port $port_10: Version '30' is below '35' at }, "Recheck after spawn failed");
        $_addr_info = $client->addr_info;
        if ($pre_connect) {
            is_deeply($client->connection_data, $_addr_info->[0],
                      "We are connected to the first server [$pre_connect]");
        } else {
            is($client->connection_data, undef,
               "We are not connected [$pre_connect]");
        }
        is_deeply(addr_filter($_addr_info), [
            {
                "bind_ip" => "127.0.0.1",
                "bind_port" => $port_10,
                "connect_ip" => "127.0.0.1",
                "connect_port" => $port_10,
                "connected" => 1,
                "family" => AF_INET,
                "last_connect_error" => "Version '30' is below '35'",
                "pid" => 1,
                "version" => 30
            },
            {
                "bind_ip" => "127.0.0.1",
                "bind_port" => $rport,
                "connect_ip" => "127.0.0.1",
                "connect_port" => $rport,
                "family" => AF_INET,
                $pre_connect ? () : (
                    "connected" => undef,
                    "last_connect_error" => "Connect error: Connection refused"
                ),
            },
            {
                "bind_ip" => "127.0.0.1",
                "bind_port" => $port_20,
                "connect_ip" => "127.0.0.1",
                "connect_port" => $port_20,
                "family" => AF_INET,
                $pre_connect ? () : (
                    "connected" => 1,
                    "last_connect_error" => "Version '20' is below '35'",
                    "version" => 20
                ),
            }
        ], "Expected connection history [$pre_connect]") ||
            dumper(addr_filter($_addr_info));

        is($client->pid, $_addr_info->[0]{pid}, "Proper pid [$pre_connect]");
        is($client->version, 30,
           "We are now talking to the spawned server [$pre_connect]");
        is($client->kill, "", "Kill what we just created [$pre_connect]");
        # Restore a listener for version 10
        $port_10 = adb_version(10);
        $addr_info =
            [@{addr_info(undef, $port_10)}, # OK, version 10
             @{addr_info(undef, $rport)},   # Reject connections
             @{addr_info(undef, $port_20)}, # OK, version 20
         ];
    }
}

# Unconditional kill, no version check
for my $adb_socket (0, 1) {
    for my $pre_connect (0, 1) {
        unalarm();
        is(ADB::Client->new(host => "127.0.0.1", port => $port_10)->version, 10,
           "Keep adb_fake alive");

        $client = new_ok("ADB::Client" =>
                         [host => "127.0.0.1", port => $port, blocking => 1,
                          adb_socket => $adb_socket, addr_info => $addr_info]);
        $client->connect if $pre_connect;
        @result = eval { $client->spawn(kill => 1) };
        is($@, "", "Spawn after killing spree [$pre_connect]");
        is_deeply(\@result, [$client->connection_data],
                  "Expected result [$pre_connect]");
        $_addr_info = $client->addr_info;
        is_deeply($client->connection_data, $_addr_info->[0],
                  "We are connected to the first server [$pre_connect]");
        is_deeply(addr_filter($_addr_info), [
            {
                "bind_ip" => "127.0.0.1",
                "bind_port" => $port_10,
                "connect_ip" => "127.0.0.1",
                "connect_port" => $port_10,
                "connected" => 1,
                "family" => AF_INET,
                "pid" => 1,
            },
            {
                "bind_ip" => "127.0.0.1",
                "bind_port" => $rport,
                "connect_ip" => "127.0.0.1",
                "connect_port" => $rport,
                "family" => AF_INET,
            },
            {
                "bind_ip" => "127.0.0.1",
                "bind_port" => $port_20,
                "connect_ip" => "127.0.0.1",
                "connect_port" => $port_20,
                "family" => AF_INET,
            }
        ], "Expected connection history [$pre_connect]") ||
            dumper(addr_filter($_addr_info));

        is($client->pid, $_addr_info->[0]{pid}, "Proper pid [$pre_connect]");
        is($client->version, 30,
           "We are now talking to the spawned server [$pre_connect]");
        is($client->kill, "", "Kill what we just created [$pre_connect]");
        # Restore a listener for version 10
        $port_10 = adb_version(10);
        $addr_info =
            [@{addr_info(undef, $port_10)}, # OK, version 10
             @{addr_info(undef, $rport)},   # Reject connections
             @{addr_info(undef, $port_20)}, # OK, version 20
         ];
    }
}

for my $adb_socket (0, 1) {
    unalarm();
    is(ADB::Client->new(host => "127.0.0.1", port => $port_10)->version, 10,
       "Keep adb_fake alive");

    # Test a join
    local $ENV{ADB_FAKE_SLEEP} = 0.1;

    my $client1 = new_ok("ADB::Client" =>
                         [host => "127.0.0.1", port => $rport, blocking => 0,
                          adb_socket => -$adb_socket]);
    my $client2 = new_ok("ADB::Client" =>
                         [host => "127.0.0.1", port => $rport, blocking => 0,
                          adb_socket => $adb_socket ? "foo" : ""]);
    is ($client1->adb_socket, -$adb_socket, "Expected adb_socket");
    is ($client2->adb_socket, $adb_socket, "Expected adb_socket");
    $callback = sub { shift; push @result, [@_] };
    $spawns0 = ADB::Client::Spawn->spawns;
    @result = ();
    $client1->spawn(callback => $callback);
    $client2->spawn(callback => $callback);
    mainloop;
    $spawns1 = ADB::Client::Spawn->spawns;
    $spawns = $spawns1 - $spawns0;
    $spawns0 = $spawns1;
    for my $i (0,1) {
        is($result[$i][0], undef, "Spawn $i succeeded [$adb_socket]");
        my $addr = $result[$i][1];
        is_deeply(addr_filter($addr), {
            "bind_ip" => "127.0.0.1",
            "bind_port" => $rport,
            "connect_ip" => "127.0.0.1",
            "connect_port" => $rport,
            "connected" => 1,
            "family" => AF_INET,
            "last_connect_error" => "Connect error: Connection refused",
            "pid" => 1
        }, "Expected history [$adb_socket $i]") || dumper(addr_filter($addr));
    }
    is($spawns, 1, "Only one spawn [$adb_socket]");
    is($result[0][1]{pid}, $result[1][1]{pid}, "Equal pids [$adb_socket]");
    is($client1->kill(blocking => 1), "", "Kill new server [$adb_socket]");
    eval { $client2->kill(blocking => 1) };
    like($@,
         qr{^Unexpected EOF(?: while still writing "0009host:kill" to adb socket)? at },
         "Also kills it for the other client [$adb_socket]");

    # Test a join with different timeouts
    $client2 = new_ok("ADB::Client" =>
                      [host => "127.0.0.1", port => $rport,
                       adb_socket => -2*$adb_socket, blocking => 0]);
    $client1 = new_ok("ADB::Client" =>
                      [host => "127.0.0.1", port => $rport,
                       spawn_timeout => -1, blocking => 0,
                       adb_socket => $adb_socket ? "bar": undef]);
    is ($client1->adb_socket, $adb_socket, "Expected adb_socket");
    is ($client2->adb_socket, -$adb_socket, "Expected adb_socket");
    $callback = sub { shift; push @result, [@_] };
    $spawns0 = ADB::Client::Spawn->spawns;
    @result = ();
    $client1->spawn(callback => $callback);
    $client2->spawn(callback => $callback);
    mainloop;
    $spawns1 = ADB::Client::Spawn->spawns;
    $spawns = $spawns1 - $spawns0;
    $spawns0 = $spawns1;
    is(@result, 2, "Got two results [$adb_socket]");
    is_deeply($result[0],
              ["ADB server 127.0.0.1 port $rport: Spawn failed: Operation timed out"],
              "First server times out") || dumper(\@result);
    is(@{$result[1]}, 2, "Second server succeeds");
    is($result[1][0], undef, "Second spawn succeeded");
    is_deeply(addr_filter($result[1][1]), {
        "bind_ip" => "127.0.0.1",
        "bind_port" => $rport,
        "connect_ip" => "127.0.0.1",
        "connect_port" => $rport,
        "connected" => 1,
        "family" => AF_INET,
        "last_connect_error" => "Connect error: Connection refused",
        "pid" => 1
    }, "Expected history for second server") || dumper(addr_filter($result[1][1]));
    is($spawns, 1, "Only one spawn");
    is($client2->kill(blocking => 1), "", "Kill new server");
    # It is possible that even though the just spawned server exited
    # that a connect still succeeds and a write can be started which then
    # immediately gets a ECONNRESET read error (confirmed with strace)
    eval { $client1->kill(blocking => 1) };
    like($@,
         qr{^\QADB server 127.0.0.1 port $rport: Connect error: \E|Unexpected EOF at },
         "Also kills it for the other client");

    # Test spawn timeout (not client spawn timeout)
    $ENV{ADB_FAKE_SLEEP} = 5;
    local $ADB::Client::Spawn::ADB_SPAWN_TIMEOUT = 0.1;

    $client1 = new_ok("ADB::Client" =>
                         [host => "127.0.0.1", port => $rport, blocking => 0,
                          adb_socket => $adb_socket]);
    $client2 = new_ok("ADB::Client" =>
                         [host => "127.0.0.1", port => $rport, blocking => 0,
                          adb_socket => $adb_socket]);
    $callback = sub { shift; push @result, [@_] };
    $spawns0 = ADB::Client::Spawn->spawns;
    @result = ();
    $client1->spawn(callback => $callback);
    $client2->spawn(callback => $callback);
    mainloop;
    is_deeply(\@result, [
        [
            "ADB server 127.0.0.1 port $rport: Spawn failed: Killed unresponsive '$ADB' with SIGTERM"
        ],
        [
            "ADB server 127.0.0.1 port $rport: Spawn failed: Killed unresponsive '$ADB' with SIGTERM"
        ]
    ], "Expected history") || dumper(\@result);
    $spawns1 = ADB::Client::Spawn->spawns;
    $spawns = $spawns1 - $spawns0;
    $spawns0 = $spawns1;
    is($spawns, 1, "Only one spawn");
    is($result[0][1]{pid}, $result[1][1]{pid}, "Equal pids");
    my $kills = ADB::Client::Spawn->kills;
    is_deeply($kills, { "KILL" => 0, "TERM" => 1 }, "Did kill") ||
        dumper($kills);
    $kills->{KILL} = $kills->{TERM} = 0;

    # Test spawn timeout (not client spawn timeout) on program cactching TERM
    # This has a race condition. The SIGTERM signal can arrive before SIGTERM
    # has been set to ignore. Increase ADB_SPAWN_TIMEOUT a tiny bit
    $ADB::Client::Spawn::ADB_SPAWN_TIMEOUT = 0.2;

    $ENV{ADB_FAKE_SLEEP} = -5;
    local $ADB::Client::Spawn::SIGTERM_TIMEOUT = 0.1;

    $client1 = new_ok("ADB::Client" =>
                         [host => "127.0.0.1", port => $rport, blocking => 0]);
    $client2 = new_ok("ADB::Client" =>
                         [host => "127.0.0.1", port => $rport, blocking => 0]);
    $callback = sub { shift; push @result, [@_] };
    $spawns0 = ADB::Client::Spawn->spawns;
    @result = ();
    $client1->spawn(callback => $callback);
    $client2->spawn(callback => $callback);
    mainloop;
    is_deeply(\@result, [
        [
            "ADB server 127.0.0.1 port $rport: Spawn failed: Killed unresponsive '$ADB' with SIGKILL"
        ],
        [
            "ADB server 127.0.0.1 port $rport: Spawn failed: Killed unresponsive '$ADB' with SIGKILL"
        ]
    ], "Expected history") || dumper(\@result);
    $spawns1 = ADB::Client::Spawn->spawns;
    $spawns = $spawns1 - $spawns0;
    $spawns0 = $spawns1;
    is($spawns, 1, "Only one spawn");
    is($result[0][1]{pid}, $result[1][1]{pid}, "Equal pids");
    $kills = ADB::Client::Spawn->kills;
    is_deeply($kills, { "KILL" => 1, "TERM" => 1 }, "Did kill") ||
        dumper($kills);
    $kills->{KILL} = $kills->{TERM} = 0;
}

# Spawn errors
$ENV{ADB_FAKE_ERROR} = "Giving up\n";

$spawns0 = ADB::Client::Spawn->spawns;
for my $os ("$^O", "FleaBSD","stderr") {
    local $^O = $os;

    unalarm();
    is(ADB::Client->new(host => "127.0.0.1", port => $port_10)->version, 10,
       "Keep adb_fake alive");

    my $reason = $os eq "FleaBSD" ? "Reason unknown": "Giving up";
    $ENV{ANDROID_ADB_LOG_PATH} = 0 if $os eq "stderr";
    my $espawns = $os eq "linux" ? 2 : 1;

    # Basic fail on unoccupied port
    $addr_info =
        [@{addr_info(undef, $rport)}, # Reject connections
     ];
    $client = new_ok("ADB::Client" =>
                     [host => "127.0.0.1", port => $port,
                      addr_info => $addr_info, blocking => 1]);
    @result = eval { $client->spawn };
    like($@,
         qr{^\QADB server 127.0.0.1 port $rport: Spawn failed: Could not start '$ADB': $reason at },
         "We can but failed to occupy the first connection [$os]");
    $spawns1 = ADB::Client::Spawn->spawns;
    $spawns = $spawns1 - $spawns0;
    $spawns0 = $spawns1;
    is($spawns, $espawns, "Expected number of spawns [$os]");
    is($client->connection_data, undef, "We are not connected");
    $_addr_info = $client->_addr_info;
    is_deeply(addr_filter($_addr_info), [
        {
            "bind_ip" => "127.0.0.1",
            "bind_port" => $rport,
            "connect_ip" => "127.0.0.1",
            "connect_port" => $rport,
            "connected" => undef,
            "family" => AF_INET,
            "last_connect_error" => "Could not start '$ADB': $reason",
        }], "Expected history") || dumper(addr_filter($_addr_info));

    # Version scan without kill.
    # Not everything is occupied and have no good version
    # Spawn proceeds to existing connection
    $addr_info =
        [@{addr_info(undef, $rport)},   # Reject connections
         @{addr_info(undef, $port_10)},	# OK, version 10
         @{addr_info(undef, $port_20)},	# OK, version 20
     ];
    $client = new_ok("ADB::Client" =>
                     [host => "127.0.0.1", port => $port,
                      addr_info => $addr_info, blocking => 1]);
    @result = eval { $client->spawn(version_min => 25) };
    like($@,
         qr{^\QADB server 127.0.0.1 port $port_10: Version '10' is below '25' at },
         "We can but failed to occupy the first connection");
    $spawns1 = ADB::Client::Spawn->spawns;
    $spawns = $spawns1 - $spawns0;
    $spawns0 = $spawns1;
    is($spawns, $espawns, "Expected number of spawns [$os]");
    is($client->connection_data, undef, "We are not connected");
    $_addr_info = $client->_addr_info;
    is_deeply(addr_filter($_addr_info), [
        {
            "bind_ip" => "127.0.0.1",
            "bind_port" => $rport,
            "connect_ip" => "127.0.0.1",
            "connect_port" => $rport,
            "connected" => undef,
            "family" => AF_INET,
            "last_connect_error" => "Could not start '$ADB': $reason",
        },
        {
            "bind_ip" => "127.0.0.1",
            "bind_port" => $port_10,
            "connect_ip" => "127.0.0.1",
            "connect_port" => $port_10,
            "connected" => 1,
            "family" => AF_INET,
            "last_connect_error" => "Version '10' is below '25'",
            "version" => 10,
        },
        {
            "bind_ip" => "127.0.0.1",
            "bind_port" => $port_20,
            "connect_ip" => "127.0.0.1",
            "connect_port" => $port_20,
            "connected" => 1,
            "family" => AF_INET,
            "last_connect_error" => "Version '20' is below '25'",
            "version" => 20,
        }
    ], "Expected history") || dumper(addr_filter($_addr_info));
    is($client->version, 10, "Expected version");

    # Try to start a non-existing program
    # Version scan without kill.
    # Not everything is occupied and have no good version
    # Spawn proceeds to existing connection
    $addr_info =
        [@{addr_info("0.0.0.0", $rport)},   # Reject connections
         @{addr_info(undef, $port_10)},	# OK, version 10
         @{addr_info(undef, $port_20)},	# OK, version 20
     ];
    $client = new_ok("ADB::Client" =>
                     [host => "127.0.0.1", port => $port,
                      adb => "/non-existing",
                      addr_info => $addr_info, blocking => 1]);
    @result = eval { $client->spawn(version_min => 25) };
    like($@, qr{^\QADB server 0.0.0.0 port $rport: Spawn failed: Could not start '/non-existing': },
         "We can but failed to occupy the first connection");
    $spawns1 = ADB::Client::Spawn->spawns;
    $spawns = $spawns1 - $spawns0;
    $spawns0 = $spawns1;
    is($spawns, 1, "Expected number of spawns (no restarts) [$os]");
    is($client->connection_data, undef, "We are not connected");
    $_addr_info = $client->_addr_info;
    is_deeply(addr_filter($_addr_info), [
        {
            "bind_ip" => "0.0.0.0",
            "bind_port" => $rport,
            "connect_ip" => "127.0.0.1",
            "connect_port" => $rport,
            "connected" => undef,
            "family" => AF_INET,
            "last_connect_error" => "Could not start '/non-existing': No such file or directory",
        },
        {
            "bind_ip" => "127.0.0.1",
            "bind_port" => $port_10,
            "connect_ip" => "127.0.0.1",
            "connect_port" => $port_10,
            "connected" => 1,
            "family" => AF_INET,
            "last_connect_error" => "Version '10' is below '25'",
            "version" => 10,
        },
        {
            "bind_ip" => "127.0.0.1",
            "bind_port" => $port_20,
            "connect_ip" => "127.0.0.1",
            "connect_port" => $port_20,
            "connected" => 1,
            "family" => AF_INET,
            "last_connect_error" => "Version '20' is below '25'",
            "version" => 20,
        }
    ], "Expected history") || dumper(addr_filter($_addr_info));
    is($client->version, 10, "Expected version");

    # Version scan with kill.
    # Not everything is occupied and have no good version
    # Spawn proceeds all the way
    $addr_info =
        [@{addr_info(undef, $rport)},   # Reject connections
         @{addr_info(undef, $port_10)},	# OK, version 10
         @{addr_info(undef, $port_20)},	# OK, version 20
     ];
    $client = new_ok("ADB::Client" =>
                     [host => "127.0.0.1", port => $port,
                      addr_info => $addr_info, blocking => 1]);
    @result = eval { $client->spawn(version_min => 25, kill => 1) };
    like($@,
         qr{^\QADB server 127.0.0.1 port $port_10: Version '10' is below '25' at },
         "We can but failed to occupy the first connection");
    $spawns1 = ADB::Client::Spawn->spawns;
    $spawns = $spawns1 - $spawns0;
    $spawns0 = $spawns1;
    is($spawns, 3*$espawns, "Expected number of spawns [$os]");
    is($client->connection_data, undef, "We are not connected");
    $_addr_info = $client->_addr_info;
    is_deeply(addr_filter($_addr_info), [
        {
            "bind_ip" => "127.0.0.1",
            "bind_port" => $rport,
            "connect_ip" => "127.0.0.1",
            "connect_port" => $rport,
            "connected" => undef,
            "family" => AF_INET,
            "last_connect_error" => "Could not start '$ADB': $reason",
        },
        {
            "bind_ip" => "127.0.0.1",
            "bind_port" => $port_10,
            "connect_ip" => "127.0.0.1",
            "connect_port" => $port_10,
            "connected" => 1,
            "family" => AF_INET,
            "last_connect_error" => "Could not start '$ADB': $reason",
            "version" => 10,
        },
        {
            "bind_ip" => "127.0.0.1",
            "bind_port" => $port_20,
            "connect_ip" => "127.0.0.1",
            "connect_port" => $port_20,
            "connected" => 1,
            "family" => AF_INET,
            "last_connect_error" => "Could not start '$ADB': $reason",
            "version" => 20,
        }
    ], "Expected history") || dumper(addr_filter($_addr_info));

    # Repair
    $port_10 = adb_version(10);
    $port_20 = adb_version(20);

    # Test a bad join
    $ENV{ADB_FAKE_SLEEP} = 0.1;
    my $client1 = new_ok("ADB::Client" =>
                         [host => "127.0.0.1", port => $rport, blocking => 0]);
    my $client2 = new_ok("ADB::Client" =>
                         [host => "127.0.0.1", port => $rport, blocking => 0]);
    $callback = sub { shift; push @result, [@_] };
    $spawns0 = ADB::Client::Spawn->spawns;
    @result = ();
    $client1->spawn(callback => $callback);
    $client2->spawn(callback => $callback);
    mainloop;
    $spawns1 = ADB::Client::Spawn->spawns;
    $spawns = $spawns1 - $spawns0;
    $spawns0 = $spawns1;
    is_deeply(\@result, [
        [
            "ADB server 127.0.0.1 port $rport: Spawn failed: Could not start '$ADB': $reason"
        ],
        [
            "ADB server 127.0.0.1 port $rport: Spawn failed: Could not start '$ADB': $reason"
        ]
    ], "Both spawns failed [$os]") || dumper(\@result);
    is($spawns, $espawns, "Only one failure [$os]");
    $ENV{ADB_FAKE_SLEEP} = 0;

    # Cleanup ANDROID_ADB_LOG_PATH for the next commands
    delete $ENV{ANDROID_ADB_LOG_PATH};
}
$ENV{ADB_FAKE_ERROR} = "";
