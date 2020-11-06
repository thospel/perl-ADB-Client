#!/usr/bin/perl -w
# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl 08_spawn.t'
#########################
## no critic (UselessNoCritic MagicNumbers)

use strict;
use warnings;

our $VERSION = "1.000";

use FindBin qw($Bin);
use lib $Bin;
use Test::More tests => 138;
use TestDrive qw(adb_start adb_version adb_unreachable remote_ip is_remote4
                 addr_filter dumper);

# We already checked loading in 02_adb_client.t
use ADB::Client qw(mainloop);
use ADB::Client::Utils qw(addr_info);

my ($addr_info, $client, @result, $_addr_info);

# Keep this default one alive
my $port = adb_start();
# These you are allowed to kill
my $port_10 = adb_version(10);
my $port_20 = adb_version(20);
# Occupy a port that rejects connections
my $rport = adb_unreachable();

my $example_host = "www.example.com";
my $example_ip = eval { is_remote4($example_host) };
my $remote_ip = remote_ip();

# Cannot bind to a remote IP
$addr_info =
    [@{addr_info($remote_ip, 1)},	# Remote
     @{addr_info("127.0.0.1", $port)},	# OK
     ];
is(@$addr_info, 2, "Two addresses");

$client = new_ok("ADB::Client" =>
                    [host => "127.0.0.1", port => $port,
                     addr_info => $addr_info, blocking => 1]);
@result = eval { $client->spawn() };
like($@, qr{^\QCould not bind to $remote_ip (127.0.0.1): }, "Cannot bind to remote ip");

# Cannot bind to an already connected remote IP
SKIP: {
    skip "Cannot resolve $example_host", 2 unless defined $example_ip;
    # Use a pretty low timeout. It's more important that I can run this
    # test on my development system than that the user can
    # (with the default timeout adb_fake will autoquit in the mean time)
    my $client = new_ok("ADB::Client" =>
                        [host => $example_ip, port => 80,
                         connection_timeout => 0.5, blocking => 1]);
    eval { $client->connect() };
  SKIP: {
        skip $@, 1 if $@;
        eval { $client->spawn() };
        like($@, qr{^\QCould not bind to $example_ip ($example_ip): },
         "Can connect but not bind to $example_host ($example_ip)");
    }
}

$addr_info =
    [@{addr_info(undef, $port_10)},	# OK, version 10
     @{addr_info(undef, $port_20)},	# OK, version 20
     ];

# Plain spawn. Everythig is occupied. Should just connect
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
            "family" => 2
        },
        {
            "bind_ip" => "127.0.0.1",
            "bind_port" => $port_20,
            "connect_ip" => "127.0.0.1",
            "connect_port" => $port_20,
            "family" => 2
        }
    ], "Expectted history");
    is_deeply($result[0], $_addr_info->[0], "Connect to first server");
    is($client->version, 10, "Expected version");
}

# Version scan without kill. Everything is occupied with bad versions
for my $pre_connect (0, 1) {
    $client = new_ok("ADB::Client" =>
                     [host => "127.0.0.1", port => $port,
                      addr_info => $addr_info, blocking => 1]);
    $client->connect if $pre_connect;
    @result = eval { $client->spawn(version_min => 40) };
    like($@, qr{^Version '10' is below '40' at },
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
                "family" => 2,
                "last_connect_error" => "Version '10' is below '40'",
                "version" => 10
            },
            {
                "bind_ip" => "127.0.0.1",
                "bind_port" => $port_20,
                "connect_ip" => "127.0.0.1",
                "connect_port" => $port_20,
                "family" => 2
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
                "family" => 2,
                "last_connect_error" => "Version '10' is below '40'",
                "version" => 10
            },
            {
                "bind_ip" => "127.0.0.1",
                "bind_port" => $port_20,
                "connect_ip" => "127.0.0.1",
                "connect_port" => $port_20,
                "connected" => 1,
                "family" => 2,
                "last_connect_error" => "Version '20' is below '40'",
                "version" => 20
            }
        ], "Expected history [$pre_connect]");
    }
    is($client->version, 10, "Expected version [$pre_connect]");
}

$addr_info =
    [@{addr_info(undef, $rport)}, # Reject connections
     @{addr_info(undef, $port_10)},	# OK, version 10
     @{addr_info(undef, $port_20)},	# OK, version 20
 ];

# Version scan without kill. Not everything is occupied and have no good version
$client = new_ok("ADB::Client" =>
                 [host => "127.0.0.1", port => $port,
                  addr_info => $addr_info, blocking => 1]);
@result = eval { $client->spawn(version_min => 25) };
is($@, "", "We can occupy the first connection");
is_deeply(\@result, [$client->connection_data], "We are connected");
$_addr_info = $client->_addr_info;
is_deeply(addr_filter($_addr_info), [
  {
    "bind_ip" => "127.0.0.1",
    "bind_port" => $rport,
    "connect_ip" => "127.0.0.1",
    "connect_port" => $rport,
    "connected" => 1,
    "family" => 2,
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
    "family" => 2,
    "last_connect_error" => "Version '10' is below '25'",
    "version" => 10,
  },
  {
    "bind_ip" => "127.0.0.1",
    "bind_port" => $port_20,
    "connect_ip" => "127.0.0.1",
    "connect_port" => $port_20,
    "connected" => 1,
    "family" => 2,
    "last_connect_error" => "Version '20' is below '25'",
    "version" => 20,
  }
], "Expected history") || dumper(addr_filter($_addr_info));
is($client->pid, $result[0]{pid}, "Proper pid");
is($client->version, 30, "Expected version");
is($client->kill, "", "Kill what we just created");

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
like($@, qr{^Version '10' is below '25' at },
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
    "family" => 2,
    "last_connect_error" => "Version '10' is below '25'",
    "version" => 10
  },
  {
    "bind_ip" => "127.0.0.1",
    "bind_port" => $rport,
    "connect_ip" => "127.0.0.1",
    "connect_port" => $rport,
    "connected" => undef,
    "family" => 2,
    "last_connect_error" => "Connect error: Connection refused"
  },
  {
    "bind_ip" => "127.0.0.1",
    "bind_port" => $port_20,
    "connect_ip" => "127.0.0.1",
    "connect_port" => $port_20,
    "connected" => 1,
    "family" => 2,
    "last_connect_error" => "Version '20' is below '25'",
    "version" => 20
  }
], "Expected history") || dumper(addr_filter($_addr_info));
is($client->version, 10, "Expected version");

# Version scan without kill. Not everything is occupied but have a good version
for my $pre_connect (0, 1) {
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
            "family" => 2,
            "last_connect_error" => "Version '10' is below '15'",
            "version" => 10
        },
        {
            "bind_ip" => "127.0.0.1",
            "bind_port" => $rport,
            "connect_ip" => "127.0.0.1",
            "connect_port" => $rport,
            "connected" => undef,
            "family" => 2,
            "last_connect_error" => "Connect error: Connection refused"
        },
        {
            "bind_ip" => "127.0.0.1",
            "bind_port" => $port_20,
            "connect_ip" => "127.0.0.1",
            "connect_port" => $port_20,
            "connected" => 1,
            "family" => 2,
            "version" => 20
        }
    ], "Expected connection history") || dumper(addr_filter($_addr_info));
}

for my $pre_connect (0, 1) {
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
            "family" => 2,
            "last_connect_error" => "Version '10' is below '15'",
            "version" => 10
        },
        {
            "bind_ip" => "127.0.0.1",
            "bind_port" => $rport,
            "connect_ip" => "127.0.0.1",
            "connect_port" => $rport,
            "connected" => undef,
            "family" => 2,
            "last_connect_error" => "Connect error: Connection refused"
        },
        {
            "bind_ip" => "127.0.0.1",
            "bind_port" => $port_20,
            "connect_ip" => "127.0.0.1",
            "connect_port" => $port_20,
            "connected" => 1,
            "family" => 2,
            "version" => 20
        }
    ], "Expected connection history") || dumper(addr_filter($_addr_info));
}

# Version scan with kill. Not everything is occupied but only bad version
# Spawned adb is good
for my $pre_connect (0, 1) {
    $client = new_ok("ADB::Client" =>
                     [host => "127.0.0.1", port => $port,
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
            "family" => 2,
            "last_connect_error" => "Version '10' is below '25'",
            "pid" => 1,
            "version" => 30
        },
        {
            "bind_ip" => "127.0.0.1",
            "bind_port" => $rport,
            "connect_ip" => "127.0.0.1",
            "connect_port" => $rport,
            "family" => 2,
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
            "family" => 2,
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
        [@{addr_info(undef, $port_10)},	# OK, version 10
         @{addr_info(undef, $rport)},	# Reject connections
         @{addr_info(undef, $port_20)},	# OK, version 20
     ];
}

# Version scan with kill. Not everything is occupied but only bad version
# Spawned adb is bad
for my $pre_connect (0, 1) {
    $client = new_ok("ADB::Client" =>
                     [host => "127.0.0.1", port => $port,
                      addr_info => $addr_info, blocking => 1]);
    $client->connect if $pre_connect;
    @result = eval { $client->spawn(version_min => 35, kill => 1) };
    like($@, qr{^Version '30' is below '35' at }, "Recheck after spawn failed");
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
            "family" => 2,
            "last_connect_error" => "Version '30' is below '35'",
            "pid" => 1,
            "version" => 30
        },
        {
            "bind_ip" => "127.0.0.1",
            "bind_port" => $rport,
            "connect_ip" => "127.0.0.1",
            "connect_port" => $rport,
            "family" => 2,
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
            "family" => 2,
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
        [@{addr_info(undef, $port_10)},	# OK, version 10
         @{addr_info(undef, $rport)},	# Reject connections
         @{addr_info(undef, $port_20)},	# OK, version 20
     ];
}

# Unconditional kill, no version check
for my $pre_connect (0, 1) {
    $client = new_ok("ADB::Client" =>
                     [host => "127.0.0.1", port => $port,
                      addr_info => $addr_info, blocking => 1]);
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
            "family" => 2,
            "pid" => 1,
        },
        {
            "bind_ip" => "127.0.0.1",
            "bind_port" => $rport,
            "connect_ip" => "127.0.0.1",
            "connect_port" => $rport,
            "family" => 2,
        },
        {
            "bind_ip" => "127.0.0.1",
            "bind_port" => $port_20,
            "connect_ip" => "127.0.0.1",
            "connect_port" => $port_20,
            "family" => 2,
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
        [@{addr_info(undef, $port_10)},	# OK, version 10
         @{addr_info(undef, $rport)},	# Reject connections
         @{addr_info(undef, $port_20)},	# OK, version 20
     ];
}
