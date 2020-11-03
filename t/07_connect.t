#!/usr/bin/perl -w
# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl 07_connect.t'
#########################
## no critic (UselessNoCritic MagicNumbers)

use strict;
use warnings;

our $VERSION = "1.000";

use FindBin qw($Bin);
use lib $Bin;
use Storable qw(dclone);

use Test::More tests => 541;
use TestDrive qw(adb_start adb_unacceptable adb_unreachable adb_version
                 adb_blackhole adb_closer dumper
                 $CONNECTION_TIMEOUT $UNREACHABLE);

# We already checked loading in 02_adb_client.t
use ADB::Client qw(mainloop);
use ADB::Client::Utils qw(addr_info clocktime_running);

my $port  = adb_start();
my $port2 = adb_version(10);
my $rport = adb_unreachable();
my $port_ssh = adb_blackhole("SSH-2.0-OpenSSH_8.4p1 Debian-2\n");
my $port_closer = adb_closer();

my (@results, $err, $result);
my $callback = sub { push @results, [shift->connected, @{dclone(\@_)}] };

# Connect to a server that sends a greeting. Keep open for later
my $client_ssh = new_ok("ADB::Client" =>
                        [host => "127.0.0.1", port => $port_ssh]);
$result = $client_ssh->connect;
isa_ok($result, "HASH", "Got a connection result from greeting server");

# Connect again to a server that sends a greeting. Keep open for later
my $client_ssh2 = new_ok("ADB::Client" =>
                        [host => "127.0.0.1", port => $port_ssh]);
$result = $client_ssh2->connect;
isa_ok($result, "HASH", "Got a connection result from greeting server");

# Connect to a server that closes the connection. Keep open for later
my $client_closer = new_ok("ADB::Client" =>
                        [host => "127.0.0.1", port => $port_closer]);
$result = $client_closer->connect;
isa_ok($result, "HASH", "Got a connection result from greeting server");

my $client = new_ok("ADB::Client" =>
                    [host => "127.0.0.1", port => $port, blocking => 0]);
is_deeply(\@results, [], "No results yet");
$client->marker(callback => $callback);
$client->connect(callback => $callback);
$client->marker(callback => $callback);
$client->version(callback => $callback);
$client->marker(callback => $callback);
my $transaction_timeout = clocktime_running();
mainloop();
$transaction_timeout = clocktime_running() - $transaction_timeout;
cmp_ok($transaction_timeout, ">", 0, "Transaction took time");
# diag($transaction_timeout);
$transaction_timeout = 2 * $transaction_timeout < 0.1 ?
    0.1 : 2 * $transaction_timeout;
$transaction_timeout = 0.1 if $transaction_timeout < 0.1;
# dumper(\@results);
is_deeply(\@results, [
    [ 0, undef ],
    [ 1, undef, {
        "bind_addr" => $results[1][2]{bind_addr},
        "bind_addr0" => $results[1][2]{bind_addr0},
        "bind_ip" => "127.0.0.1",
        "bind_port" => $port,
        "connect_addr" => $results[1][2]{connect_addr},
        "connect_ip" => "127.0.0.1",
        "connect_port" => $port,
        "connected" => $results[1][2]{connected},
        "family" => 2
    }],
    [ 1, undef ],
    [ 0, undef, 39 ],
    [ 0, undef ]
], "Expected connection results");
ok($results[1][2]{connected}, "Is connected after connect");

# Check that we can re-resolve
@results = ();
$client->version(callback => $callback);
$client->resolve(callback => $callback,
                 addr_info => addr_info("127.0.0.1", $port2));
$client->version(callback => $callback);
$client->forget(callback => $callback);
$client->version(callback => $callback);
is_deeply(\@results, [], "No results yet");
mainloop();
# dumper(\@results);
is_deeply(\@results, [
  [ 0, undef, 39 ],	# Connected to 39 server
  [ 0, undef ],		# Resolve
  [ 0, undef, 39 ],	# Still connected to 30 server since we are sticky
  [ 0, undef ],		# Forget
  [ 0, undef, 10 ]	# Now we connect to the new server
], "Check re-resolve results");

# Same thing, but re-resolv from client host/port
@results = ();
$client->version(callback => $callback);
$client->resolve(callback => $callback);
$client->version(callback => $callback);
$client->forget(callback => $callback);
$client->version(callback => $callback);
is_deeply(\@results, [], "No results yet");
mainloop();
# dumper(\@results);
is_deeply(\@results, [
  [ 0, undef, 10 ],	# Connected to 10 server
  [ 0, undef ],		# Resolve
  [ 0, undef, 10 ],	# Still connected to 10 sserver since we are sticky
  [ 0, undef ],		# Forget
  [ 0, undef, 39 ]	# Now we connect to the old server
], "Check re-resolve results");

# And again, but this time don't do the resolve ourselves
@results = ();
$client->version(callback => $callback);
$client->resolve(callback => $callback, host => "127.0.0.1", port => $port2);
$client->version(callback => $callback);
$client->forget(callback => $callback);
$client->version(callback => $callback);
is_deeply(\@results, [], "No results yet");
mainloop();
# dumper(\@results);
is_deeply(\@results, [
  [ 0, undef, 39 ],	# Connected to 39 server
  [ 0, undef ],		# Resolve
  [ 0, undef, 39 ],	# Still connected to 30 server since we are sticky
  [ 0, undef ],		# Forget
  [ 0, undef, 10 ]	# Now we connect to the new server
], "Check re-resolve results");

# Now it's later. Check the closer and the greeter
isa_ok($client_closer->connect, "HASH",
       "We will reconnect an immediate closer");
eval { $client_closer->version };
$err = $@;
# The EOF can happen before or after we sent the command
# This depends on how fast the fake server is, so we can't predict without wait
like($err,
     qr{^Unexpected EOF while still writing "000Chost:version" to adb socket at |Unexpected EOF at },
     "But actual commands will fail with unexpected EOF");
eval { $client_ssh->connect };
$err = $@;
like($err,
     qr{^Response without having sent anything: "ssh-2\.0-openssh_8.4p1 debian-2\\n" at },
     "We can't reconnect a greeter");
eval { $client_ssh2->version };
$err = $@;
like($err,
     qr{^Response while command has not yet completed: "ssh-2\.0-openssh_8\.4p1 debian-2\\n" at },
     "We cant's end commands to a greeter");

# Connect to something that listens but does not accept
# (but the OS will still accept for you)
# So we only notice if we connect and it doesn't answer
@results = ();
my $aport = adb_unacceptable();
$client = new_ok("ADB::Client" =>
                    [host => "127.0.0.1", port => $aport, blocking => 0,
                     transaction_timeout => $transaction_timeout]);
$client->marker(callback => $callback);
$client->connect(callback => $callback);
$client->marker(callback => $callback);
# We can connect on an already connected client
$client->connect(callback => $callback);
$client->marker(callback => $callback);
$client->version(callback => $callback);
$client->marker(callback => $callback);
mainloop();
# dumper(\@results);
is_deeply(\@results, [
    [ 0, undef ],
    [ 1, undef, {
        "bind_addr" => $results[1][2]{bind_addr},
        "bind_addr0" => $results[1][2]{bind_addr0},
        "bind_ip" => "127.0.0.1",
        "bind_port" => $aport,
        "connect_addr" => $results[1][2]{connect_addr},
        "connect_ip" => "127.0.0.1",
        "connect_port" => $aport,
        "connected" => $results[1][2]{connected},
        "family" => 2
    }],
    [ 1, undef ],
    [ 1, undef, {
        "bind_addr" => $results[1][2]{bind_addr},
        "bind_addr0" => $results[1][2]{bind_addr0},
        "bind_ip" => "127.0.0.1",
        "bind_port" => $aport,
        "connect_addr" => $results[1][2]{connect_addr},
        "connect_ip" => "127.0.0.1",
        "connect_port" => $aport,
        "connected" => $results[1][2]{connected},
        "family" => 2
    }],
    [ 1, undef ],
    [ 0, "Operation timed out" ],
], "Expected connection results");
ok($results[1][2]{connected}, "Is connected after connect");

# Connect to something that listens but does not accept
# (but the OS will still accept for you)
@results = ();
$client = new_ok("ADB::Client" =>
                    [host => "127.0.0.1", port => $rport, blocking => 0]);
$client->marker(callback => $callback);
$client->connect(callback => $callback);
$client->marker(callback => $callback);
$client->version(callback => $callback);
$client->marker(callback => $callback);
mainloop();
#dumper(\@results);
is_deeply(\@results, [
    [ 0, undef ],
    [ 0, $results[1][1]]
], "Expected connection error");
like($results[1][1], qr{^ADB server 127\.0\.0\.1 port $rport: Connect error: },
    "Expected connection error");

# Connect to something that shouldn't answer
@results = ();
$client = new_ok("ADB::Client" =>
                    [host => $UNREACHABLE, blocking => 0,
                     connection_timeout => 0]);
$client->marker(callback => $callback);
$client->connect(callback => $callback);
$client->marker(callback => $callback);
mainloop();
# dumper(\@results);
is_deeply(\@results, [
    [ 0, undef ],
    [ 0, $results[1][1]]
], "Expected connection error");
like($results[1][1], qr{^ADB server 192\.0\.2\.1 port 5037: Connect error: },
    "Expected connection error");

# Try multiconnect, Several scenarios
# Index 0: First works
# Index 1: Second works
# Index 3: Third works

my @addr_info = (
    [@{addr_info("127.0.0.1", $port)},	# OK
     @{addr_info("127.0.0.1", $rport)},	# Refuse
     @{addr_info($UNREACHABLE, 5037)},  # Timeout
     @{addr_info("127.0.0.1", $port2)}],# OK that shoud not be reached

    [@{addr_info("127.0.0.1", $rport)},	# Refuse
     @{addr_info("127.0.0.1", $port)},	# OK
     @{addr_info($UNREACHABLE, 5037)},	# Timeout
     @{addr_info("127.0.0.1", $port2)}],# OK that shoud not be reached

    [@{addr_info("127.0.0.1", $rport)},	# Refuse
     @{addr_info($UNREACHABLE, 5037)},	# Timeout
     @{addr_info("127.0.0.1", $port)},	# OK
     @{addr_info("127.0.0.1", $port2)}],# OK that shoud not be reached
);
for my $addr_info (@addr_info) {
    for my $ai (@$addr_info) {
        $ai->{connection_timeout} = -1 if $ai->{connect_ip} eq $UNREACHABLE;
    }
}
# dumper(\@addr_info);
for my $i (0..2) {
    is(@{$addr_info[$i]}, 4, "combined addr_info has 4 elements");

    # direct: Immediately do a command instead of doing an explicit connect
    for my $direct (0..1) {
        # dumper($addr_info[$i]);
        my $client = new_ok("ADB::Client" =>
                            [host => "127.0.0.1", port => 1,
                             addr_info => $addr_info[$i],
                             connection_timeout => $CONNECTION_TIMEOUT]);
        my $_addr_info = $client->_addr_info;
        cmp_ok($_addr_info, '!=', $addr_info[$i],
               "ADB::Client clones addr_info [$i, $direct]]");
        is_deeply($_addr_info, $addr_info[$i],
                  "addr_info clone is identical [$i, $direct]");

        # retry 0: Make initial connection
        # retry 1: Make a new connection on the same ADB::Client
        #          This should ONLY use the previously good ip/port
        # retry 2: Forget. Should be like initial again
        # retry 3: Kill the good server. This should make the connect fail
        for my $retry (0..3) {
            my $old_port = $port;
            if ($retry == 2) {
                is($client->forget, undef, "Can forget [$i, $direct, $retry]");
            } elsif ($retry == 3) {
                my $new_port = adb_version();
                # Now kill the good server
                $client->kill;
                for my $addr_info (@addr_info) {
                    my $replaces = 0;
                    for my $ai (@$addr_info) {
                        if ($ai->{connect_ip} eq "127.0.0.1" && $ai->{connect_port} == $port) {
                            $ai = addr_info("127.0.0.1", $new_port)->[0];
                            ++$replaces;
                        }
                    }
                    is($replaces, 1,
                       "Fixed up 1 address [$i, $direct, $retry]");
                }
                $port = $new_port;
            }
            my $result;
            if ($direct == 0) {
                $result = eval { $client->connect() };
                my $err = $@;
                if ($retry == 3) {
                    like($err, qr{^ADB server 127\.0\.0\.1 port $old_port: Connect error: },
                         "Connection error [$i, $direct, $retry]");
                } else {
                    is($err, "", "No connection error [$i, $direct, $retry]");
                    my $r = $client->connection_data;
                    is_deeply($r, $result,
                              "connection_data corresponds to connection result [$i, $direct, $retry]");
                    cmp_ok($r, '!=', $result,
                           "connection_data is cloned [$i, $direct, $retry]");
                }
            } else {
                my $r = eval { $client->version() };
                my $err = $@;
                if ($retry == 3) {
                    like($err,
                         qr{^ADB server 127\.0\.0\.1 port $old_port: Connect error: },
                         "Connection error [$i, $direct, $retry]");
                } else {
                    is($err, "", "No connection error [$i, $direct, $retry]");
                    is($r, 39, "Expected version [$i, $direct, $retry]");
                }
            }
            $result ||= $client->connection_data;
            is_deeply($result, {
                "bind_addr" => $result->{bind_addr},
                "bind_addr0" => $result->{bind_addr0},
                "bind_ip" => "127.0.0.1",
                "bind_port" => $old_port,
                "connect_addr" => $result->{connect_addr},
                "connect_ip" => "127.0.0.1",
                "connect_port" => $old_port,
                "connected" => $result->{connected},
                "family" => 2,
                $retry == 1 || $retry == 2 ? (last_connect_error => "Unprobed") :
                $retry == 3 ? (last_connect_error => $result->{last_connect_error}) :
                (),
            }, "Got through set and connected [$i, $direct, $retry]");
            if ($retry == 3) {
                like($result->{last_connect_error}, qr{^Connect error: },
                   "Some sort of connect error [$i, $direct, $retry]");
                is($result->{connected}, undef,
                       "Connection state turned off [$i, $direct, $retry]");
            } else {
                cmp_ok($result->{connected}, '>', 0,
                       "Proper connect time [$i, $direct, $retry]");
            }
            cmp_ok($_addr_info->[$i], "!=", $result,
                   "Identify accepted connection is a clone [$i, $direct, $retry]");
            is_deeply($_addr_info->[$i], $result,
                   "Identify accepted connection [$i, $direct, $retry]");
            for my $j (0..$i-1) {
                ok(exists $_addr_info->[$j]{connected},
                   "Connection tried [$i, $direct, $retry, $j]");
                is($_addr_info->[$j]{connected}, undef,
                   "Connection failed [$i, $direct, $retry, $j]");
                ok(exists $_addr_info->[$j]{last_connect_error},
                   "Connection tried [$i, $direct, $retry, $j]");
                ok($_addr_info->[$j]{last_connect_error},
                   "Connection failed [$i, $direct, $retry, $j]");
                if ($retry == 0 || $retry == 2) {
                    isnt($_addr_info->[$j]{last_connect_error}, "Unprobed",
                         "Connection tried [$i, $direct, $retry, $j]");
                } else {
                    is($_addr_info->[$j]{last_connect_error}, "Unprobed",
                       "Connection not tried [$i, $direct, $retry, $j]");
                }
            }
            for my $j ($i+1..$#{$addr_info[$i]}) {
                ok(!exists $_addr_info->[$j]{connected},
                   "Connection not tried [$i, $direct, $retry, $j]");
                if ($retry == 0) {
                    ok(!exists $_addr_info->[$j]{last_connect_error},
                       "Connection not tried [$i, $direct, $retry, $j]");
                } else {
                    is($_addr_info->[$j]{last_connect_error}, "Unprobed",
                       "Connection not tried [$i, $direct, $retry, $j]");
                }
            }
            is_deeply($_addr_info, $client->addr_info,
                      "_addr_info tracks addr_info [$i, $direct, $retry]");
            $client->close;
            # dumper($_addr_info);

            # Make sure we notice connection attempts
            for my $ai (@$_addr_info) {
                $ai->{last_connect_error} = "Unprobed";
            }
        }
    }
}

# Completely failing set of addr info
# We expect this to try all adresses on each connection attempt and to keep
# failing
my $addr_info = [
     @{addr_info("127.0.0.1", $rport)},	# Refuse
     @{addr_info($UNREACHABLE, 5037)},  # Timeout
];
for my $ai (@$addr_info) {
    $ai->{connection_timeout} = -1 if $ai->{connect_ip} eq $UNREACHABLE;
}
for my $direct (0..1) {
    # dumper($addr_info[$i]);
    my $client = new_ok("ADB::Client" =>
                        [host => "127.0.0.1", port => 1,
                         addr_info => $addr_info,
                         connection_timeout => $CONNECTION_TIMEOUT]);
    my $_addr_info = $client->_addr_info;

    # direct: Immediately do a command instead of doing an explicit connect
    for my $retry (0..2) {
        if ($direct == 0) {
            my $dummy = eval { $client->connect() };
            my $err = $@;
            like($err, qr{^ADB server 127\.0\.0\.1 port $rport: Connect error: },
                 "Connection error [$direct, $retry]");
        } else {
            my $dummy = eval { $client->version() };
            my $err = $@;
            like($err, qr{^ADB server 127\.0\.0\.1 port $rport: Connect error: },
                 "Connection error [$direct, $retry]");
        }
        # dumper($_addr_info);
        for my $ai (@$_addr_info) {
            like($ai->{last_connect_error}, qr{^Connect error: },
                 "Expected connection error [$direct, $retry]");
            ok(exists $ai->{connected},
                 "Have connected status [$direct, $retry]");
            is($ai->{connected}, undef,
                 "Have unconnected status [$direct, $retry]");
            # Make sure we notice connection attempts
            $ai->{last_connect_error} = "Unprobed";
        }
    }
}
