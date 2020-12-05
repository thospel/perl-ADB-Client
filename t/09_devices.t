#!/usr/bin/perl -w
# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl 09_devices.t'
#########################
## no critic (UselessNoCritic MagicNumbers)
# Test some simple commands

use strict;
use warnings;

our $VERSION = "1.000";

use FindBin qw($Bin);
use lib $Bin;
use Test::More tests => 183;
use TestDrive qw(adb_start adb_version dumper);

# We already checked loading in 04_adb_client.t
use ADB::Client qw(mainloop);

# keep this one alive
my $port = adb_start();
# $port = 5037;
my $port_10 = adb_version(10);

my ($client, $client2, $client3, $client4, @result);
my %serial_result = (
    features => {
        usb => [{
            "cmd" => 1,
            "shell_v2" => 1,
            "stat_v2" => 1,
            "wabbits" => 1,
        }, [
            "shell_v2",
            "cmd",
            "stat_v2",
            "wabbits",
        ], "shell_v2,cmd,stat_v2,wabbits"],
        local => [
            {
                "abb" => 1,
                "abb_exec" => 1,
                "apex" => 1,
                "cmd" => 1,
                "fixed_push_mkdir" => 1,
                "fixed_push_symlink_timestamp" => 1,
                "shell_v2" => 1,
                "stat_v2" => 1,
                "zorro"	=> 1,
            },
            [
                "fixed_push_mkdir",
                "shell_v2",
                "apex",
                "stat_v2",
                "abb",
                "fixed_push_symlink_timestamp",
                "cmd",
                "abb_exec",
                "zorro",
            ],
            "fixed_push_mkdir,shell_v2,apex,stat_v2,abb,fixed_push_symlink_timestamp,cmd,abb_exec,zorro"
        ],
    },
    serial => {
        usb => ["52000c4748d6a283"],
        local => ["10.253.0.13:5555"],
    },
    state => {
        usb => ["device"],
        local => ["device"],
    },
    device_path => {
        usb => ["usb:1-1.2"],
        local => ["unknown"],
    },
);
my @serial_commands = sort keys %serial_result;

$client = new_ok("ADB::Client" => [host => "127.0.0.1", port => $port_10]);
$client2 = new_ok("ADB::Client" => [host => "127.0.0.1",
                                    port => $port_10, blocking => 0]);
$client3 = new_ok("ADB::Client" => [host => "127.0.0.1",
                                    port => $port_10, blocking => 0]);
my @result3;
my $callback3 = sub { shift; push @result3, [@_] };

$client4 = new_ok("ADB::Client" => [host => "127.0.0.1",
                                    port => $port_10, blocking => 0]);
my @result4;
my $callback4 = sub { shift; push @result4, [@_] };

# Test host:devices
@result = $client->devices;
is_deeply(\@result, [
    {
        "10.253.0.13:5555" => "device",
        "52000c4748d6a283" => "device"
    },
], "Expected devices result");

# Test host:devices-l
@result = $client->devices_long;
is_deeply(\@result, [
    {
        "10.253.0.13:5555" => {
            "device" => "zeroflte",
            "model" => "SM_G920F",
            "product" => "zerofltexx",
            "state" => "device",
            "transport_id" => 1
        },
        "52000c4748d6a283" => {
            "device" => "kminiltexx",
            "model" => "SM_G800F",
            "product" => "lineage_kminilte",
            "state" => "device",
            "transport_id" => 2,
            "usb" => "1-1.2"
        }
    }], "Expected devices long result") || dumper(\@result);

eval { $client->wait_device };
like($@, qr{^\Qmore than one device/emulator at },
     "Waiting while multiple devices is an error");
eval { $client->wait_id_device };
like($@, qr{^\QMissing id at },
     "transport_id must be a natural number");
eval { $client->wait_id_device(-2) };
like($@, qr{^\QId must be a natural number at },
     "transport_id must be a natural number");
is($client->wait_id_device(2), "", "Can wait trasport id device");
is($client->wait_usb_device, "", "Can wait unique usb device");
is($client->wait_local_device, "", "Can wait unique local device");

is($client->disconnect("10.253.0.13"), "disconnected 10.253.0.13",
   "Disconnect device");
eval { $client->disconnect("10.253.0.13") };
like($@, qr{^\Qno such device '10.253.0.13:5555'},
     "Cannot do a double disconnect");

$client3->wait_local_device(callback => $callback3);

# Check that device is gone
@result = $client->devices;
is_deeply(\@result, [
  {
    "52000c4748d6a283" => "device"
  },
], "Expected devices result") || dumper(\@result);

is(@result3, 0, "No local device yet");
is($client->wait_usb_device, "", "Can wait for still unique usb device");

# This should usually start the wait while there is no device
# Not something you van see, but checked with strace
# Pre-connect $client2 so that the wait almost certainly starts before
# the device gets connected
$client2->_connect(blocking => 1);
# diag("strace trigger");
$client->connect("10.253.0.13", blocking => 0);
is(@result3, 0, "No local device yet");
is($client2->wait_local_device(blocking => 1), "",
   "Can wait for soon unique local device");

# The local device is there now
mainloop();
is_deeply(\@result3, [[undef, ""]], "Finished wait for local device") ||
    dumper(\@result3);

eval { $client4->wait_serial };
like($@, qr{^\QMissing serial at }, "Must give a serial");
eval { $client4->wait_serial_device };
like($@, qr{^\QMissing serial at }, "Must give a serial");
# Start a new wait
@result4 = ();
$client4->wait_serial_device("10.253.0.11:1234", callback => $callback4);

eval { $client->connect("10.253.0.13:5555") };
like($@, qr{^\Qalready connected to 10.253.0.13:5555 at },
     "Cannot do a double connect");

eval { $client->disconnect("52000c4748d6a283") };
like($@, qr{^\Qno such device '52000c4748d6a283:5555' at},
     "Cannot disconnect an usb device");

eval { $client->connect("10.253.0.8") };
like($@, qr{^\Qfailed to connect to '10.253.0.8:5555': Connection refused at },
     "Cannot connect to something unreachable");

# Check that device is back
@result = $client->devices;
is_deeply(\@result, [
  {
    "10.253.0.13:5555" => "device",
    "52000c4748d6a283" => "device"
  },
], "Expected devices result") || dumper(\@result);

is(@result4, 0, "Still not seen 10.253.0.11:1234") || dumper(\@result4);
is($client->connect("10.253.0.11:1234"), "connected to 10.253.0.11:1234",
   "Connect new local device");
is($client4->version(blocking => 1), 10, "Trigger a wait on client4");
is_deeply(\@result4, [[undef, ""]], "Wait for 10.253.0.11:1234 is over");
is($client->wait_serial_device("10.253.0.11:1234"), "",
   "Wait for 10.253.0.11:1234 now returns immediately");

is($client->wait_usb_device, "", "Can wait for still unique usb device");
eval { $client->wait_local_device };
like($@, qr{^\Qmore than one emulator at },
     "Cannot wait for many local devices");
is($client->device_drop("10.253.0.11:1234"), "Dropped '10.253.0.11:1234'",
   "Drop new local device (which was added using connect");

# Check that device is back
@result = $client->devices;
is_deeply(\@result, [
  {
    "10.253.0.13:5555" => "device",
    "52000c4748d6a283" => "device"
  },
], "Expected devices") || dumper(\@result);

my $callback = sub {
    push @result, [shift->connected, @_];
};
@result = ();
$client2->devices_track(callback => $callback);
$client2->version(callback => $callback);
is(@result, 0, "Nothing queued yet");
mainloop();
is(@result, 2, "Executed 2 commands");
is(@{$result[0]}, 4, "devices_track returns 4 things");
my $tracker = delete $result[0][3];
isa_ok($tracker, "ADB::Client::Tracker", "Expected tracker type");
is_deeply(\@result, [
  [
    0,
    undef,
    {
      "10.253.0.13:5555" => "device",
      "52000c4748d6a283" => "device"
    },
  ],
  [
    0,
    undef,
    10,
  ]
], "Expected track-devices and version output") || dumper(\@result);

# Also start waiting for an usb drop
@result4 = ();
$client4->wait_usb("disconnect", callback => $callback4);

my @tracked;
$tracker->track(sub { shift; push @tracked, [@_] });
eval { $tracker->track(sub {}) };
like($@, qr{^Already tracking at }, "Cannot do a double track");

my $host_features = $client->host_features;
is_deeply($host_features, {
    "abb" => 1,
    "abb_exec" => 1,
    "apex" => 1,
    "cmd" => 1,
    "fixed_push_mkdir" => 1,
    "fixed_push_symlink_timestamp" => 1,
    "ls_v2" => 1,
    "push_sync" => 1,
    "remount_shell" => 1,
    "sendrecv_v2" => 1,
    "sendrecv_v2_brotli" => 1,
    "sendrecv_v2_dry_run_send" => 1,
    "sendrecv_v2_lz4" => 1,
    "sendrecv_v2_zstd" => 1,
    "shell_v2" => 1,
    "stat_v2" => 1,
    "track_app" => 1
}, "Get just the host_features hash") || dumper($host_features);

for my $command (@serial_commands) {
    eval { $client->$command };
    like($@, qr{^more than one device/emulator at },
         "Cannot get '$command' from more than 1 device");
}

eval { $client->transport_any };
like($@, qr{^\Qmore than one device/emulator at },
     "Multiple devices for transport any");

eval { $client->tport_any };
like($@, qr{^\Qmore than one device/emulator at },
     "Multiple devices for tport any");

for my $command (@serial_commands) {
    is($client->transport_usb, "", "Can transport usb");
    @result = $client->$command;
    is_deeply(\@result, $serial_result{$command}{usb},
              "Can get $command from single device") || dumper(\@result);
}

for my $command (@serial_commands) {
    is($client->transport_local, "", "Can transport local");
    @result = $client->$command;
    is_deeply(\@result, $serial_result{$command}{local}, "Can get $command from single device") || dumper(\@result);
}

for my $command (@serial_commands) {
    is($client->transport_serial("52000c4748d6a283"), "", "Connect by serial");
    @result = $client->$command;
    is_deeply(\@result, $serial_result{$command}{usb},
              "Can get $command from single device") || dumper(\@result);
}

# Try a filtered features by HASH, try connecting with tport serial
is($client->tport_serial("52000c4748d6a283"), 2, "Connect by serial");
@result = $client->features(filter => $host_features);
is_deeply(\@result,
          [{
              "cmd" => 1,
              "shell_v2" => 1,
              "stat_v2" => 1,
              "wabbits"	=> 0,
          }, [
              "shell_v2",
              "cmd",
              "stat_v2",
              "wabbits",
          ], "shell_v2,cmd,stat_v2,wabbits"],
          "Can get filtered features from single device") || dumper(\@result);

# Try a filtered features by ARRAY, try connecting with tport usb
is($client->tport_usb, 2, "Can tport usb");
@result = $client->features(filter => [keys %$host_features]);
is_deeply(\@result, [{
    "cmd" => 1,
    "shell_v2" => 1,
    "stat_v2" => 1,
    "wabbits"	=> 0,
}, [
    "shell_v2",
    "cmd",
    "stat_v2",
    "wabbits",
], "shell_v2,cmd,stat_v2,wabbits"], "Can get features from single device") || dumper(\@result);

# Use tport to get to command
for my $command (@serial_commands) {
    is($client->tport_local, 3, "Can tport local");
    @result = $client->$command;
    is_deeply(\@result, $serial_result{$command}{local},
              "Can get $command from single device") || dumper(\@result);
}

for my $command (@serial_commands) {
    # Can get directly from usb/local
    for my $type (qw(usb local)) {
        my $c = "${command}_$type";
        @result = $client->$c;
        is_deeply(\@result, $serial_result{$command}{$type},
                  "Can get $c from single device") || dumper(\@result);
    }
    # Can get directly from serial
    my $c = "${command}_serial";
    @result = $client->$c("52000c4748d6a283");
    is_deeply(\@result, $serial_result{$command}{usb},
              "Can get $c from single device") || dumper(\@result);
}

# Reduce to one devices
$client->device_drop("10.253.0.13:5555");
#eval { $client->version };
#like($@, qr{^ab},
#     "We lost the connection because we had 10.253.0.13:5555 as transport");

is($client->wait_device, "", "We have a device");

eval { $client->transport_serial("10.253.0.13:5555") };
like($@, qr{^\Qdevice '10.253.0.13:5555' not found at},
     "Connect to non-existing device");

eval { $client->tport_serial("10.253.0.13:5555") };
like($@, qr{^\Qdevice '10.253.0.13:5555' not found at},
     "Connect to non-existing device");

# Can get device properties without setting a transport
for my $command (@serial_commands) {
    @result = $client->$command;
    is_deeply(\@result, $serial_result{$command}{usb},
              "Can get $command from single device") || dumper(\@result);
}

is($client->transport_any, "", "Can transport any");
is($client->transport_usb, "", "Can transport usb");
eval { $client->transport_local };
like($@, qr{^\Qno emulators found at }, "Dropped the networked device");

is($client->tport_any, 2, "Can tport any");
is($client->tport_usb, 2, "Can tport usb");
eval { $client->tport_local };
like($@, qr{^\Qno emulators found at }, "Dropped the networked device");

is(@result4, 0, "usb still not disconnected");
is($client->device_drop("52000c4748d6a283"), "Dropped '52000c4748d6a283'",
   "Drop device");
# No more devices
is($client4->version(blocking => 1), 10, "Force client4 into a result");
is_deeply(\@result4, [[undef, "" ]], "USB device disconnected") ||
    dumper(\@result4);

is($client->wait_usb("disconnect"), "", "no device is always disconnected");

for my $command (@serial_commands) {
    eval { $client->$command };
    like($@, qr{^no devices/emulators found at }, "Cannot get $command without devices");
}

for my $transport (qw(transport_any tport_any)) {
eval { $client->$transport };
like($@, qr{^\Qno devices/emulators found at },
     "No devices for $transport");
}

for my $transport (qw(transport_usb tport_usb)) {
eval { $client->$transport };
like($@, qr{^\Qno devices found at },
     "No devices for $transport");
}

for my $transport (qw(transport_local tport_local)) {
eval { $client->$transport };
like($@, qr{^\Qno emulators found at },
     "No devices for $transport");
}

is($client->device_add("10.253.0.13:5555"), "Added '10.253.0.13:5555'",
   "Add device");
$client->kill(blocking => 0);

mainloop();
is_deeply(\@tracked, [[
    undef,
    {
        "10.253.0.13:5555" => "offline",
        "52000c4748d6a283" => "device"
    },
    { "10.253.0.13:5555" => [ "device", "offline" ] }
], [
    undef,
    { "52000c4748d6a283" => "device" },
    { "10.253.0.13:5555" => ["offline", ""] }
], [
    undef,
    { "52000c4748d6a283" => "offline" },
    { "52000c4748d6a283" => [ "device", "offline" ] }
], [
    undef,
    { },
    { "52000c4748d6a283" => ["offline", ""], }
], [
   undef,
    { "10.253.0.13:5555" => "offline" },
    { "10.253.0.13:5555" => ["", "offline"], }
], [
    undef,
    { "10.253.0.13:5555" => "device" },
    { "10.253.0.13:5555" => [ "offline", "device" ] }
], [
    "EOF",
    1
]], "Expected device history") || dumper(\@tracked);
eval { $tracker->untrack };
like($@, qr{^Not tracking at }, "Cannot undo a finished tracker");
eval { $tracker->track };
like($@, qr{^No callback at }, "Cannot track without callback");
eval { $tracker->track(sub {}) };
like($@, qr{^Socket closed at }, "Cannot track a finished tracker");

# Restart killed service
$port_10 = adb_version(10);
$client3 = new_ok("ADB::Client" => [host => "127.0.0.1",
                                    port => $port_10, blocking => 0]);

# Now try blocking devices_track
$client2 = new_ok("ADB::Client" => [host => "127.0.0.1", port => $port_10]);
@result = $client2->devices_track;
is(@result, 2, "Expect 2 results");
$tracker = delete $result[1];
is_deeply(\@result, [
  {
    "10.253.0.13:5555" => "device",
    "52000c4748d6a283" => "device"
  },
], "Our devices are back on the new server") || dumper(\@result);
isa_ok($tracker, "ADB::Client::Tracker", "Expected tracker type");
# Tracking testing is a bit tricky since we need something to change the
# devices while we are stuck in mainloop. Use other mainloop callbacks
$client = new_ok("ADB::Client" => [host => "127.0.0.1", port => $port_10,
                                   blocking => 0]);
$client->version(callback => sub {
                     my $client = shift;
                     $client-> device_drop("10.253.0.13:5555");
                     $client->device_drop("52000c4748d6a283");
});
@tracked = $tracker->wait;
is_deeply(\@tracked, [
    {
        "10.253.0.13:5555" => "offline",
        "52000c4748d6a283" => "device"
    },
    { "10.253.0.13:5555" => [ "device", "offline" ] }
], "First event") || dumper(\@tracked);
@tracked = $tracker->wait;
is_deeply(\@tracked, [
    { "52000c4748d6a283" => "device" },
    { "10.253.0.13:5555" => ["offline", ""] }
], "Second event") || dumper(\@tracked);

@tracked = $tracker->wait;
is_deeply(\@tracked, [
    { "52000c4748d6a283" => "offline" },
    { "52000c4748d6a283" => [ "device", "offline" ] },
], "Third event") || dumper(\@tracked);
# Now we are sure 52000c4748d6a283 is offline start waiting for an usb device
# diag("strace trigger");
@result3 = ();
$client3->wait_usb_device(callback => $callback3);

@tracked = $tracker->wait;
is_deeply(\@tracked, [
    {},
    { "52000c4748d6a283" => ["offline", ""] }
], "Fourth event") || dumper(\@tracked);

# We are now without devices again

# Notice that $client is non-blocking and already connected
# $client2 is blocking and not connected.
# So this test should be very likely to have $client blocked waiting for a
# device (I confirmed with strace that this actually tests something)
$client2->device_add("10.253.0.13:5555", blocking => 0);
is($client->wait_device(blocking => 1), "", "Can wait for first device");

is($client->wait_local("disconnect", blocking => 1), "",
   "local device is always disconnected");

# Try to create a situation where the first device add event is already
# written by the time the "host:track-devices" response comes back
# Pre-connect $client
$client->_connect;
my @tracked2;
$client2->devices_track(blocking => 0,
                        callback => sub { shift; push @tracked2, [@_] });
# Spam a lot of changes. We hope to attach an answer to the devices_track answer
for my $i (1..3) {
    $client->device_drop("10.253.0.13:5555");
    $client->device_add("10.253.0.13:5555");
}
is(@result3, 0, "Still no usb device");
is($client->device_add("52000c4748d6a283", blocking => 1),
   "Added '52000c4748d6a283'", "Add two devices in a really convoluted way");
is(@tracked2, 1, "Have one result from the callback");
is($tracked2[0][0], undef, "No error from devices_track");
my $tracked = scalar $tracker->wait;
is_deeply($tracked, { "10.253.0.13:5555" => "offline" }, "Scalar context") ||
    dumper($tracked);
is($client3->version(blocking => 1), 10, "Trigger wait on client3");
is_deeply(\@result3, [[undef, ""]], "And now we have out usb device");

# Reboot/restart tests and waiting for unusual states (I dont think they exist)
# First double check current staste
$client = new_ok("ADB::Client" => [host => "127.0.0.1", port => $port_10]);
@result = $client->devices;
is_deeply(\@result, [
    {
        "10.253.0.13:5555" => "device",
        "52000c4748d6a283" => "device"
    },
], "Expected devices result") || dumper(\@result);

is($client->wait("disconnect"), "",
   "local device is always disconnected");
is($client->wait_local("disconnect"), "",
   "local device is always disconnected");
is($client->wait_serial("10.253.0.13:5555", "disconnect"), "",
   "local device is always disconnected");
is($client->wait_serial("Boem", "disconnect"), "",
   "nonsense device is always disconnected");
is($client->wait_id(int(1e5), "disconnect"), "",
   "nonsense transport id is always disconnected");

my $transport_id = $client->tport_usb;
like($transport_id, qr{^\d+\z}a, "Connect to usb device");
# Start waiting on a specific device
$client4 = new_ok("ADB::Client" => [host => "127.0.0.1",
                                    port => $port_10, blocking => 0]);
@result4 = ();
# Give client4 a head start in the race
$client4->_connect(blocking => 1);
$client4->wait_id($transport_id, "disconnect", callback => $callback4);
is($client3->version(blocking => 1), 10, "Trigger wait on client3");
is(@result4, 0, "transport_id event didn't trigger yet");
# Now restart the the usb device
is($client->restart, "", "Can reboot");
is($client->state_usb, "device", "Still a device");
is($client4->version(blocking => 1), 10, "Force a result for client4");
is_deeply(\@result4, [[undef, ""]], "The usb device disconnected");

is($client->transport_usb, "", "Connect to usb device");
eval { $client->reboot };
like($@, qr{^\QToo few arguments at }, "Cannot reboot without arguments");

is($client->transport_usb, "", "Connect to usb device");
is($client->reboot("recovery"), "", "Can reboot");
is($client->state_usb, "recovery", "Now in recovery");
