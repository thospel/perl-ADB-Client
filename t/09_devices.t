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
use Test::More tests => 72;
use TestDrive qw(adb_start adb_version dumper);

# We already checked loading in 04_adb_client.t
use ADB::Client qw(mainloop);

# keep this one alive
my $port = adb_start();
# $port = 5037;
my $port_10 = adb_version(10);

my ($client, @result);

$client = new_ok("ADB::Client" => [host => "127.0.0.1", port => $port_10]);

# Test host:devices
@result = $client->devices;
is_deeply(\@result, [
    {
        "10.253.0.13:5555" => "device",
        "52000c4748d6a283" => "device"
    },
    ["10.253.0.13:5555", "52000c4748d6a283"],
    "10.253.0.13:5555\tdevice\n52000c4748d6a283\tdevice\n"
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
    },
    ["10.253.0.13:5555", "52000c4748d6a283"],
    "10.253.0.13:5555\tdevice\tproduct:zerofltexx model:SM_G920F device:zeroflte transport_id:1\n52000c4748d6a283\tdevice\tusb:1-1.2 product:lineage_kminilte model:SM_G800F device:kminiltexx transport_id:2\n"],
          "Expected devices long result");

my $client2 = new_ok("ADB::Client" => [host => "127.0.0.1", port => $port_10, blocking => 0]);
my $callback = sub {
    push @result, [shift->connected, @_];
};
@result = ();
$client2->devices_track(callback => $callback);
$client2->version(callback => $callback);
is(@result, 0, "Nothing queued yet");
mainloop();
is(@result, 2, "Executed 2 commands");
is(@{$result[0]}, 6, "devices_track returns 4 things");
my $tracker = delete $result[0][5];
isa_ok($tracker, "ADB::Client::Tracker", "Expected tracker type");
is_deeply(\@result, [
  [
    0,
    undef,
    {
      "10.253.0.13:5555" => "device",
      "52000c4748d6a283" => "device"
    },
    [
      "10.253.0.13:5555",
      "52000c4748d6a283"
    ],
    "10.253.0.13:5555\tdevice\n52000c4748d6a283\tdevice\n"
  ],
  [
    0,
    undef,
    10,
  ]
], "Expected track-devices and version output") || dumper(\@result);
my @tracked;
$tracker->track(sub { shift; push @tracked, [@_] });
eval { $tracker->track(sub {}) };
like($@, qr{^Already tracking at }, "Cannot do a double track");

eval { $client->features };
like($@, qr{^more than one device/emulator at }, "Cannot get features from more than 1 device");

eval { $client->transport_any };
like($@, qr{^\Qmore than one device/emulator at },
     "Multiple devices for transport any");

eval { $client->transport_tcp };
like($@, qr{^\Qmore than one device/emulator at},
     "Multiple devices for transport tcp");

eval { $client->tport_any };
like($@, qr{^\Qmore than one device/emulator at },
     "Multiple devices for tport any");

eval { $client->tport_tcp };
like($@, qr{^\Qmore than one device/emulator at},
     "Multiple devices for tport tcp");

is($client->transport_usb, "", "Can transport usb");
@result = $client->features;
is_deeply(\@result, [{
    "cmd" => 1,
    "shell_v2" => 1,
    "stat_v2" => 1
}, [
    "shell_v2",
    "cmd",
    "stat_v2"
], "shell_v2,cmd,stat_v2"], "Can get features from single device") || dumper(\@result);

is($client->transport_local, "", "Can transport local");
@result = $client->features;
is_deeply(\@result, [
    {
        "abb" => 1,
        "abb_exec" => 1,
        "apex" => 1,
        "cmd" => 1,
        "fixed_push_mkdir" => 1,
        "fixed_push_symlink_timestamp" => 1,
        "shell_v2" => 1,
        "stat_v2" => 1
    },
    [
        "fixed_push_mkdir",
        "shell_v2",
        "apex",
        "stat_v2",
        "abb",
        "fixed_push_symlink_timestamp",
        "cmd",
        "abb_exec"
    ],
    "fixed_push_mkdir,shell_v2,apex,stat_v2,abb,fixed_push_symlink_timestamp,cmd,abb_exec"
], "Can get features from single device") || dumper(\@result);

is($client->transport("52000c4748d6a283"), "", "Connect by serial");
@result = $client->features;
is_deeply(\@result, [{
    "cmd" => 1,
    "shell_v2" => 1,
    "stat_v2" => 1
}, [
    "shell_v2",
    "cmd",
    "stat_v2"
], "shell_v2,cmd,stat_v2"], "Can get features from single device") || dumper(\@result);

is($client->tport("52000c4748d6a283"), 2, "Connect by serial");
@result = $client->features;
is_deeply(\@result, [{
    "cmd" => 1,
    "shell_v2" => 1,
    "stat_v2" => 1
}, [
    "shell_v2",
    "cmd",
    "stat_v2"
], "shell_v2,cmd,stat_v2"], "Can get features from single device") || dumper(\@result);

is($client->tport_usb, 2, "Can tport usb");
@result = $client->features;
is_deeply(\@result, [{
    "cmd" => 1,
    "shell_v2" => 1,
    "stat_v2" => 1
}, [
    "shell_v2",
    "cmd",
    "stat_v2"
], "shell_v2,cmd,stat_v2"], "Can get features from single device") || dumper(\@result);

is($client->tport_local, 1, "Can tport local");
@result = $client->features;
is_deeply(\@result, [
    {
        "abb" => 1,
        "abb_exec" => 1,
        "apex" => 1,
        "cmd" => 1,
        "fixed_push_mkdir" => 1,
        "fixed_push_symlink_timestamp" => 1,
        "shell_v2" => 1,
        "stat_v2" => 1
    },
    [
        "fixed_push_mkdir",
        "shell_v2",
        "apex",
        "stat_v2",
        "abb",
        "fixed_push_symlink_timestamp",
        "cmd",
        "abb_exec"
    ],
    "fixed_push_mkdir,shell_v2,apex,stat_v2,abb,fixed_push_symlink_timestamp,cmd,abb_exec"
], "Can get features from single device") || dumper(\@result);

$client->device_drop("10.253.0.13:5555");
eval { $client->transport("10.253.0.13:5555") };
like($@, qr{^\Qdevice '10.253.0.13:5555' not found at},
     "Connect to non-existing device");

@result = $client->features;
is_deeply(\@result, [{
    "cmd" => 1,
    "shell_v2" => 1,
    "stat_v2" => 1
}, [
    "shell_v2",
    "cmd",
    "stat_v2"
], "shell_v2,cmd,stat_v2"], "Can get features from single device") || dumper(\@result);

is($client->transport_any, "", "Can transport any");
is($client->transport_tcp, "", "Can transport tcp");
is($client->transport_usb, "", "Can transport usb");
eval { $client->transport_local };
like($@, qr{^\Qno emulators found at }, "Dropped the networked device");

is($client->device_drop("52000c4748d6a283"), "Dropped", "Drop device");
eval { $client->features };
like($@, qr{^no devices/emulators found at }, "Cannot get features without devices");

eval { $client->transport_any };
like($@, qr{^\Qno devices/emulators found at },
     "No devices for transport any");
eval { $client->transport_tcp };
like($@, qr{^\Qno devices/emulators found at},
     "No devices for transport tcp");
eval { $client->transport_local };
like($@, qr{^\Qno emulators found at },
     "No devices for transport any");
eval { $client->transport_usb };
like($@, qr{^\Qno devices found at},
     "No devices for transport tcp");

is($client->device_add("10.253.0.13:5555"), "Added", "Add device");
$client->kill(blocking => 0);

mainloop();
is_deeply(\@tracked, [[
    undef,
    {
        "10.253.0.13:5555" => "offline",
        "52000c4748d6a283" => "device"
    },
    [
        "10.253.0.13:5555",
        "52000c4748d6a283"
    ],
    "10.253.0.13:5555\toffline\n52000c4748d6a283\tdevice\n",
    {
        "change" => {
            "10.253.0.13:5555" => [
                "device",
                "offline"
            ]
        },
    }
], [
    undef,
    {
        "52000c4748d6a283" => "device"
    },
    [
        "52000c4748d6a283"
    ],
    "52000c4748d6a283\tdevice\n",
    {
        "delete" => {
            "10.253.0.13:5555" => "offline"
        }
    }
], [
    undef,
    {
        "52000c4748d6a283" => "offline"
    },
    [
        "52000c4748d6a283"
    ],
    "52000c4748d6a283\toffline\n",
    {
        "change" => {
            "52000c4748d6a283" => [
                "device",
                "offline"
            ]
        },
    }
], [
    undef,
    {
    },
    [],
    "",
    {
        "delete" => {
            "52000c4748d6a283" => "offline"
        }
    }
], [
    undef,
    {
        "10.253.0.13:5555" => "offline"
    },
    [
        "10.253.0.13:5555"
    ],
    "10.253.0.13:5555\toffline\n",
    {
        "add" => {
            "10.253.0.13:5555" => "offline"
        }
    }
], [
    undef,
    {
        "10.253.0.13:5555" => "device"
    },
    [
        "10.253.0.13:5555"
    ],
    "10.253.0.13:5555\tdevice\n",
    {
        "change" => {
            "10.253.0.13:5555" => [
                "offline",
                "device"
            ]
        }
    }
], [
    "EOF",
    1
]], "Expected device history") || dumper(\@tracked);
eval { $tracker->untrack };
like($@, qr{^Not tracking at }, "Cannot undo a finished tracker");
eval { $tracker->track };
like($@, qr{^Socket closed at }, "Cannot track a finished tracker");

# Restart killed service
$port_10 = adb_version(10);

# Now try blocking devices_track
$client2 = new_ok("ADB::Client" => [host => "127.0.0.1", port => $port_10]);
@result = $client2->devices_track;
is(@result, 4, "Expect 4 results");
$tracker = delete $result[3];
is_deeply(\@result, [
  {
    "10.253.0.13:5555" => "device",
    "52000c4748d6a283" => "device"
  },
  [
    "10.253.0.13:5555",
    "52000c4748d6a283"
  ],
  "10.253.0.13:5555\tdevice\n52000c4748d6a283\tdevice\n"
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
    [
        "10.253.0.13:5555",
        "52000c4748d6a283"
    ],
    "10.253.0.13:5555\toffline\n52000c4748d6a283\tdevice\n",
    {
        "change" => {
            "10.253.0.13:5555" => [
                "device",
                "offline"
            ]
        },
    }
], "First event") || dumper(\@tracked);
@tracked = $tracker->wait;
is_deeply(\@tracked, [
    { "52000c4748d6a283" => "device" },
    [ "52000c4748d6a283" ],
    "52000c4748d6a283\tdevice\n",
    {
        "delete" => {
            "10.253.0.13:5555" => "offline"
        }
    }
], "Second event") || dumper(\@tracked);
@tracked = $tracker->wait;
is_deeply(\@tracked, [
    { "52000c4748d6a283" => "offline" },
    [ "52000c4748d6a283" ],
    "52000c4748d6a283\toffline\n",
    {
        "change" => {
            "52000c4748d6a283" => [
                "device",
                "offline"
            ]
        },
    }
], "Third event") || dumper(\@tracked);
@tracked = $tracker->wait;
is_deeply(\@tracked, [
    {},
    [],
    "",
    {
        "delete" => {
            "52000c4748d6a283" => "offline"
        }
    }
], "Fourth event") || dumper(\@tracked);
is($client->device_add("10.253.0.13:5555", blocking => 1),
   "Added", "Add device");
my $tracked = scalar $tracker->wait;
is_deeply($tracked, { "10.253.0.13:5555" => "offline" }, "Scalar context") ||
    dumper($tracked);
is($client->device_add("52000c4748d6a283", blocking => 1),
   "Added", "Add device");
$tracker->track(sub { die "Boem" if $_[4] =~ /52000c4748d6a283/});
eval { mainloop() };
like($@, qr{^\QBoem at }, "Error during callback");
