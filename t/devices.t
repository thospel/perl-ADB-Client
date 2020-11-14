#!/usr/bin/perl -w
use strict;
use warnings;

use Carp;
use IO::Socket::IP qw();
use ADB::Client qw($ADB);

$ADB = "bin/adb_fake";

ADB::Client->add_command(["device_drop"  => "internal:device_drop:%s", -1, 0]);

my ($adb_out, $adb_control, $pid);

my $warnings = 0;
$SIG{__WARN__} = sub {
    $warnings++;
    my $string = shift;
    chomp $string;
    print STDERR "Perl Warning: $string\n";
};

END {
    local $?;
    close($adb_control);
    $adb_control = undef;
    my $exit = do { local $/; <$adb_out> };
    close($adb_out);
    $adb_out = undef;
}

sub adb_start {
    $pid = do {
        # open($adb_out = undef, "-|", $ADB)
        open($adb_out = undef, "-|", $^X, $ADB, "--blib") ||
            die "Cannot even start fake adb server: $^E";
    };
    my $line = <$adb_out> //
        die "Unexpected EOF from fake adb server";
    my ($port) = $line =~ m{^Port: (\d+)$} or die "Invalid Port answer";

    $adb_control = IO::Socket::IP->new(
        PeerHost => "127.0.0.1",
        PeerPort => $port) ||
            die "Could seemingly start fake adb server, but cannot connect to control 127.0.0.1:$port: $@";
    $line = <$adb_control> // die "Unexpected EOF from fake adb server control";
    print $adb_control "Listener\n";
    $line = <$adb_control> // die "Unexpected EOF from fake server";
    ($port) = $line =~ m{^Port: (\d+)$} or die "Invalid Port answer";
    return int($port);
}

my $port = adb_start();
my $client2 = ADB::Client->new(port => $port, blocking => 0);
my $client  = ADB::Client->new(port => $port);

$client2->transport_usb;
$client->device_drop("52000c4748d6a283", blocking => 1);
