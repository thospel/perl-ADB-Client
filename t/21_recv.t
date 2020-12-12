#!/usr/bin/perl -w
# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl 21_recv.t'
#########################
## no critic (UselessNoCritic MagicNumbers)
# Test some simple commands

use strict;
use warnings;

our $VERSION = "1.000";

use FindBin qw($Bin);
use lib $Bin;
use Errno qw(EAGAIN EINTR EWOULDBLOCK);

my $tests;
BEGIN { $tests = 36 }
use Test::More tests => $tests;

use TestDrive qw($tests_driver $tests_pre $t_dir $tmp_dir
                 unalarm
                 adb_server adb_start adb_run filesystem dumper $developer);

# We already checked loading in 04_adb_client.t
use ADB::Client qw(mainloop $ADB $ADB_HOST $ADB_PORT);
use ADB::Client::Utils
    qw(adb_file_type_from_mode realtime errno_adb_from_native time_from_adb);

my $adb_dir = "/sdcard/adb_client_test";
my $test_dir = $t_dir . $adb_dir;
my $utf8_file = "f\xf3\xf2\x{1321}";
my $decoded_file = $utf8_file;
utf8::encode($decoded_file);

my $version = 10;
my ($created, $android_root);
SKIP : {
    if ($developer) {
        $version = adb_server(41, $tests);

        $ADB = $ENV{ADB_CLIENT_TEST_REAL};
        is(adb_run("-e", "shell", "rm -rf $adb_dir && echo done"),
           "done\n", "adb shell 'rm -rf $adb_dir'");
        like(adb_run("-e", "push", $test_dir, "/sdcard/"),
             qr{4 files pushed, 0 skipped},
             "adb push $test_dir /sdcard/");
        is(adb_run("-e", "shell", "mkdir $adb_dir/empty_dir && echo done"),
           "done\n", "adb shell 'mkdir $adb_dir/empty_dir'");
        $created = 1;
        skip "Developer mode doesn't start a fake adb server", $tests_pre-4;
    }

    $android_root = filesystem("$t_dir/sdcard");
    # Neither git, nor MANIFEST nor adb push handle empty directories well
    # So make on ourselves
    mkdir("$android_root$adb_dir/empty_dir") ||
        die "Could not mkdir($android_root$adb_dir/empty_dir): $^E";

    my $port = adb_start($version);

    $ADB_HOST = "127.0.0.1";
    $ADB_PORT = $port;

    my $client = new_ok("ADB::Client");
    $client->transport_local;
    is($client->filesystem($android_root), "Set filesystem", "Set filesystem");
}

END {
  SKIP: {
        local $?;
        skip "No cleanup for plain user", 2 unless $developer;
        skip "Nothing to cleanup", 2 unless $created;
        is(adb_run("-e", "shell", "rm -rf $adb_dir && echo done"),
           "done\n", "rm -rf /sdcard/adb_client_test");
    }
}

my $client = new_ok("ADB::Client");
$client->transport_local;
my @result = eval { $client->sync };
if ($@) {
  SKIP: {
        my $err = $@;
        diag("Could nor run sync: <$err> \[@result]");
        skip "Could nor run sync: <$err>", $tests - $tests_driver - $tests_pre - 1;
    }
    exit;
}
is_deeply(\@result, [""], "Can run sync");

pipe(my $rd, my $wr) || die "Could not create pipe: $^E";
$rd->blocking(0);
$wr->blocking(0);

sub _reader {
    my ($obj) = shift;
    my $rc = sysread($obj->{socket}, my $buffer, int(2**16));
    if ($rc) {
        $obj->{in} .= $buffer;
    } elsif (defined ($rc)) {
        # EOF
        delete $obj->{reader};
    } elsif ($! == EAGAIN || $! == EINTR || $! == EWOULDBLOCK) {
        return;
    } else {
        die "Could not read from pipe: $^E";
    }
}

my $obj = bless {
    in		=> "",
    socket	=> $rd,
};
$obj->{reader} = $rd->add_read($obj, \&_reader);
@result = $client->recv_v1("$adb_dir/foo", socket => $wr);
is_deeply(\@result, [""], "No data is returned, it all goes into the socket");
is($obj->{in}, "foo\n", "We received the file contents");
ok($obj->{reader}, "Still reading from socket");

unalarm();
my $client1 = new_ok("ADB::Client", [block_size => 1]);
$client1->transport_local;
@result = $client1->sync;
is_deeply(\@result, [""], "Can run sync");
$obj->{in} = "";
@result = $client1->recv_v1("$adb_dir/$utf8_file",
                            socket => $wr,
                           transfer_block_size => 1,
                           data => "a" x 28);
is_deeply(\@result, [""], "No data is returned, it all goes into the socket");
is($obj->{in},
   "a" x 28 . "foo\n$decoded_file\n",
   "We received the file contents");
ok($obj->{reader}, "Still reading from socket");

# Notice that this intentionally generates a file of size 4(2**17+1)
# The extra 4 bytes will test the handling of a short trailer
my $content = "";
$content .= pack("N", rand(2**32)) for 0..2**17;
my $content_length = length($content);
my $bl = $client->block_size(2**16);
my ($length, $mtime) = $client->send_v1("$adb_dir/bar", $content,
                                        mtime => 8,
                                        ftype => "REG",
                                        perms => 0444);
$client->block_size($bl);
is($length, $content_length, "Expected length");
is($mtime, 8, "Expected mtime");

unalarm();
$obj->{in} = "";
# $obj->{reader} = $rd->add_read($obj, \&_reader);
@result = $client->recv_v1("$adb_dir/bar", socket => $wr);
is_deeply(\@result, [""], "No data is returned, it all goes into the socket");
is($obj->{in}, $content, "We received the file contents");
ok($obj->{reader}, "Still reading from socket");

unalarm();
my $file = "$tmp_dir/out";
@result = $client->recv_v1("$adb_dir/$utf8_file",
                           file => $file,
                           transfer_block_size => 1,
                           data => "a" x 28);
is_deeply(\@result, [""], "No data is returned, it all goes into the file");
open(my $fh, "<", $file) || die "Could not open '$file': $^E";
binmode $fh;
my $c = do { local $/; <$fh> };
is($c, "a" x 28 . "foo\n$decoded_file\n", "We received the file contents");

unalarm();
@result = $client->recv_v1("$adb_dir/bar", file => $file);
is_deeply(\@result, [""], "No data is returned, it all goes into the file");
$fh = undef;
open($fh, "<", $file) || die "Could not open '$file': $^E";
binmode $fh;
$c = do { local $/; <$fh> };
is($c, $content, "We received the file contents");

unalarm();
$fh = undef;
open($fh, ">", $file) || die "Could not open '$file': $^E";
binmode($fh);
@result = $client->recv_v1("$adb_dir/bar", file => $fh);
is_deeply(\@result, [""], "No data is returned, it all goes into the file");
$fh = undef;
open($fh, "<", $file) || die "Could not open '$file': $^E";
binmode $fh;
$c = do { local $/; <$fh> };
is($c, $content, "We received the file contents");
