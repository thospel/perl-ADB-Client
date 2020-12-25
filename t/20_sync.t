#!/usr/bin/perl -w
# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl 20_sync.t'
#########################
## no critic (UselessNoCritic MagicNumbers)
# Test some simple commands

use strict;
use warnings;

our $VERSION = "1.000";

use FindBin qw($Bin);
use lib $Bin;
use Errno qw(ENOPROTOOPT);

my $tests;
BEGIN { $tests = 71 }
use Test::More tests => $tests;

use TestDrive qw($tests_driver $tests_pre $t_dir
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

# Try lstat v1
@result = $client->lstat_v1("/sdcard");
is_deeply(\@result, [{
    # mode	=> 0120777,
    mode	=> 0120644,
    mtime	=> $result[0]{mtime},
    size	=> 21,
    ftype	=> "LINK",
    perms	=> 0644,
  }
], "Can run lstat_v1 on /sdcard") || dumper(\@result);

# Try lstat v2
@result = $client->lstat_v2("/sdcard");
is_deeply(\@result, [{
    atime => $result[0]{atime},
    ctime => $result[0]{ctime},
    dev => $result[0]{dev},
    gid => 0,
    ino => $result[0]{ino},
    mode => 0120644,
    # mode => 0120777,
    ftype => "LINK",
    perms => 0644,
    mtime => $result[0]{mtime},
    nlink => 1,
    size => 21,
    uid => 0
}], "Can run lstat_v2 on /sdcard") || dumper(\@result);

# Try stat v2
@result = $client->stat_v2("/sdcard");
is(@result, 1, "One result");
isa_ok($result[0], "HASH", "Which");
SKIP: {
    skip "stat_v2 result is not a hash", 2 unless ref $result[0] eq "HASH";
    is_deeply([sort keys %{$result[0]}],
              [qw(atime ctime dev ftype gid ino mode mtime nlink perms size uid)],
              "Can run stat_v2 on /sdcard") || dumper([sort keys %{$result[0]}]);
    is($result[0]{mode}, 040771, "Get proper file mode");
}

# Try list v1
# @result = $client->list_v1($adb_dir);
@result = $client->list_v1("$adb_dir/empty_dir");
isa_ok($result[0], "HASH", "Which");
SKIP: {
    skip "list_v1 result is not a hash", 1 unless ref $result[0] eq "HASH";
    # skip "list_v1 result does not include . and ..", 1 unless %{$result[0]};
    is_deeply(\@result, [{
        "." => {
            mode => 040771,
            ftype => "DIR",
            perms => 0771,
            mtime => $result[0]{"."}{mtime},
            size => $result[0]{"."}{size},
        },
        ".." => {
            mode => 040771,
            ftype => "DIR",
            perms => 0771,
            mtime => $result[0]{".."}{mtime},
            size => $result[0]{".."}{size},
        }
    }], "List v1 on empty directory") ||dumper(\@result);
}

my @progress;
@result = $client->list_v1("$adb_dir/empty_dir", on_progress => sub {
    my ($client0, $data, $file, $stat) = @_;
    cmp_ok($client0, "==", $client, "First on_progress argument is the client");
    isa_ok($data, "HASH", "Second on_progress argument");
    push @progress, [$file, $stat];
    $data->{$file} = 1;
});
is_deeply(\@result, [{ "." => 1, ".." => 1 }], "Got modified result") ||
    dumper(\@result);
is_deeply(\@progress, [
    [".",
     {
         mode => 040771,
         ftype => "DIR",
         perms => 0771,
         mtime => $progress[0][1]{mtime},
         size => $progress[0][1]{size},
     }
 ],
    ["..",
     {
         mode => 040771,
         ftype => "DIR",
         perms => 0771,
         mtime =>  $progress[1][1]{mtime},
         size => $progress[1][1]{size},
     }
 ]
], "Got on_progress callbacks") || dumper(\@progress);

my $noproto_adb = errno_adb_from_native(ENOPROTOOPT);
my $noproto_native = local $! = ENOPROTOOPT;
@result = $client->list_v1("$adb_dir/does_not_exist");
is_deeply(\@result, [$noproto_adb], "List v1 on non existing directory");

@result = $client->list_v1("$adb_dir/foo");
is_deeply(\@result, ["Protocol not available"],
          "List v1 on existing but not a directory");
@result = $client->list_v1($adb_dir);
is(@result, 1, "One result");
isa_ok($result[0], "HASH", "Which") || dumper(\@result);
my @files = sort keys %{$result[0]};
is_deeply(\@files, [qw(. .. dir empty empty_dir foo), $utf8_file],
          "Proper list of files") || dumper(\@files);
my $stat_foo = $result[0]{foo};
isa_ok($stat_foo, "HASH", "Any single stat entry");
is_deeply([sort keys %$stat_foo], [qw(ftype mode mtime perms size)],
          "Expected stat keys") || dumper($stat_foo);
is($stat_foo->{mode}, 0100660, "Expected mode");
is(adb_file_type_from_mode($stat_foo->{mode}), "REG", "foo is a regulat file");
is(adb_file_type_from_mode($result[0]{dir}{mode}), "DIR", "dir is a directory");

# Try list_v1 recusrive
@result = $client->list_v1($adb_dir, recursive => 1, on_progress => sub {
    my ($client, $data, $file, $stat, $parent) = @_;
    delete @$stat{qw(mtime size)};
    $stat->{parent} = $parent;
    $data->{$file} = $stat;
});
my $expect = [{
    "." => {
        parent => "/sdcard/adb_client_test",
        ftype => "DIR",
        mode => 040771,
        perms => 0771
    },
    ".." => {
        parent => "/sdcard/adb_client_test",
        ftype => "DIR",
        mode => 040771,
        perms => 0771
    },
    dir => {
        parent => "/sdcard/adb_client_test",
        ftype => "DIR",
        mode => 040771,
        perms => 0771,
        tree => {
            "." => {
                parent => "/sdcard/adb_client_test/dir",
                ftype => "DIR",
                mode => 040771,
                perms => 0771
            },
            ".." => {
                parent => "/sdcard/adb_client_test/dir",
                ftype => "DIR",
                mode => 040771,
                perms => 0771
            },
            bar => {
                parent => "/sdcard/adb_client_test/dir",
                ftype => "REG",
                mode => 0100660,
                perms => 0660
            }
        }
    },
    empty => {
        parent => "/sdcard/adb_client_test",
        ftype => "REG",
        mode => 0100660,
        perms => 0660
    },
    empty_dir => {
        parent => "/sdcard/adb_client_test",
        ftype => "DIR",
        mode => 040771,
        perms => 0771,
        tree => {
            "." => {
                parent => "/sdcard/adb_client_test/empty_dir",
                ftype => "DIR",
                mode => 040771,
                perms => 0771
            },
            ".." => {
                parent => "/sdcard/adb_client_test/empty_dir",
                ftype => "DIR",
                mode => 040771,
                perms => 0771
            }
        }
    },
    foo => {
        parent => "/sdcard/adb_client_test",
        ftype => "REG",
        mode => 0100660,
        perms => 0660
    },
    "f\x{f3}\x{f2}\x{1321}" => {
        parent => "/sdcard/adb_client_test",
        ftype => "REG",
        mode => 0100660,
        perms => 0660
    }
}];
is_deeply(\@result, $expect, "Recursive directory") || dumper(\@result);

# Try list_v1 recusrive
@result = $client->list_dir($adb_dir, recursive => 1, self_parent => 0);
is_deeply(\@result, [{
  "/sdcard/adb_client_test/dir" => 1,
  "/sdcard/adb_client_test/dir/bar" => 1,
  "/sdcard/adb_client_test/empty" => 1,
  "/sdcard/adb_client_test/empty_dir" => 1,
  "/sdcard/adb_client_test/foo" => 1,
  "/sdcard/adb_client_test/f\x{f3}\x{f2}\x{1321}" => 1
}], "Recursive directory") || dumper(\@result);

# Try list v2
if (0) {
    # @result = $client->list_v2($adb_dir);
    @result = $client->list_v2("$adb_dir/empty_dir");
    is_deeply(\@result, [{
        "." => {
            "mode" => 040771,
            "mtime" => $result[0]{"."}{mtime},
            "size" => 4096
        },
        ".." => {
            "mode" => 040771,
            "mtime" => $result[0]{".."}{mtime},
            "size" => 4096
        }
    }], "List v2 on empty directory") ||dumper(\@result);
    @result = $client->list_v2("$adb_dir/does_not_exist");
    is_deeply(\@result, [ "Protocol not available" ], "List v2 on non existing directory");
    @result = $client->list_v2("$adb_dir/foo");
    is_deeply(\@result, [ "Protocol not available" ], "List v2 on non file");
}

# Try recv v1
@result = $client->recv_v1("$adb_dir/foo");
is_deeply(\@result, ["foo\n" ], "Fetch content of foo");
# Check unicode fetch and raw
@result = $client->recv_v1("$adb_dir/$utf8_file", raw => 1);
is_deeply(\@result,
          ["DATA\r\0\0\0foo\n$decoded_file\nDONE\0\0\0\0"],
          "Fetch content of utf8_file") || dumper(\@result);

# Try recv v2
if (0) {
    @result = $client->recv_v2("$adb_dir/foo");
    is_deeply(\@result, ["foo\n" ], "Fetch content of foo");
}

# Try send v1

my $time_before = int(realtime());
my $content = "barbar\n";
my $content_length = length $content;
@result = $client->send_v1("$adb_dir/bar", data => $content);
my $time_after = realtime();
is(@result, 2, "Two results") || dumper(\@result);
is($result[0], $content_length, "Return bytes transferred") || dumper(\@result);
my $mtime = $result[1];
cmp_ok($mtime, ">=", $time_before, "Time lower bound") || dumper(\@result);
cmp_ok($mtime, "<=", $time_after, "Time upper bound") || dumper(\@result);

# Check transferred file
@result = $client->lstat_v1("$adb_dir/bar");
is_deeply(\@result, [{
    mode => 0100660,
    ftype => "REG",
    perms => 0660,
    mtime => $mtime,
    size => $content_length,
}], "Expected stat of new file") || dumper(\@result);
SKIP: {
    skip "Basic file transfer already failed", 4 unless
        is($client->recv_v1("$adb_dir/bar"), $content, "Can retreive sent file");
    my $content = "";
    $content .= pack("N", rand(2**32)) for 1..2**16;
    my $content_length = length($content);
    my ($length, $mtime) = $client->send_v1("$adb_dir/bar",
                                            data => $content,
                                            mtime => 8,
                                            ftype => "REG",
                                            perms => 0444);
    is($length, $content_length, "Expected length");
    is($mtime, 8, "Expected mtime");
    my $stat = $client->lstat_v1("$adb_dir/bar");
    is_deeply($stat, {
        mode => 0100660,
        ftype => "REG",
        perms => 0660,
        mtime => 8,
        size => 4*2**16,
    }, "Expected stat") || dumper($stat);
    is($client->recv_v1("$adb_dir/bar"), $content, "Can retreive sent file");
}

# Try send_v1raw
@result = $client->send_v1("$adb_dir/bar",
                           data => pack("(a4V/a*)3a4x4",
                           DATA => "bar",
                           DATA => "",
                           DATA => "zzzef",
                           "DONE"), raw => 1);
is($result[0], 40, "Raw bytes transferred");
is($client->recv_v1("$adb_dir/bar"), "barzzzef", "Can retreive sent file");

# Try send v2
if (0) {
    $content = "barbar\n";
    $content_length = length $content;
    @result = $client->send_v2("$adb_dir/bar", $content);
}

is($client->quit, "", "Quit sync mode");

# Check mkdir_unlink, create directories
$client->transport_local;
$client->sync;
ok($client->connected, "We start connected");
@result = eval { $client->mkdir_unlink("$adb_dir/deep1/deep2/deep3/target") };
is($@, "", "no error from mkdir_unlink");
is(@result, 0, "Nothing returned on success") || dumper(\@result);
ok(!$client->connected, "We do however lose the connection");
$client->transport_local;
$client->sync;
@result = $client->list_dir("$adb_dir/deep1", recursive => 1, self_parent => 0);
is_deeply(\@result, [{
    "/sdcard/adb_client_test/deep1/deep2" => 1,
    "/sdcard/adb_client_test/deep1/deep2/deep3" => 1
}], "Directories now exist") || dumper(\@result);

# Check mkdir_unlink, create directory on top of a file
@result = eval { $client->mkdir_unlink("$adb_dir/foo/deeper") };
like($@, qr{^\Qcouldn't create file: Not a directory at },
     "Error from mkdir_unlink");
$client->transport_local;
$client->sync;

# Check mkdir_unlink, create disallowed directory
@result = eval { $client->mkdir_unlink("/deep1/deep2") };
like($@, qr{^\Qsecure_mkdirs failed: },
     "Error from mkdir_unlink");
$client->transport_local;
$client->sync;

# Check mkdir_unlink, unlink a file
@result = $client->lstat_v1("$adb_dir/bar");
is_deeply(\@result, [{    "ftype" => "REG",
    "mode" => 0100660,
    "mtime" => time_from_adb(0),
    "perms" => 0660,
    "size" => 8
}], "File $adb_dir/bar exists") || dumper(\@result);
@result = eval { $client->mkdir_unlink("$adb_dir/bar") };
is($@, "", "no error from mkdir_unlink");
is(@result, 0, "Nothing returned on success") || dumper(\@result);
$client->transport_local;
$client->sync;
@result = $client->lstat_v1("$adb_dir/bar");
is_deeply(\@result, [$noproto_native, $noproto_adb], "File is gone") ||
    dumper(\@result);

# Try to get a FAIL response while still sending
eval { $client->send_v1("$adb_dir/baz",
                        data => pack("(a4V/a*)2a4x4",
                             DEAD => "bar",
                             DATA => "z" x 40,
                             "DONE"),
                        raw => 1, low_water => 1, high_water => 10) };
like($@, qr{^\Qinvalid data message at }, "Normal error");

$client->transport_local;
$client->sync;
eval { $client->recv_v1("$adb_dir/dir") };
like($@, qr{^\Qread failed: Is a directory at }, "Fetch content of a directory");
