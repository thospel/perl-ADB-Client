package TestDrive;
# $Id: TestDrive.pm 5091 2012-05-15 15:09:26Z hospelt $
## no critic (UselessNoCritic MagicNumbers)
use strict;
use warnings;

our $VERSION = "1.000";

use Carp;
use FindBin qw($Bin);
use IPC::Open2;
use File::Temp qw(tempdir);
use Data::Dumper;

# We tested in 01_adb_check_response.t (with BAIL_OUT) that this can be used
use ADB::Client::Utils qw(display_string);
# We tested in t/02_adb_client (with BAIL_OUT) that this can be used
use ADB::Client;

# We tested in t/02_adb_client (with BAIL_OUT) that we can add these commands
ADB::Client->add_command(["failer" => "Wee", 0, 1]);
ADB::Client->add_command(["echo" => "host:echo:%s", -1, 1]);

BEGIN {
    $INC{"Test/More.pm"} || croak "Must use Test::More before using TestDrive";
}

use Exporter::Tidy
    other =>
    [qw($Bin $tmp_dir $t_dir $base_dir $old_stderr %expect_objects
        $TRANSACTION_TIMEOUT $CONNECTION_TIMEOUT $UNREACHABLE
        adb_start adb_stop adb_unacceptable adb_unreachable adb_closer
        adb_version adb_blackhole
        collect_stderr collected_stderr uncollect_stderr dumper)];

$SIG{INT} = sub {
    Test::More::diag("Caught signal INT");
    Test::More::fail("Signal");
    exit 1;
};

$Bin =~ s{/+\z}{};
our $t_dir = $Bin;
our $base_dir = $t_dir;
$base_dir =~ s{/t\z}{} ||
    croak "test directory $t_dir does not seem to end on /t";

my ($from_adb, $to_adb, $pid);

our $TRANSACTION_TIMEOUT = $ENV{ADB_CLIENT_TEST_TRANSACTION_TIMEOUT} || 0.5;
our $CONNECTION_TIMEOUT = $ENV{ADB_CLIENT_TEST_CONNECTION_TIMEOUT}   // undef;
# 192.0.2.0/24 is assigned to TEST-NET-1
# Hopefully that gives us an unreachable IP address, but maybe firewall rules
# will fake a connection refused
our $UNREACHABLE = $ENV{ADB_CLIENT_TEST_UNREACHABLE} || "192.0.2.1";
# State globals
our $tmp_dir;
our $old_stderr;
our %expect_objects = (
    "ADB::Client"	=> 0,
    "ADB::Client::Ref"	=> 0,
    "ADB::Client::Command"	=> 0,
);

my $warnings = 0;
$SIG{__WARN__} = sub {
    $warnings++;
    my $string = shift;
    chomp $string;
    Test::More::diag("Perl Warning: $string");
};

END {
    local $?;
    if ($old_stderr) {
        Test::More::diag("STDERR was still redirected");
        uncollect_stderr();
    }
    adb_stop();
    Test::More::is($warnings, 0, "No warnings");
    for my $class (qw(ADB::Client ADB::Client::Ref ADB::Client::Command)) {
        Test::More::is($class->objects, $expect_objects{$class}, "Cleaned up all $class objects");
    }
}

sub collect_stderr {
    $tmp_dir ||= tempdir(CLEANUP => 1);
    croak "Already collecting STDERR" if $old_stderr;
    open($old_stderr, ">&", "STDERR") || die "Can't dup STDERR: $!";
    open(STDERR, ">", "$tmp_dir/stderr") ||
        die "Could not redirect STDERR to '$tmp_dir/stderr': $!";
    my $fh = select(STDERR);
    $| = 1;
    select($fh);
}

sub uncollect_stderr {
    croak "Not collecting STDERR" if !$old_stderr;
    my $collected_stderr = collected_stderr();
    open(STDERR, ">&", $old_stderr) || die "Can't dup \$old_stderr: $!";
    $old_stderr = undef;
    return $collected_stderr;
}

sub collected_stderr {
    open(my $fh, "<", "$tmp_dir/stderr") ||
        die "Could not open '$tmp_dir/stderr': $!";
    my $old = do { local $/; <$fh> };
    # unlink("$tmp_dir/stderr");
    return $old;
}

sub adb_start {
    $pid = eval {
        local $SIG{__WARN__} = sub {};
        open2(my $adb_out, my $adb_in, "$base_dir/bin/adb_fake");
        $from_adb = $adb_out;
        $to_adb = $adb_in;
        $to_adb->autoflush;
    };
    Test::More::BAIL_OUT("Cannot even start fake adb server: $@") if $@;
    my $line = <$from_adb> //
        Test::More::BAIL_OUT("Could seemingly start fake adb server, but got no ouptut");
    my $regex = qr{^Port: ([1-9][0-9]*)\n\z};
    Test::More::like($line, $regex, "Could not start Listening port fake adb server") ||
          Test::More::BAIL_OUT("Could seemingly start fake adb server, but got unexpected ouptut: " .display_string($line));
    $line =~ $regex || die "Could not start Listening port";
    my $port = int($1);
    Test::More::ok(0 < $port && $port < 65536, "Proper port range") ||
          Test::More::BAIL_OUT("Could seemingly start fake adb server, invalid port $port");
    return $port;
}

sub adb_stop {
    if (!@_ || !$_[0]) {
        $to_adb || return "Already stopped";
        close($to_adb);
        $to_adb = undef;
        return "" if @_;
    }

    if (!@_ || $_[0]) {
        $from_adb || return "Already stopped";
        my $exit = do { local $/; <$from_adb> };
        close($from_adb);
        $from_adb = undef;
        return Test::More::is($exit, "Close: 0 0\n", "Expect final status from fake adb");
    }
}

sub _adb_unacceptable {
    print $to_adb "@_\n";
    my $line = <$from_adb>;
    my $regex = qr{^Port: ([1-9][0-9]*)\n\z};
    Test::More::like($line, $regex, "Could start @_") ||
                         die "Invalid Port answer";
    $line =~ $regex || die "Invalid Port answer";
    my $port = int($1);
    Test::More::ok(0 < $port && $port < 65536, "Proper port range") ||
          Test::More::BAIL_OUT("Could seemingly start fake adb server, invalid port $port");
    return $port;
}

sub adb_unacceptable {
    return _adb_unacceptable("Unacceptable");
}

sub adb_unreachable {
    return _adb_unacceptable("Unreachable");
}

sub adb_closer {
    return _adb_unacceptable("Closer");
}

sub adb_version {
    return _adb_unacceptable("Listener" . (@_ ? " @_" : ""));
}

sub adb_blackhole {
    my $arg = $_[0] || "";
    $arg =~s /\n/\\n/g;
    return _adb_unacceptable("Blackhole" . (defined $_[0] ? " $arg" : ""));
}

sub dumper {
    local $Data::Dumper::Indent	  = 1;
    local $Data::Dumper::Sortkeys = 1;
    local $Data::Dumper::Useqq	  = 1;
    local $Data::Dumper::Terse	  = 1;

    Test::More::diag("Dumped variable:");
    print STDERR Dumper(@_);
}

1;
