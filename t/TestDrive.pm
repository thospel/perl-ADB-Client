package TestDrive;
# $Id: TestDrive.pm 5091 2012-05-15 15:09:26Z hospelt $
## no critic (UselessNoCritic MagicNumbers)
use strict;
use warnings;

# Realy try not to get stuck
alarm 20;

our $VERSION = "1.000";

use Carp;
use FindBin qw($Bin);
use File::Temp qw(tempdir);
use Data::Dumper;
use IO::Socket::IP qw();
use IO::Socket qw(inet_ntoa pack_sockaddr_in inet_aton
                  AF_INET SOCK_DGRAM IPPROTO_UDP);
use Errno qw(EADDRNOTAVAIL ECONNREFUSED ETIMEDOUT);
use Storable qw(dclone);

BEGIN {
    croak "Loaded ADB::Client::Utils before TestDrive" if $INC{"ADB/Client/Utils.pm"};
    # Ignore environment variables already set before calling "make test"
    $ENV{ADB_CLIENT_ENV} = 0;
    $ENV{ADB_FAKE_ERROR} = "";
    $ENV{ADB_FAKE_SLEEP} = 0;
}

# We tested in 01_adb_check_response.t (with BAIL_OUT) that this can be used
use ADB::Client::Utils qw(display_string);
# We tested in t/02_adb_client (with BAIL_OUT) that this can be used
use ADB::Client qw($ADB);

# We tested in t/02_adb_client (with BAIL_OUT) that we can add these commands
ADB::Client->add_command(["failer" => "Wee", 0, 1]);
ADB::Client->add_command(["echo" => "internal:echo:%s", -1, 1]);
ADB::Client->add_command(["device_drop" => "internal:device_drop:%s", -1, 1]);
ADB::Client->add_command(["pid" => "internal:pid", -1, 1]);

BEGIN {
    $INC{"Test/More.pm"} || croak "Must use Test::More before using TestDrive";
}

use Exporter::Tidy
    other =>
    [qw($Bin $tmp_dir $t_dir $base_dir $old_stderr %expect_objects
        $TRANSACTION_TIMEOUT $CONNECTION_TIMEOUT $UNREACHABLE
        adb_start adb_stop adb_unacceptable adb_unreachable adb_closer adb_echo
        adb_version adb_blackhole remote_ip is_remote4 addr_filter
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
$ADB = "$base_dir/bin/adb_fake";

my ($adb_out, $adb_control, $pid);
my $ECONNREFUSED = $! = ECONNREFUSED;
my $ETIMEDOUT    = $! = ETIMEDOUT;

our $TRANSACTION_TIMEOUT = $ENV{ADB_CLIENT_TEST_TRANSACTION_TIMEOUT} || 0.5;
our $CONNECTION_TIMEOUT = $ENV{ADB_CLIENT_TEST_CONNECTION_TIMEOUT} // undef;
# 192.0.2.0/24 is assigned to TEST-NET-1
# Hopefully that gives us an unreachable IP address, but maybe firewall rules
# will fake a connection refused
our $UNREACHABLE = $ENV{ADB_CLIENT_TEST_UNREACHABLE} || "192.0.2.1";
# State globals
our $tmp_dir;
our $old_stderr;

# Classes for which we count bless/destruct cycles
my @object_classes = qw(ADB::Client ADB::Client::Ref ADB::Client::Command
                        ADB::Client::Starter ADB::Client::StarterRef);
our %expect_objects;
$expect_objects{$_} = 0 for @object_classes;

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
    for my $class (@object_classes) {
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

sub is_remote4 {
    my ($ip) = @_;
    my $addr = inet_aton($ip) || croak "Cannot resolve '$ip'";
    socket(my $udp, AF_INET, SOCK_DGRAM, IPPROTO_UDP) ||
        die "Could not create AF_INET udp socket: $^E";
    return undef if bind($udp, pack_sockaddr_in(0, $addr));
    $! == EADDRNOTAVAIL || die "Could not bind UDP socket: $^E";
    return inet_ntoa($addr);
}

sub remote_ip {
    my $addr = inet_aton("1.2.3.4");
    for (0..999) {
        my $ip = inet_ntoa($addr) ||
            die "Assertion: Invalid packed IP ", unpack("N", $addr);
        return $ip if is_remote4($ip);
        $addr = pack("N", rand((224-1)*2**24)+2**24);
    }
    die "Could not determine any non local IP. It seems I can bind to anything";
}

sub adb_start {
    my $blib = grep $_ eq "$base_dir/blib/lib", @INC;

    $pid = do {
        local $SIG{__WARN__} = sub {};
        # open($adb_out = undef, "-|", "$base_dir/bin/adb_fake")
        open($adb_out = undef, "-|", $^X, "$base_dir/bin/adb_fake", $blib ? "--blib" : ()) ||
            Test::More::BAIL_OUT("Cannot even start fake adb server: $^E");
    };
    my $line = <$adb_out> //
        Test::More::BAIL_OUT("Unexpected EOF from fake adb server");
    my $regex = qr{^Port: ([1-9][0-9]*)\n\z};
    Test::More::like($line, $regex, "Control port on fake adb server") ||
          Test::More::BAIL_OUT("Could seemingly start fake adb server, but got unexpected ouptut: " .display_string($line));
    $line =~ $regex || die "Assertion: Inconsistent match";
    my $port = int($1);
    Test::More::ok(0 < $port && $port < 65536, "Proper port range") ||
          Test::More::BAIL_OUT("Could seemingly start fake adb server, invalid control port $port");

    $adb_control = IO::Socket::IP->new(
        PeerHost => "127.0.0.1",
        PeerPort => $port) ||
            Test::More::BAIL_OUT("Could seemingly start fake adb server, but cannot connect to control 127.0.0.1:$port: $@");
    $line = <$adb_control> //
        Test::More::BAIL_OUT("Unexpected EOF from fake adb server control");
    $line =~ /^adb_fake [1-9][0-9]*\.[0-9]{3}\n\z/ ||
        Test::More::BAIL_OUT("Unexpected EOF from greeting fake adb server control: $line");
    return adb_version();
}

sub adb_stop {
    if (!@_ || !$_[0]) {
        $adb_control || return "Already stopped";
        close($adb_control);
        $adb_control = undef;
        return "" if @_;
    }

    if (!@_ || $_[0]) {
        $adb_out || return "Already stopped";
        my $exit = do { local $/; <$adb_out> };
        close($adb_out);
        $adb_out = undef;
        return Test::More::is($exit, "Close: 0 0 0\n", "Expect final status from fake adb");
    }
}

sub _adb_listener {
    print $adb_control "@_\n";
    my $line = <$adb_control> //
        Test::More::BAIL_OUT("Unexpected EOF from fake server");
    my $regex = qr{^Port: ([1-9][0-9]*)\n\z};
    Test::More::like($line, $regex, "Could start @_") ||
          Test::More::BAIL_OUT("Improper reply from fake server: $line");
    $line =~ $regex || die "Invalid Port answer";
    my $port = int($1);
    Test::More::ok(0 < $port && $port < 65536, "Proper port range") ||
          Test::More::BAIL_OUT("Improper port from fake  adb server: $port");
    return $port;
}

sub adb_unacceptable {
    return _adb_listener("Unacceptable");
}

sub adb_unreachable {
    return _adb_listener("Unreachable");
}

sub adb_closer {
    return _adb_listener("Closer");
}

sub adb_echo {
    return _adb_listener("Echo");
}

sub adb_version {
    return _adb_listener("Listener" . (@_ ? " @_" : ""));
}

sub adb_blackhole {
    my $arg = $_[0] || "";
    $arg =~s /\n/\\n/g;
    return _adb_listener("Blackhole" . (defined $_[0] ? " $arg" : ""));
}

sub addr_filter {
    my ($addr_info) = @_;

    return $addr_info if ref $addr_info eq "";

    my $ais = dclone($addr_info);
    $ais = [$ais] if UNIVERSAL::isa($addr_info, "HASH");
    for my $ai (@$ais) {
        for my $name (qw(bind_addr bind_addr0 connect_addr)) {
            if (!delete $ai->{$name}) {
                local $Data::Dumper::Indent	  = 1;
                local $Data::Dumper::Sortkeys = 1;
                local $Data::Dumper::Useqq	  = 1;
                local $Data::Dumper::Terse	  = 1;
                my $dump = Dumper($addr_info);
                $dump =~ s/\s+\z//;
                die "No $name in addr_info element: $dump";
            }
            $ai->{connected} = 1 if $ai->{connected};
            $ai->{pid} = 1 if $ai->{pid};
            if ($ai->{last_connect_error}) {
                for ($ai->{last_connect_error}) {
                    m{^Connect error: } || next;
                    next if s{^Connect error: \Q$ECONNREFUSED\E\z}{Connect error: Connection refused};
                    next if s{^Connect error: \Q$ETIMEDOUT\E\z}{Connect error: Connection timed out};
                    die "Cannot normalize connection error '$_'";
                }
            }
        }
    }
    return UNIVERSAL::isa($addr_info, "HASH") ? $ais->[0] : $ais;
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
