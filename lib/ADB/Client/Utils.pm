package ADB::Client::Utils;
use strict;
use warnings;

our $VERSION = '1.000';

use Data::Dumper;
use File::Spec;
use Time::Local qw(timegm);
use Time::HiRes qw(clock_gettime CLOCK_REALTIME CLOCK_MONOTONIC);
use Errno qw(ENOTCONN EPROTONOSUPPORT ENOPROTOOPT);
use Socket qw(:addrinfo unpack_sockaddr_in unpack_sockaddr_in6 inet_ntop
              inet_pton inet_aton sockaddr_family pack_sockaddr_in
              pack_sockaddr_in6
              SOCK_STREAM IPPROTO_TCP IPPROTO_UDP AF_INET AF_INET6 SOCK_DGRAM
              SOL_SOCKET SO_ACCEPTCONN);
use Scalar::Util qw(dualvar);
our $SO_ACCEPTCONN = eval { SO_ACCEPTCONN };
use Carp;
our @CARP_NOT = qw(ADB::Client::Ref);

use Exporter::Tidy
    other =>[qw(addr_info adb_addr_info info caller_info callers dumper
                string_from_value display_string adb_check_response realtime
                clocktime realtime_running clocktime_running ip_port_from_addr
                addr_from_ip_port is_listening get_home errno_native time_to_adb
                time_from_adb
                _native adb_file_type_from_mode adb_permissions_from_mode
                $me %errno_adb %errno_native %adb_mode_from_file_type
                %adb_file_type_from_mode
                $BASE_REALTIME $BASE_CLOCKTIME $CLOCK_TYPE $SO_ACCEPTCONN
                $DEBUG $VERBOSE $QUIET $ADB_HOST $ADB_PORT $ADB_SERIAL
                ADB_FILE_TYPE_MASK ADB_PERMISSION_MASK OKAY FAIL SUCCEEDED
                FAILED BAD_ADB ASSERTION INFINITY DISPLAY_MAX EPOCH0 IPV6)];

use constant {
    # Code assumes OKAY and FAIL both have length 4, so you can't change this
    OKAY		=> "OKAY",
    FAIL		=> "FAIL",
    DISPLAY_MAX		=> 60,
    SUCCEEDED		=> 0,	# This one must be the only false value
    FAILED		=> 1,
    BAD_ADB		=> 2,
    ASSERTION		=> 3,
    INFINITY		=> 9**9**9,
    EPOCH0		=> Time::Local::timegm(0,0,0,1,0,1970),
    IPV6		=>
    socket(my $s, AF_INET6, SOCK_STREAM, IPPROTO_TCP) ? 1 :
    $! == EPROTONOSUPPORT ? 0 :
    die("Cannot create socket: $^E"),
};

our ($DEBUG, $VERBOSE, $QUIET);

our $ADB_CLIENT_ENV = $ENV{ADB_CLIENT_ENV} // 1 || undef;
our $ADB_HOST	= $ADB_CLIENT_ENV && $ENV{ANDROID_ADB_SERVER_ADDRESS} // "127.0.0.1";
our $ADB_PORT	= $ADB_CLIENT_ENV && $ENV{ANDROID_ADB_SERVER_PORT} // 5037;
our $ADB_SERIAL	= $ADB_CLIENT_ENV && $ENV{ANDROID_SERIAL} || undef;

our $CLOCK_TYPE;
our $CLOCK_TYPE_NAME =
    eval { $CLOCK_TYPE = CLOCK_MONOTONIC; "MONOTONIC" } ||
    eval { $CLOCK_TYPE = CLOCK_REALTIME;  "REAL" } ||
    die "Time::HiRes doesn't even have CLOCK_REALTIME";

# This can skip
sub realtime {
    return clock_gettime(CLOCK_REALTIME);
}

# This cannot skip (if we have CLOCK_MONOTONIC)
sub clocktime {
    return clock_gettime($CLOCK_TYPE);
}

# Moment the program started (or at least the module was loaded)
our $BASE_REALTIME  = realtime();
our $BASE_CLOCKTIME = clocktime();

# time relative to program start (or module load)
sub realtime_running {
    return clock_gettime(CLOCK_REALTIME) - $BASE_REALTIME;
}

sub clocktime_running {
    return clock_gettime($CLOCK_TYPE) - $BASE_CLOCKTIME;
}

sub time_to_adb {
    return shift() - EPOCH0;
}

sub time_from_adb {
    return shift() + EPOCH0;
}

our $me;
if ($^O eq "MSWin32") {
    require Win32;
    $me = Win32::LoginName();
} else {
    if (my $user = $ENV{LOGNAME}) {
        if (defined(my $uid = getpwnam($user))) {
            $me = $user if $> == $uid;
        }
    }
    $me ||= getpwuid $>;
}
die "Can't determine who I am" if !$me;
# We can basically trust $me since it came from a real system request
# Still, let's filter some weird characters
die "Unacceptable userid '$me'" if $me eq "." || $me eq "..";
$me =~ /^([0-9A-Za-z_.-]+)\z/ || die "Weird characters in userid '$me'";
# Seems ok. Untaint
$me = $1;	## no critic (UselessNoCritic CaptureWithoutTest)

# Determine HOME directory
sub get_home() {
    my $home = $ENV{HOME};
    if (!$home) {
        if ($^O eq "MSWin32") {
            croak "Cannot determine the home directory since user '$me' has neither the 'HOME' nor the 'UserProfile' environment variable" if !$ENV{UserProfile};
            $home = $ENV{UserProfile};
        } else {
            my @user_props = getpwuid($>) or
                croak "Could not get userinfo for '$>'";
            $home = $user_props[7] || croak "User $> has no home";
        }
    }
    $home =~ tr{\\}{/} if $^O eq "MSWin32";
    $home =~ s{/+\z}{};
    $home = "/" if $home eq "";

    # Due to the way the previous tests are done we know $home is not empty
    # Check and untaint
    ## no critic (UselessNoCritic CaptureWithoutTest)
    $home =~ m{^([\x20-\x7f]+)\z} || do {
        $home =~ m{([^\x20-\x7f])};
        croak sprintf("HOME variable '%s' contains unacceptable character '%s' (\\x%02x)", $home, $1, ord($1));
    };
    $home = $1;
    File::Spec->file_name_is_absolute($home) ||
        croak "Home directory '$home' is not absolute";
    # Downgrade failure should be impossible
    utf8::downgrade($home);
    $ENV{HOME} = $home;
    # utf8::decode($home);
    return $home;
}

sub addr_info {
    my ($host, $port, $no_die) = @_;

    $host //= $ADB_HOST;
    $port //= $ADB_PORT;
    my ($err, @ai) = getaddrinfo($host, $port, {
        socktype	=> SOCK_STREAM,
        protocol	=> IPPROTO_TCP,
        flags		=> AI_PASSIVE,
    });
    if ($err) {
        $err = "Could not resolve($host, $port): $err";
        return $err if $no_die;
        die $err;
    }
    my $address = $no_die ? eval { adb_addr_info(\@ai) } : adb_addr_info(\@ai);
    # dumper($address);
    return $address if $address;
    $err = $@;
    $err =~ s/ at .*//s;
    return $err;
}

sub adb_addr_info {
    my ($ais) = @_;

    @$ais || die "Empty address info list";
    my @address;
    for my $ai (@$ais) {
        socket(my $udp, $ai->{family}, SOCK_DGRAM, IPPROTO_UDP) ||
            die "Could not create AF_INET UDP socket: $^E";
        my ($bind_ip, $bind_port, $family, @rest) =
            ip_port_from_addr($ai->{addr});
        $bind_port || die "Invalid zero port\n";

        # Address with unspecified port
        # Used to try a local UDP bind to see if this address is local
        # (we don't want to clash with an existing port)
        my $bind_addr0 = addr_from_ip_port($bind_ip, 0, @rest);

        # Make sure we have a canonical form without garbage
        # cpan/Socket/Socket.xs zeroes the structure before filling it in
        my $bind_addr = addr_from_ip_port($bind_ip, $bind_port, @rest);

        my $connect_addr = addr_from_ip_port(
            $bind_ip eq "0.0.0.0" ? "127.0.0.1" :
            $bind_ip eq "::"      ? "::1" :
            $bind_ip, $bind_port, @rest);
        connect($udp, $connect_addr) ||
            die "Assertion: Could not connect UDP probe socket: $^E\n";
        $connect_addr = getpeername($udp) ||
            die "Assertion: Could not getpeername on connected UDP socket: $^E\n";
        my ($connect_ip, $connect_port) = ip_port_from_addr($connect_addr);

        my $connect_addr0;
        if (0) {
            $connect_addr0 = getsockname($udp) ||
                die "Assertion: No getsockname on connected UDP socket: $^E\n";
            my ($ip, undef, undef, @rest) = ip_port_from_addr($connect_addr0);
            $connect_addr0 = addr_from_ip_port($ip, 0, @rest);
            # dumper([ip_port_from_addr($connect_addr), ip_port_from_addr($connect_addr0)]);
        }
        push @address, {
            family		=> $ai->{family},
            bind_addr0		=> $bind_addr0,
            bind_addr		=> $bind_addr,
            bind_port		=> $bind_port,
            bind_ip		=> $bind_ip,
            $connect_addr0 ? (connect_addr0	=> $connect_addr0) : (),
            connect_addr	=> $connect_addr,
            connect_port	=> $connect_port,
            connect_ip		=> $connect_ip,
        };
    }
    return \@address;
}

sub ip_port_from_addr {
    my ($addr) = @_;

    my $family = sockaddr_family($addr);
    my ($port, $packed, @more) =
        $family == AF_INET  ? unpack_sockaddr_in ($addr) :
        $family == AF_INET6 ? unpack_sockaddr_in6($addr) :
        croak "Unhandled family '$family'";
    return inet_ntop($family, $packed), $port, $family, @more;
}

sub addr_from_ip_port {
    my $ip   = shift;
    my $port = shift;
    if (my $addr = inet_aton($ip)) {
        return pack_sockaddr_in($port, $addr);
    }
    return pack_sockaddr_in6($port, inet_pton(AF_INET6, $ip), @_);
}

sub is_listening {
    my ($socket) = @_;

    fileno($socket) // croak "Not a filehandle";

    if (defined $SO_ACCEPTCONN) {
        # Failure means it's not a socket or it's not a type that can listen
        my $packed = getsockopt($socket, SOL_SOCKET, SO_ACCEPTCONN) ||
            croak "Could not getsockopt(SOL_SOCKET, SO_ACCEPTCONN): $^E";
        return unpack("I", $packed);
    }
    # On systems Without SO_ACCEPTCONN we cannot determine if a socket listens
    # But we can at least do some sanity checks

    # If it is listening it cannot be connected
    return 0 if getpeername($socket);
    # Should only call this on sockets
    $! == ENOTCONN || croak "Could not getpeername: $^E";

    # Check the local address.
    # die instead of croak since getpeername already checked sanity of $socket
    my $addr = getsockname($socket) ||
        die "Could not getsockname: $^E";
    my (undef, $port) = ip_port_from_addr($addr);
    # If no port is set this socket isn't bound
    $port || return 0;
    # Should we try to connect to it ?
    # Can that be blocked by OS rules leading to timeouts ?
    return 1;
}

sub info {
    local ($!, $^E);
    if (!@_) {
        my (undef, $filename, $line) = caller(0);
        @_ = ("$filename $line");
    }
    my $format = shift;
    $format =~ s/\n?\z/\n/;
    if (!@_) {
        @_ = ($format);
        $format = "%s";
    }
    my $time = realtime;
    my $itime = int($time);
    my ($sec, $min, $hour, $day, $mon, $year) = localtime($itime);
    # This code didn't exist before 2000, so $year >= 100
    my $gtime = timegm($sec, $min, $hour, $day, $mon, $year);
    my $offset = ($gtime - $itime) / 60;
    my $sign = "+";
    if ($offset < 0) {
        $sign = "-";
        $offset = -$offset;
    }
    my $hoffset = $offset / 60;
    my $moffset = $offset % 60;
    printf(STDERR "%04d-%02d-%02d %02d:%02d:%06.3f %s%02d%02d: $format",
           $year+1900, $mon+1, $day, $hour, $min, $time-$itime+$sec,
           $sign, $hoffset, $moffset,
           @_);
}

sub callers {
    my (@lines, $file, $line, $i);
    # Skip the entry for callers itself, so start $i at 1
    $file =~ s{.*/}{}s, push @lines, "$file:$line" while (undef, $file, $line) = caller(++$i);
    return join(" ", @lines);
}

sub caller_info {
    my $format = shift;
    if (@_) {
        info("$format [%s]", @_, callers());
    } else {
        my $callers = callers();
        $callers =~ s{%}{%%}g;
        info("$format [$callers]");
    }
}

sub dumper {
    local $Data::Dumper::Indent	  = 1;
    local $Data::Dumper::Sortkeys = 1;
    local $Data::Dumper::Useqq	  = 1;
    local $Data::Dumper::Terse    = 1;
    print STDERR Dumper(@_);
}

sub string_from_value {
    local $Data::Dumper::Indent	  = 0;
    local $Data::Dumper::Sortkeys = 1;
    local $Data::Dumper::Useqq	  = 1;
    local $Data::Dumper::Trailingcomma = 0;
    # local $Data::Dumper::Varname  = "VAR";
    local $Data::Dumper::Terse = 1;
    local $Data::Dumper::Quotekeys = 0;
    local $Data::Dumper::Sparseseen = 1;
    return Dumper(shift);
}

sub display_string {
    my $str = shift // return "undef";
    return "$str" if ref $str;
    return string_from_value($str) if length $str <= DISPLAY_MAX;
    return string_from_value(substr($str, 0, DISPLAY_MAX)) . "...";
}

# We assume $len_added (number of just added bytes) > 0
# Also length $data->{in} <= $len_added
# $data->{in} will be modified for SUCCEEDED and FAILED
# No modification for other non empty cases (caller should discard $data->{in})
sub adb_check_response {
    my ($data, $len_added, $nr, $expect_eof) = @_;

    return ASSERTION, "Assertion: negative input" if $len_added < 0;

    my $len = length $data->{in};

    if ($len - $len_added < 4) {
        # status bytes were added ($data->{in} used to be shorter than 4 bytes)
        my $plen = $len < 4 ? $len : 4;
        my $prefix = substr($data->{in}, 0, $plen);
        if ($prefix ne substr(OKAY, 0, $plen) &&
                $prefix ne substr(FAIL, 0, $plen)) {
            # Answer will be neither OKAY nor FAIL
            $prefix = display_string($prefix);
            return BAD_ADB, "Bad ADB status $prefix";
        }
    }

    if ($len < 4) {
        return if $len_added;
        return BAD_ADB, "Unexpected EOF" if $len == 0;
        my $prefix = display_string($data->{in});
        return BAD_ADB, "Truncated ADB status $prefix";
    }
    # From here $len >= 4

    my $status = substr($data->{in}, 0, 4);
    # Invariant: $response = OKAY or $response = FAIL
    # But only with correct use where you keep calling this function on the
    # growing string. You CAN call this function out of the blue and break this
    if ($status ne OKAY) {
        $nr = -1;
        $expect_eof = 1;
    }
    if ($nr >= 0) {
        if ($len - $len_added >= 4+$nr) {
            my $response = display_string($data->{in});
            return ASSERTION, "Assertion: Should already have processed $response";
        }
        if ($len_added == 0) {
            # Implies $len < 4+$nr
            if ($nr == INFINITY) {
                substr($data->{in}, 0, 4, "");
                my $response = substr($data->{in}, 0, $len-4, "");
                return SUCCEEDED, $response;
            } else {
                my $response = display_string($data->{in});
                return BAD_ADB, "Truncated ADB response $response";
            }
        }

        # Still too short
        $len >= 4+$nr || return;

        if ($len > 4+$nr && $expect_eof) {
            my $response = display_string($data->{in});
            return BAD_ADB, "Spurious bytes in ADB response $response";
        }
        substr($data->{in}, 0, 4, "");
        my $response = substr($data->{in}, 0, $nr, "");
        return SUCCEEDED, $response;
    }

    if ($len > 4 && $len - $len_added < 8) {
        # hex length bytes were added
        # ($data->{in} was shorter than 8 bytes and is now more than 4 bytes)
        my $code = substr($data->{in}, 4, $len < 8 ? $len-4 : 8-4);
        if ($code !~ /^[0-9a-fA-F]{1,4}\z/) {
            $code = display_string($code);
            return BAD_ADB, "Bad ADB hex length $code";
        }
        return BAD_ADB, qq{Truncated hex length "$code"} if $len_added == 0;
    }

    if ($len < 8) {
        return if $len_added;
        my $prefix = display_string($data->{in});
        return BAD_ADB, "Truncated ADB response $prefix";
    }
    # from here $len >= 8

    my $code = substr($data->{in}, 4, 4);
    if ($code !~ /^[0-9a-fA-F]{4}\z/) {
        $code = display_string($code);
        return ASSERTION, "Assertion: adb hex length is suddenly $code";
    }
    my $more = hex $code;
    if ($len - $len_added >= 8 + $more) {
        my $response = display_string($data->{in});
        return ASSERTION, "Assertion: Should already have processed $response";
    }
    if ($len < 8 + $more) {
        return if $len_added;
        $len -= 8;
        return BAD_ADB, "Truncated ADB response (expected $more, got $len bytes)";
    }
    # from here $len >= 8 + $more

    if ($len > 8 + $more && $expect_eof) {
        my $response = display_string($data->{in});
        return BAD_ADB, "Spurious bytes in ADB response $response";
    }

    $status =
        $status eq OKAY ? SUCCEEDED :
        $status eq FAIL ? FAILED :
        do {
            $status = display_string($status);
            return ASSERTION, "Assertion: adb response status is suddenly $status";
        };
    substr($data->{in}, 0, 8, "");
    my $response = substr($data->{in}, 0, $more, "");
    return $status, $response;
}

# These are the android errno numbers and texts.
# They are the same as on linux
our %_errno_adb = (
    EACCES	=> [13, "Permission denied"],
    EEXIST	=> [17, "File exists"],
    EFAULT	=> [14, "Bad address"],
    EFBIG	=> [27, "File too large"],
    EINTR	=> [4, "Interrupted system call"],
    EINVAL	=> [22, "Invalid argument"],
    EIO		=> [5, "Input/output error"],
    EISDIR	=> [21, "Is a directory"],
    ELOOP	=> [40, "Too many levels of symbolic links"],
    EMFILE	=> [24, "Too many open files"],
    ENAMETOOLONG=> [36, "File name too long"],
    ENFILE	=> [23, "Too many open files in system"],
    ENOENT	=> [2, "No such file or directory"],
    ENOMEM	=> [12, "Cannot allocate memory"],
    ENOPROTOOPT	=> [92, "Protocol not available"],
    ENOSPC	=> [28, "No space left on device"],
    ENOTDIR	=> [20, "Not a directory"],
    EOVERFLOW	=> [75, "Value too large for defined data type"],
    EPERM	=> [1, "Operation not permitted"],
    EROFS	=> [30, "Read-only file system"],
    ETXTBSY	=> [26, "Text file busy"],
);

our %errno_native = our %errno_adb = (0 => dualvar(0, ""));
while (my ($key, $value) = each %_errno_adb) {
    my $code = Errno->can($key) || die "Unknown errno $key";
    my $err = local $! = eval { Errno->$key } || ENOPROTOOPT;
    $errno_adb{$value->[0]} = dualvar($value->[0], $value->[1]);
    $errno_native{$value->[0]} = local $! = $code->();
}

sub errno_native {
    return $errno_native{0+shift};
}

use constant {
    ADB_FILE_TYPE_MASK  => 0170000,
    ADB_PERMISSION_MASK	=> 07777,
};

our %adb_mode_from_file_type = (
    SOCK	=> 0140000,
    LNK		=> 0120000,
    REG		=> 0100000,
    BLK		=> 0060000,
    DIR		=> 0040000,
    CHR		=> 0020000,
    FIFO	=> 0010000,
);

our %adb_file_type_from_mode = reverse %adb_mode_from_file_type;

sub adb_file_type_from_mode {
    my $mode = shift // croak "Undefined mode";
    return $adb_file_type_from_mode{$mode & ADB_FILE_TYPE_MASK} //
        croak sprintf "Cannot get file_type from %o", $mode;
}

sub adb_permissions_from_mode {
    return shift() & ADB_PERMISSION_MASK;
}

1;
