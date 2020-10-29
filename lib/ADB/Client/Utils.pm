package ADB::Client::Utils;
use strict;
use warnings;

our $VERSION = '1.000';

use Data::Dumper;
use Time::Local qw(timegm);
use Time::HiRes qw(clock_gettime CLOCK_REALTIME CLOCK_MONOTONIC);
use Socket qw(:addrinfo unpack_sockaddr_in unpack_sockaddr_in6 inet_ntop
              pack_sockaddr_in pack_sockaddr_in6
              SOCK_STREAM IPPROTO_TCP IPPROTO_UDP AF_INET AF_INET6 SOCK_DGRAM);
use IO::Socket qw();

use Exporter::Tidy
    other	=>[qw(addr_info info caller_info dumper string_from_value
                      display_string adb_check_response realtime clocktime
                      $BASE_REALTIME $BASE_CLOCKTIME $CLOCK_TYPE $DEBUG $VERBOSE
                      OKAY FAIL SUCCEEDED FAILED BAD_ADB ASSERTION INFINITY
                      DISPLAY_MAX)];

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
};

our ($DEBUG, $VERBOSE);

our $CLOCK_TYPE;
our $CLOCK_TYPE_NAME =
    eval { $CLOCK_TYPE = CLOCK_MONOTONIC; "MONOTONIC" } ||
    eval { $CLOCK_TYPE = CLOCK_REALTIME;  "REAL" } ||
    die "Time::HiRes doesn't even have CLOCK_REALTIME";

sub realtime {
    return clock_gettime(CLOCK_REALTIME);
}

sub clocktime {
    return clock_gettime($CLOCK_TYPE);
}

our $BASE_REALTIME  = realtime();
our $BASE_CLOCKTIME = clocktime();

sub addr_info {
    my ($host, $port, $no_die) = @_;

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
    my ($first_err, @address);
    for my $ai (@ai) {
        eval {
            my $udp = IO::Socket->new();
            $udp->socket($ai->{family}, SOCK_DGRAM, IPPROTO_UDP) || next;
            my ($bind_port, $b_addr, @rest) =
                $ai->{family} == AF_INET  ? unpack_sockaddr_in ($ai->{addr}) :
                $ai->{family} == AF_INET6 ? unpack_sockaddr_in6($ai->{addr}) :
                die "Assertion: Unknown family '$ai->{family}'";
            $bind_port || die "Invalid zero port";
            my $bind_addr0 =
                $ai->{family} == AF_INET  ? pack_sockaddr_in (0, $b_addr) :
                $ai->{family} == AF_INET6 ? pack_sockaddr_in6(0, $b_addr, @rest) :
                die "Assertion: Unknown family '$ai->{family}'";
            # Make sure we have a canonical form without garbage
            # cpan/Socket/Socket.xs zeroes the structure before filling it in
            my $bind_addr =
                $ai->{family} == AF_INET  ? pack_sockaddr_in ($bind_port, $b_addr) :
                $ai->{family} == AF_INET6 ? pack_sockaddr_in6($bind_port, $b_addr, @rest) :
                die "Assertion: Unknown family '$ai->{family}'";
            $udp->connect($ai->{addr}) ||
                die "Assertion: Could not connect UDP probe socket: $^E";
            my $connect_addr = $udp->peername //
                die "Assertion: No getpeername on connected UDP socket: $^E";
            my ($connect_port, $c_addr) =
                $ai->{family} == AF_INET  ? unpack_sockaddr_in ($connect_addr) :
                $ai->{family} == AF_INET6 ? unpack_sockaddr_in6($connect_addr) :
                die "Assertion: Unknown family '$ai->{family}'";
            push @address, {
                family		=> $ai->{family},
                bind_addr0	=> $bind_addr0,
                bind_addr	=> $bind_addr,
                bind_port	=> $bind_port,
                bind_ip		=> inet_ntop($ai->{family}, $b_addr),
                connect_addr	=> $connect_addr,
                connect_port	=> $connect_port,
                connect_ip	=> inet_ntop($ai->{family}, $c_addr),
            };
        };
        $first_err ||= $@;
    }
    if (!@address) {
        $err = $first_err ? "Could not resolve($host, $port): $first_err" :
            "No usable resolve for ($host, $port)";
        return $err if $no_die;
        die $err;
    }

    # dumper(\@address);
    return \@address;
}

sub info {
    local ($!, $^E);
    if (!@_) {
        my (undef, $filename, $line) = caller(1);
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

sub caller_info {
    my $format = shift;
    my (@lines, $file, $line, $i);
    $file =~ s{.*/}{}s, push @lines, "$file:$line" while (undef, $file, $line) = caller($i++);
    if (@_) {
        info("$format [line %s]", "@lines");
    } else {
        info("$format [line @lines]");
    }
}

sub dumper {
    local $Data::Dumper::Indent	  = 1;
    local $Data::Dumper::Sortkeys = 1;
    local $Data::Dumper::Useqq	  = 1;
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
        # status bytes were added ($data->{in} was shorter than 4 bytes)
        my $plen = $len < 4 ? $len : 4;
        my $prefix = substr($data->{in}, 0, $plen);
        if ($prefix ne substr(OKAY, 0, $plen) &&
                $prefix ne substr(FAIL, 0, $plen)) {
            # Answer will be neither OKAY nor FAIL
            $prefix = display_string($prefix);
            return BAD_ADB, "Bad prefix $prefix";
        }
    }

    if ($len < 4) {
        return if $len_added;
        my $prefix = display_string($data->{in});
        return BAD_ADB, "Truncated prefix $prefix";
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
                utf8::decode($response);
                return SUCCEEDED, $response;
            } else {
                my $response = display_string($data->{in});
                return BAD_ADB, "Truncated response $response";
            }
        }

        # Still too short
        $len >= 4+$nr || return;

        if ($len > 4+$nr && $expect_eof) {
            my $response = display_string($data->{in});
            return BAD_ADB, "Spurious bytes in response $response";
        }
        substr($data->{in}, 0, 4, "");
        my $response = substr($data->{in}, 0, $nr, "");
        utf8::decode($response);
        return SUCCEEDED, $response;
    }

    if ($len > 4 && $len - $len_added < 8) {
        # hex length bytes were added
        # ($data->{in} was shorter than 8 bytes and is now more than 4 bytes)
        my $code = substr($data->{in}, 4, $len < 8 ? $len-4 : 8-4);
        if ($code !~ /^[0-9a-fA-F]{1,4}\z/) {
            $code = display_string($code);
            return BAD_ADB, "Bad hex length $code";
        }
        return BAD_ADB, qq{Truncated hex length "$code"} if $len_added == 0;
    }

    if ($len < 8) {
        return if $len_added;
        my $prefix = display_string($data->{in});
        return BAD_ADB, "Truncated prefix $prefix";
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
        return BAD_ADB, "Truncated answer (expected $more, got $len bytes)";
    }
    # from here $len >= 8 + $more

    if ($len > 8 + $more && $expect_eof) {
        my $response = display_string($data->{in});
        return BAD_ADB, "Spurious bytes in response $response";
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
    utf8::decode($response);
    return $status, $response;
}

1;
