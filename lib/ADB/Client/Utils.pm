package ADB::Client::Utils;
use strict;
use warnings;

our $VERSION = '1.000';

use Data::Dumper;
use Time::Local qw(timegm);
use Socket qw(:addrinfo unpack_sockaddr_in unpack_sockaddr_in6 inet_ntop
              pack_sockaddr_in pack_sockaddr_in6
              SOCK_STREAM IPPROTO_TCP IPPROTO_UDP AF_INET AF_INET6 SOCK_DGRAM);
use IO::Socket qw();

use Exporter::Tidy
    other	=>[qw(addr_info info caller_info dumper $DEBUG $VERBOSE)];

our ($DEBUG, $VERBOSE);

sub addr_info {
    my ($host, $port) = @_;

    my ($err, @ai) = getaddrinfo($host, $port, {
        socktype	=> SOCK_STREAM,
        protocol	=> IPPROTO_TCP,
        flags		=> AI_PASSIVE,
    });
    die "Could not resolve($host, $port): $err" if $err;
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
    @address || die $first_err || "No usable resolve for ($host, $port)";
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
    my $time = ADB::Client::Timer->realtime;
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
    my (@lines, $line, $i);
    push @lines, $line while $line = (caller($i++))[2];
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

1;
