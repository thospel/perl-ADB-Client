package ADB::Client::Ref;
use strict;
use warnings;

our $VERSION = '1.000';

use Carp;
use Scalar::Util qw(weaken refaddr);
# use IO::Socket::IP;
use Errno qw(EINPROGRESS EWOULDBLOCK EINTR EAGAIN);

use ADB::Client::Events qw(mainloop unloop loop_level realtime clocktime
                           $BASE_REALTIME $BASE_CLOCKTIME $CLOCK_TYPE);
use ADB::Client::ServerStart;
use ADB::Client::Utils qw(addr_info info caller_info dumper $DEBUG $VERBOSE);

use Exporter::Tidy
    other	=>[qw(addr_info mainloop unloop loop_level dumper
                      info caller_info realtime clocktime
                      $BASE_REALTIME $BASE_CLOCKTIME $CLOCK_TYPE
                      $CALLBACK_DEFAULT
                      $ADB_HOST $ADB_PORT $ADB $ADB_SOCKET $DEBUG $VERBOSE)];

our @CARP_NOT = qw(ADB::Client);

our $BLOCK_SIZE = 65536;

our $CALLBACK_DEFAULT	= \&callback_default;
our $ADB = "adb";
our $ADB_HOST	= "127.0.0.1";
our $ADB_SOCKET	= undef;
our $ADB_PORT	= 5037;
our $TRANSACTION_TIMEOUT = 10;
our $CONNECT_TIMEOUT = 10;

# Notice that the client argument isn't yet blessed at this point
sub new {
    @_ % 2 == 0 || croak "Odd number of arguments";

    my ($class, $client, %arguments) = @_;

    my $blocking = delete $arguments{blocking} // 1;
    my $adb  = delete $arguments{adb} // $ADB;
    my $adb_socket = delete $arguments{adb_socket} // $ADB_SOCKET;
    my $host = delete $arguments{host} // $ADB_HOST;
    my $port = delete $arguments{port} // $ADB_PORT;
    my $reresolve = delete $arguments{reresolve};
    my $connect_timeout = delete $arguments{connect_timeout} //
        $CONNECT_TIMEOUT;
    my $transaction_timeout = delete $arguments{transaction_timeout} //
        $TRANSACTION_TIMEOUT;
    my $block_size = delete $arguments{block_size} || $BLOCK_SIZE;

    croak "Unknown argument " . join(", ", keys %arguments) if %arguments;

    my $client_ref = bless {
        host		=> $host,
        port		=> $port,
        reresolve	=> $reresolve,
        addr_info	=> undef,
        connect_timeout		=> $connect_timeout,
        transaction_timeout	=> $transaction_timeout,
        adb		=> $adb,
        adb_socket	=> $adb_socket,
        block_size	=> $block_size,
        socket		=> undef,
        blocking	=> $blocking,
        connected	=> undef,
        in		=> "",
        out		=> "",
        commands	=> [],
        children	=> {},
        result		=> [],
    }, $class;
    weaken($client_ref->{client} = $client);
    $client_ref->resolve();
    return $client_ref;
}

sub delete {
    my ($client_ref) = @_;

    my $children = $client_ref->{children};
    $client_ref->{children} = {};
    $_->delete(1) for values %$children;
    @{$client_ref->{result}} = ()
}

sub child_add {
    my ($client_ref, $child) = @_;
    $client_ref->{children}{refaddr($child)} = $child;
}

sub child_delete {
    my ($client_ref, $child) = @_;
    delete $client_ref->{children}{refaddr($child)};
}

sub DESTROY {
    info("DESTROY @_") if $DEBUG;
    shift->delete;
}

sub callback_default {
    die $_[1] if $_[1];
}

sub callback_blocking {
    my ($client_ref, $loop_level) = @_;

    $client_ref->{result}[$loop_level] = undef;
    return sub {
        my $client_ref = $ {shift()};
        $client_ref->{result}[$loop_level] = \@_;
        unloop($loop_level);
    };
}

sub wait : method {
    my ($client_ref, $loop_level) = @_;

    mainloop();
    my $result = delete $client_ref->{result}[$loop_level] //
        die "Assertion: Exit mainloop without setting result";
    croak $result->[0] if $result->[0];
    wantarray || return $result->[1];
    # remove the flag indicating success
    shift @$result;
    return @$result;
}

sub resolve {
    my ($client_ref) = @_;
    $client_ref->{addr_info} = addr_info($client_ref->{host}, $client_ref->{port});
}

sub server_start {
    my ($client_ref, $callback, $arguments) = @_;

    ADB::Client::ServerStart->new($client_ref, $callback, $arguments);
}

1;
__END__
# Below is stub documentation for your module. You'd better edit it!

=head1 NAME

ADB::Client::Ref - Perl extension for blah blah blah

=head1 SYNOPSIS

  use ADB::Client::Ref;
  blah blah blah

=head1 DESCRIPTION

Stub documentation for ADB::Client::Ref, created by h2xs. It looks like the
author of the extension was negligent enough to leave the stub
unedited.

Blah blah blah.

=head2 EXPORT

None by default.



=head1 SEE ALSO

Mention other useful documentation such as the documentation of
related modules or operating system documentation (such as man pages
in UNIX), or any relevant external documentation such as RFCs or
standards.

If you have a mailing list set up for your module, mention it here.

If you have a web site set up for your module, mention it here.

=head1 AUTHOR

Ton Hospel, E<lt>ton@E<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2020 by Ton Hospel

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.30.3 or,
at your option, any later version of Perl 5 you may have available.

=cut
