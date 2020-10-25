package ADB::Client;
use strict;
use warnings;

our $VERSION = '1.000';

use Carp;

use ADB::Client::Ref qw(addr_info mainloop unloop loop_level
                        info caller_info dumper realtime clocktime
                        $BASE_REALTIME $BASE_CLOCKTIME $CLOCK_TYPE
                        $CALLBACK_DEFAULT
                        $ADB_HOST $ADB_PORT $ADB $DEBUG $VERBOSE);
use Exporter::Tidy
    other	=>[qw(addr_info mainloop unloop loop_level dumper
                      info caller_info realtime clocktime
                      $BASE_REALTIME $BASE_CLOCKTIME $CLOCK_TYPE
                      $CALLBACK_DEFAULT
                      $ADB_HOST $ADB_PORT $ADB $DEBUG $VERBOSE)];

# Sanity check
die "Bad file '", __FILE__, "'" if __FILE__ =~ /["\n\0]/;

sub ref_class {
    return shift . "::Ref";
}

sub new {
    my $class = shift;
    my $client = \my $client_ref;
    $client_ref = $class->ref_class->new($client, @_);
    return bless $client, $class;
}

sub DESTROY {
    info("DESTROY @_") if $DEBUG;
    ${shift()}->delete;
}

for my $name (qw(server_start)) {
    my %replace = (
        LINE	=> __LINE__+6,
        FILE	=> __FILE__,
        NAME	=> $name,
    );
    my $code = '
#line LINE "FILE"
sub NAME {
    @_ % 2 == 1 || croak "Odd number of arguments";
    my $client_ref = $ {shift()};
    my %arguments = @_;
    if (delete $arguments{blocking} // $client_ref->{blocking}) {
        # blocking
        my $loop_level = loop_level();
        $client_ref->NAME($client_ref->callback_blocking($loop_level), \%arguments);
        return $client_ref->wait($loop_level);
    }
    $client_ref->NAME(delete $arguments{callback} || $CALLBACK_DEFAULT, \%arguments);
    return;
}
1;
';
    $code =~ s/\b(NAME|LINE|FILE)\b/$replace{$1}/g;
    eval $code || die $@;
}

1;
__END__
# Below is stub documentation for your module. You'd better edit it!

=head1 NAME

ADB::Client - Perl extension for blah blah blah

=head1 SYNOPSIS

  use ADB::Client;
  blah blah blah

=head1 DESCRIPTION

Stub documentation for ADB::Client, created by h2xs. It looks like the
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
