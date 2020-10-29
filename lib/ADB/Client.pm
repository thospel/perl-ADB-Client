package ADB::Client;
use strict;
use warnings;

our $VERSION = '1.000';

use Carp;

use ADB::Client::Ref qw(mainloop unloop loop_levels
                        $CALLBACK_DEFAULT
                        $ADB_HOST $ADB_PORT $ADB);
use ADB::Client::Utils qw(info string_from_value $DEBUG $VERBOSE);

use Exporter::Tidy
    other	=>[qw(mainloop unloop loop_levels string_from_value
                      $CALLBACK_DEFAULT
                      $ADB_HOST $ADB_PORT $ADB $DEBUG $VERBOSE)];

# Sanity check
die "Bad file '", __FILE__, "'" if __FILE__ =~ /["\n\0]/;

my $objects = 0;

sub objects {
    return $objects;
}

sub ref_class {
    return shift . "::Ref";
}

sub new {
    my $class = shift;
    my $client = \my $client_ref;
    $client_ref = $class->ref_class->new($client, @_);
    ++$objects;
    return bless $client, $class;
}

sub DESTROY {
    --$objects;
    info("DESTROY @_") if $DEBUG;
    my $client_ref = ${shift()};
    $client_ref->delete if $client_ref;
}

sub client_ref {
    return ${shift()};
}

# Simply forward command
for my $name (qw(connected close activate host port)) {
    my %replace = (
        NAME	=> $name,
        FILE	=> __FILE__,
        LINE	=> __LINE__+4,
    );
    my $code = '
#line LINE "FILE"
sub NAME : method {
    my $client_ref = $ {shift()};
    return $client_ref->NAME(@_);
}
1;
';
    $code =~ s/\b(NAME|LINE|FILE)\b/$replace{$1}/g;
    # print STDERR $code;
    eval $code || die $@;
}

for my $name (qw(server_start)) {
    my %replace = (
        NAME	=> $name,
        FILE	=> __FILE__,
        LINE	=> __LINE__+4,
    );
    my $code = '
#line LINE "FILE"
sub NAME {
    @_ % 2 == 1 || croak "Odd number of arguments";
    my $client_ref = $ {shift()};
    my %arguments = @_;
    if (delete $arguments{blocking} // $client_ref->{blocking}) {
        # blocking
        my $loop_levels = loop_levels();
        $client_ref->NAME(\%arguments, $client_ref->callback_blocking($loop_levels));
        return $client_ref->wait($loop_levels);
    }
    $client_ref->NAME(\%arguments, delete $arguments{callback} || $CALLBACK_DEFAULT);
    return;
}
1;
';
    $code =~ s/\b(NAME|LINE|FILE)\b/$replace{$1}/g;
    # print STDERR $code;
    eval $code || die $@;
}

sub add_commands {
    my ($class) = @_;

    $class->ref_class->commands_add($class);
}

sub add_command {
    my $class = shift;
    $class->ref_class->command_add($class, @_);
}

sub _add_command {
    my ($class, $index) = @_;

    my ($command_name, $nr_vars, $special) =
        $class->ref_class->command_get($index);
    my %replace = (
        NAME	=> $command_name,
        PROXY	=> $special ? $command_name : "command_simple",
        INDEX	=> $index,
        NR_VARS	=> $nr_vars,
        FILE	=> __FILE__,
        LINE	=> __LINE__+4,
    );
    my $code = '
#line LINE "FILE"
sub NAME : method {
    @_ > NR_VARS || croak "Too few arguments";
    @_ % 2 != NR_VARS % 2 || croak "Odd number of arguments";
    my $client_ref = $ {shift()};
    my @vars = NR_VARS ? splice(@_, 0, NR_VARS) : ();
    my %arguments = @_;
    if (delete $arguments{blocking} // $client_ref->{blocking}) {
        # blocking
        my $loop_levels = loop_levels();
        $client_ref->PROXY(\%arguments, $client_ref->callback_blocking($loop_levels), INDEX, \@vars);
        return $client_ref->wait($loop_levels);
    }
    $client_ref->PROXY(\%arguments, delete $arguments{callback} || $CALLBACK_DEFAULT, INDEX, \@vars);
    return;
}
1;
';
    $code =~ s/\b(NAME|LINE|NR_VARS|PROXY|FILE|INDEX)\b/$replace{$1}/g;
    # print STDERR $code;
    eval $code || die $@;
}

__PACKAGE__->add_commands();

# Convenience functions
sub transport_usb {
    my $client = shift;
    return $client->transport("usb", @_);
}

sub transport_tcp {
    my $client = shift;
    return $client->transport("tcp", @_);
}

sub transport_any {
    my $client = shift;
    return $client->transport("any", @_);
}

sub transport_local {
    my $client = shift;
    return $client->transport("local", @_);
}

sub tport_usb {
    my $client = shift;
    return $client->tport("usb", @_);
}

sub tport_tcp {
    my $client = shift;
    return $client->tport("tcp", @_);
}

sub tport_any {
    my $client = shift;
    return $client->tport("any", @_);
}

sub tport_local {
    my $client = shift;
    return $client->tport("local", @_);
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
