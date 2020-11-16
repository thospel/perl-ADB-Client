package ADB::Client;
use strict;
use warnings;

our $VERSION = '1.000';

use Carp;

use ADB::Client::Ref qw($CALLBACK_DEFAULT $ADB $ADB_HOST $ADB_PORT);
use ADB::Client::Utils qw(info string_from_value ip_port_from_addr is_listening
                          $DEBUG $VERBOSE $QUIET);
use ADB::Client::Events qw(mainloop event_init unloop loop_levels
                           timer immediate);

use Exporter::Tidy
    events	=>[qw(mainloop event_init unloop loop_levels timer immediate)],
    other	=>[qw(string_from_value
                      $CALLBACK_DEFAULT $ADB_HOST $ADB_PORT $ADB
                      $DEBUG $VERBOSE $QUIET)];

# Sanity check
die "Bad file '", __FILE__, "'" if __FILE__ =~ /["\n\0]/;

my $objects = 0;

sub objects {
    return $objects;
}

END {
    # $QUIET first for easier code coverage
    info("Still have %d %s objects at program end", $objects, __PACKAGE__) if !$QUIET && $objects;
}

sub ref_class {
    return shift . "::Ref";
}

sub new {
    my $class = shift;

    my $client = \my $client_ref;
    my $ref_class = ref $class eq "" ? $class->ref_class : $class->client_ref;
    $client_ref = $ref_class->new($client, @_);
    ++$objects;
    return bless $client, ref $class eq "" ? $class : ref($class);;
}

sub DESTROY {
    --$objects;
    info("DESTROY @_") if $DEBUG;
    my $client_ref = ${shift()};
    $client_ref->delete;
}

sub client_ref {
    return ${shift()};
}

my @keep_spawn_socket = qw(adb adb_socket spawn_timeout);
sub spawn_socket {
    @_ % 2 == 0 || croak "Odd number of arguments";
    my ($class, $s, %arguments) = @_;
    my %keep_args = (blocking => delete $arguments{blocking} // 1 );
    @keep_args{@keep_spawn_socket} = delete @arguments{@keep_spawn_socket};
    my $callback = $keep_args{blocking} ? undef : delete $arguments{callback};
    croak "Unknown argument " . join(", ", keys %arguments) if %arguments;

    fileno($s) // croak "Socket is not an IO handle";
    my $socket;
    {
        # Make a copy without CLOEXEC
        local $^F = 2**31-1;
        open($socket, "+>&", $s) || croak "Could not dup socket: $^E";
    }
    my $addr = getsockname($socket) || croak "Cannot getsockname: $^E";
    @keep_args{qw(host port)} = ip_port_from_addr($addr);
    is_listening($socket) || croak "Socket is not listening";

    my $client = $class->new(%keep_args);
    my $_addr_info = $client->_addr_info;
    @$_addr_info == 1 || die "Assertion: Multiple addr_info";
    $_addr_info->[0]{bind_addr} = $socket;

    $client->spawn($callback ? (callback => $callback) : ());
    return $client;
}

# Forward activate with a toplevel argument
sub activate {
    my ($client, $top_level) = @_;

    $client->client_ref->activate();
}

# Simply forward command
for my $name (
    qw(connected host port adb_socket fatal addr_info _addr_info
       connection_data command_retired post_activate blocking)) {
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

    # Check before utf8::encode (no need to utf8::upgrade or utf8::downgrade)
    # This also checks higher up in the class hierarchy
    croak "Attempt to redefine already existing command '$command_name'" if
        $class->can($command_name);

    $command_name =~ /^[^\W\d]\w*\z/u ||
        croak "Command_name '$command_name' is invalid as a perl identifier";
    utf8::encode($command_name);

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
use utf8;

sub NAME : method {
    @_ > NR_VARS || croak "Too few arguments";
    @_ % 2 != NR_VARS % 2 || croak "Odd number of arguments";
    my $client = shift;
    $client = $client->new if ref $client eq "";
    my $client_ref = $$client;
    my @vars = NR_VARS ? splice(@_, 0, NR_VARS) : ();
    my %arguments = @_;
    if (delete $arguments{blocking} // $client_ref->{blocking}) {
        # blocking
        $client_ref->PROXY(\%arguments, $client_ref->callback_blocking, INDEX, \@vars);
        return $client_ref->wait;
    }
    $client_ref->PROXY(\%arguments, delete $arguments{callback} || $CALLBACK_DEFAULT, INDEX, \@vars);
    return;
}
1;
';
    $code =~ s/\b(NAME|LINE|NR_VARS|PROXY|FILE|INDEX)\b/$replace{$1}/g;
    # print STDERR $code;
    if (!eval $code) {
        $@ =~ s/\.?\n\z//;
        croak $@;
    }
}

__PACKAGE__->add_commands();

# Convenience functions
sub transport_usb {
    return shift->_transport("usb", @_);
}

sub transport_tcp {
    return shift->_transport("tcp", @_);
}

sub transport_any {
    return shift->_transport("any", @_);
}

sub transport_local {
    return shift->_transport("local", @_);
}

sub transport_serial {
    my $client = shift;
    my $serial = shift || croak "Missing serial";
    return $client->_transport_serial($serial, "any", @_);
}

sub transport_id {
    my $client = shift;
    my $id = shift // croak "Missing id";
    $id =~ /^\s*([0-9]+)\s*$/ || croak "id must be a natural number";
    $id = int($1);
    return $client->_transport_id($id, "any", @_);
}

sub tport_usb {
    return shift->_tport("usb", @_);
}

sub tport_tcp {
    return shift->_tport("tcp", @_);
}

sub tport_any {
    return shift->_tport("any", @_);
}

sub tport_local {
    return shift->_tport("local", @_);
}

# Both of these work:
# host-serial:0715f712da553032:tport:this_does_not_matter
# host:tport:serial:0715f712da553032
sub tport_serial {
    my $client = shift;
    my $serial = shift || croak "Missing serial";
    return $client->_tport_serial($serial, "any", @_);
}

# Only this works: host-transport-id:346:tport:this_does_not_matter
# This fails: host:tport:transport-id:346
sub tport_id {
    my $client = shift;
    my $id = shift // croak "Missing id";
    $id =~ /^\s*([0-9]+)\s*$/ || croak "id must be a natural number";
    $id = int($1);
    return $client->_tport_id($id, "any", @_);
}

sub wait : method {
    return shift->_wait("any", @_);
}

sub wait_serial {
    my $client = shift;
    my $serial = shift || croak "Missing serial";
    return $client->_wait_serial($serial, "any", @_);
}

sub wait_id {
    my $client = shift;
    my $id = shift // croak "Missing id";
    $id =~ /^\s*([0-9]+)\s*$/ || croak "id must be a natural number";
    $id = int($1);
    return $client->_wait_id($id, "any", @_);
}

sub wait_usb {
    return shift->_wait("usb", @_);
}

sub wait_local {
    return shift->_wait("local", @_);
}

# The wait functions are mostly used to wait for a device

sub wait_device {
    return shift->_wait("any", "device", @_);
}

sub wait_serial_device {
    my $client = shift;
    my $serial = shift || croak "Missing serial";
    return $client->_wait_serial($serial, "any", "device", @_);
}

sub wait_id_device {
    my $client = shift;
    my $id = shift // croak "Missing id";
    $id =~ /^\s*([0-9]+)\s*$/ || croak "Id must be a natural number";
    $id = int($1);
    return $client->_wait_id($id, "any", "device", @_);
}

sub wait_usb_device {
    return shift->_wait("usb", "device", @_);
}

sub wait_local_device {
    return shift->_wait("local", "device", @_);
}

sub restart {
    my $client = shift;
    return $client->reboot("", @_);
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
