package ADB::Client::Package;
# $HeadURL: xxx/lib/ADB/Client/Package.pm $
# $Id: xxx $

# START HISTORY
# autogenerated by release_pm
use strict;
use warnings;
use vars qw($VERSION $SUB_VERSION $release_time %history);
$VERSION = "1.000";
$SUB_VERSION = "026";
$release_time = 1605292422;	## no critic (UselessNoCritic MagicNumbers)
%history = (
  'Changes' => {
    '1.000' => '1.000'
  },
  'MANIFEST' => {
    '1.000' => '1.000'
  },
  'MANIFEST.SKIP' => {
    '1.000' => '1.000'
  },
  'Makefile.PL' => {
    '1.000' => '1.000'
  },
  'README' => {
    '1.000' => '1.000'
  },
  'bin/adb_fake' => {
    '1.000' => '1.000'
  },
  'bin/adb_tester' => {
    '1.000' => '1.000'
  },
  'lib/ADB/Client.pm' => {
    '1.000' => '1.000'
  },
  'lib/ADB/Client/Command.pm' => {
    '1.000' => '1.000'
  },
  'lib/ADB/Client/Events.pm' => {
    '1.000' => '1.000'
  },
  'lib/ADB/Client/Package.pm' => {
    '1.000' => '1.000'
  },
  'lib/ADB/Client/Ref.pm' => {
    '1.000' => '1.000'
  },
  'lib/ADB/Client/ServerStart.pm' => {
    '1.000' => '1.000'
  },
  'lib/ADB/Client/Spawn.pm' => {
    '1.000' => '1.000'
  },
  'lib/ADB/Client/SpawnRef.pm' => {
    '1.000' => '1.000'
  },
  'lib/ADB/Client/Timer.pm' => {
    '1.000' => '1.000'
  },
  'lib/ADB/Client/Tracker.pm' => {
    '1.000' => '1.000'
  },
  'lib/ADB/Client/Utils.pm' => {
    '1.000' => '1.000'
  },
  't/00_load.t' => {
    '1.000' => '1.000'
  },
  't/01_adb_check_response.t' => {
    '1.000' => '1.000'
  },
  't/02_ip_port_from_addr.t' => {
    '1.000' => '1.000'
  },
  't/03_is_listening.t' => {
    '1.000' => '1.000'
  },
  't/04_adb_client.t' => {
    '1.000' => '1.000'
  },
  't/05_adb_fake.t' => {
    '1.000' => '1.000'
  },
  't/06_kill.t' => {
    '1.000' => '1.000'
  },
  't/07_marker.t' => {
    '1.000' => '1.000'
  },
  't/08_commands.t' => {
    '1.000' => '1.000'
  },
  't/09_devices.t' => {
    '1.000' => '1.000'
  },
  't/10_connect.t' => {
    '1.000' => '1.000'
  },
  't/11_spawn.t' => {
    '1.000' => '1.000'
  },
  't/12_spawn_socket.t' => {
    '1.000' => '1.000'
  },
  't/20_ipv6.t' => {
    '1.000' => '1.000'
  },
  't/21_non_loopback.t' => {
    '1.000' => '1.000'
  },
  't/90_real_adb.t' => {
    '1.000' => '1.000'
  },
  't/98_fatal.t' => {
    '1.000' => '1.000'
  },
  't/99_internals.t' => {
    '1.000' => '1.000'
  },
  't/TestDrive.pm' => {
    '1.000' => '1.000'
  },
  'version_check' => {
    '1.000' => '1.000'
  }
);

use Carp;

my $epoch_base;

sub SUB_VERSION {
    return $SUB_VERSION;
}

sub FULL_VERSION {
    return "$VERSION.$SUB_VERSION";
}

sub release_time {
    if (!defined $epoch_base) {
        require Time::Local;
        $epoch_base = Time::Local::timegm(0,0,0,1,0,1970);	## no critic (UselessNoCritic MagicNumbers)
    }
    return $release_time + $epoch_base;
}

sub released {
    my ($package, $version) = @_;
    my $p = $package;
    $p =~ s{::}{/}g;
    my $history = $history{"lib/$p.pm"} ||
        croak "Could not find a history for package '$package'";
    my $lowest = 9**9**9;
    for my $v (keys %$history) {
        $lowest = $v if $v >= $version && $v < $lowest;
    }
    croak "No known version '$version' of package '$package'" if
        $lowest == 9**9**9;
    return $history->{$lowest};
}
1;
__END__

=for stopwords ADB::Client globals

=head1 NAME

ADB::Client::Package - Version and history of ADB::Client

=head1 SYNOPSIS

  use ADB::Client::Package;

  $epoch_time = ADB::Client::Package->release_time();

  $package_version = ADB::Client::Package->VERSION;

  $min_package_version = ADB::Client::Package::released($module, $module_version);

=head1 DESCRIPTION

In the context of this documentation a C<package> is a set of files that
together make up a perl extension, not to be confused with the more normal
perl concept of a namespace for globals.

A package release with a certain package version number contains a number of
modules and other files with their own version numbers. This module contains a
history of which files with which versions where in which package release and
also knows when the current package was released.

This module contains a few simple methods to query this.

The version number of this Package.pm module is always equal to the version
number of the package release. This means you can use this to query this
release number and also makes it a convenient target for Makefile.PL
dependencies.

=head1 METHODS

=over

=item X<release_time>$epoch_time = ADB::Client::Package->release_time()

Returns the the number of non-leap seconds since whatever time the system that
calls this function considers to be the epoch the last time
L<release_pm|release_pm(1)> was run (even if that was on another system with a
different epoch). Since the idea is to run L<release_pm|release_pm(1)> just
before releasing the package this is therefore the value of
L<time()|perlfunc/time> on the system calling this function at the moment the
package was released. This number is suitable for feeding to
L<gmtime()|perlfunc/gmtime> and L<local_time()|perlfunc/local_time>

=item X<VERSION>$package_version = ADB::Client::Package->VERSION

This is the normal L<UNIVERSAL::VERSION|UNIVERSAL/VERSION> method you can use
on all modules, but this particular module is guaranteed to have a version
number that is the same as that of the package

=item X<released>$min_package_version = ADB::Client::Package::released($module, $module_version)

Given a module name (e.g. Foo::Bar) and a module version number (e.g. 1.023)
returns the lowest package version in which that module (as the file
F<Foo/Bar.pm>) had at least that module version

=back

=head1 EXPORTS

None.

=head1 SEE ALSO

L<release_pm|release_pm(1)> which can be used to keep Package.pm files up to
date.

=cut

# END HISTORY
