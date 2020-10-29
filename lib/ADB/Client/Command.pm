package ADB::Client::Command;
use strict;
use warnings;

our $VERSION = '1.000';

use ADB::Client::Utils qw(info $DEBUG);

my $objects = 0;

sub new {
    ++$objects;
    return bless[], shift;
}

sub DESTROY {
    --$objects;
    info("DESTROY @_") if $DEBUG;
}

sub objects {
    return $objects;
}

END {
    info("Still have $objects ADB::Client::Ref::Command objects at program end") if $objects;
}

1;
