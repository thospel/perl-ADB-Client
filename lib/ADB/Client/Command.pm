package ADB::Client::Command;
use strict;
use warnings;

our $VERSION = '1.000';

use ADB::Client::Utils qw(info $DEBUG $QUIET);

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
    # $QUIET first for easier code coverage
    info("Still have %d %s objects at program end", $objects, __PACKAGE__) if !$QUIET && $objects;
}

1;
