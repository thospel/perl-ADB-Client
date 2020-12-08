package ADB::Client::SpawnRef;
use strict;
use warnings;

our $VERSION = '1.000';

use Scalar::Util qw(weaken);

use ADB::Client::Utils qw(info $DEBUG $QUIET);

my $objects = 0;

sub new {
    my ($class, $starter, $client) = @_;

    weaken($starter->{clients}{++$starter->{index}} = $client);
    ++$objects;
    my $starter_ref = bless {
        starter	=> $starter,
        index	=> $starter->{index},
    }, $class;
    # weaken($starter_ref->{handlers}{starter});
    $client->{handlers}{starter} = $starter_ref;
    return $starter_ref;
}

sub DESTROY {
    --$objects;
    info("DESTROY @_") if $DEBUG;

    my ($starter_ref) = @_;

    if (my $starter = $starter_ref->{starter}) {
        delete $starter->{clients}{$starter_ref->{index}};
    }
    # Starter continues running even if no more clients in
    # case another join gets done before the end
}

sub objects {
    return $objects;
}

END {
    # $QUIET first for easier code coverage
    info("Still have %d %s objects at program end", $objects, __PACKAGE__) if !$QUIET && $objects;
}

1;
