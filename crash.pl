#!/usr/bin/perl -w
use Carp;
use Scalar::Util qw(weaken);

sub _reader {
    shift->{writer} = undef;
    confess "Boem";
}

my $client = {};
weaken(my $writes = $client->{writer} = bless [\&_reader, $client]);
$$_->[0]->($$_->[1]) for \$writes;
