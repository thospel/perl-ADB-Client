#!/usr/bin/perl -l
package Foo;
use Carp;

sub bar {
    croak "Boom!";
}

package main;
use Data::Dumper;
sub DESTROY { print "DESTROY" }

{
    my $obj  = bless [];
    eval { Foo::bar(undef, $obj)};
    print "ERR=$@";
    print Dumper(\@DB::args);
}
print "END";
