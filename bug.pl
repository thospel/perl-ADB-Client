#!/usr/bin/perl -l
package Foo;
use Carp;

sub bar {
    # splice(@_, 0, 1);
    # die "Boom!";
    croak "Boom!";
}

package main;
use Data::Dumper;
sub DESTROY { print "DESTROY" }

{
    my $obj  = bless [];
    eval { Foo::bar(undef, $obj)};
    print "ERR=$@";
    # print Dumper(\@DB::args);
    # @DB::args = ();
    # eval { Carp::croak "zz" };
}
print "END";
