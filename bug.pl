#!/usr/bin/perl -l
package Foo;
use Carp;

sub bar {
    splice(@_, 0, 1);
    # die "Boom!";
    croak "Boom!";
}

package main;
sub DESTROY { print "DESTROY" }

{
    my $obj  = bless [];
    eval { Foo::bar(1, $obj)};
    print "ERR=$@";
    # @DB::args = ();
    # eval { Carp::croak "zz" };
}
print "END";
