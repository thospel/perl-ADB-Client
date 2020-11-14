#!/usr/bin/perl -l
use strict;
use warnings;

sub DESTROY { print "DESTROY" };
{
    my $a = bless [];
    B::bar($a);
}
print "END";

package B;
use Carp;
sub foo { croak "zz" }
sub bar {
    eval { foo($_[0]) };
    \@DB::args;
};
