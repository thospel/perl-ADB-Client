#!/usr/bin/perl -w
# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl 80_syntax.t'
#########################
## no critic (UselessNoCritic MagicNumbers)
use strict;
use warnings;

our $VERSION = "1.000";

use FindBin qw($Bin);
use lib $Bin;
use Test::More tests => 10;
use TestDrive qw(collect_stderr collected_stderr uncollect_stderr);

sub check {
    collect_stderr();
    my $rc = system($^X, "-c", @_, "--blib");
    uncollect_stderr();
    my $errors = collected_stderr();
    $errors =~ s/.* syntax OK\n//;
    if ($errors ne "") {
        diag($errors);
        return 1;
    }
    return $rc;
}

$Bin =~ s{/t/?\z}{} || die "No /t at end of $Bin";

for my $script (qw(adb_fake adb_tester adb_tracker)) {
    ok(!check("-I", "$Bin/blib/lib", "-I", "$Bin/blib/arch",
              "$Bin/bin/$script"),
       "Can compile $Bin/bin/$script");
}
