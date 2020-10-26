#!/usr/bin/perl -w
# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl 00_load.t'
#########################
## no critic (UselessNoCritic MagicNumbers)

use strict;
use warnings;

our $VERSION = "1.000";

use Test::More tests => 9;
for my $module (qw(ADB::Client::Package)) {
    use_ok($module) || BAIL_OUT("Cannot even use $module");
}
my $released = ADB::Client::Package->release_time;
like($released, qr{^[0-9]+\z}, "release_time is a number");
is(ADB::Client::Package->release_time, $released,
   "Still the same release time");
like(ADB::Client::Package->SUB_VERSION, qr{^[0-9]{3}\z}, "SUB_VERSION is a 3 digit number");
like(ADB::Client::Package->FULL_VERSION, qr{^[1-9][0-9]*(?:\.[0-9]{3}){2}\z}, "FULL_VERSION is a 1.3.3 digit number");
is(ADB::Client::Package::released("ADB::Client::Package", "1.000"),
   "1.000", "Module released");
eval { ADB::Client::Package::released("Mumble", "1.000") };
like($@, qr{^Could not find a history for package 'Mumble' at },
     "Expected module not found");
eval { ADB::Client::Package::released("ADB::Client/Package", "9999") };
like($@,
     qr{^No known version '9999' of package 'ADB::Client/Package' at },
     "Expected version not found");
# The fact that this makes cond coverage 100% must be a Devel::Cover bug
eval { ADB::Client::Package::released("OogieBoogie", "1.000") };
like($@,
     qr{^Could not find a history for package 'OogieBoogie' at },
     "No history for unknown modules");
