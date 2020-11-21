#!/usr/bin/perl -w
# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl 01_adb_check_response.t'
#########################
## no critic (UselessNoCritic MagicNumbers)

# This should provide complete coverage of adb_check_response
# (Check using "make cover")

use strict;
use warnings;

our $VERSION = "1.000";

use Test::More tests => 434;

BEGIN {
    use_ok("ADB::Client::Utils",
           qw(adb_check_response display_string DISPLAY_MAX $me
              SUCCEEDED FAILED BAD_ADB ASSERTION INFINITY OKAY FAIL)) ||
        BAIL_OUT("Cannot even import adb_check_response from ADB::Client::Utils");
}

my $name = (getpwnam($me))[6];
diag("Reminder: Set environment variables ADB_CLIENT_TEST_DEVELOPER and ADB_CLIENT_TEST_REAL for some tests against the real ADB server") if $name eq "Ton Hospel";

my $long_string = "ABC\nde\"zz\0fg\th\x{c1}zwarf\tha" . "-" x DISPLAY_MAX;
my @args;

# Also implicitely checks display_string
is(display_string(undef), "undef");
@args = [];
is(display_string(@args), "@args");

# Check some constants
is(OKAY, "OKAY");
is(FAIL, "FAIL");
# INFINITY is really big
is(1+INFINITY, INFINITY);
is(0.5*INFINITY, INFINITY);
# it's not NaN
is(INFINITY, INFINITY);

# Check negative add
@args = ({ in => "ZZ" }, -1, -1);
is_deeply([adb_check_response(@args)],
          [ASSERTION, 'Assertion: negative input']);
is($args[0]{in}, "ZZ");

# Check zero add
@args = ({ in => "" }, 0, -1);
is_deeply([adb_check_response(@args)], [BAD_ADB, 'Unexpected EOF']);
is($args[0]{in}, "");

@args = ({ in => "ZZ" }, 0, -1);
is_deeply([adb_check_response(@args)], [BAD_ADB, 'Bad ADB status "ZZ"']);
is($args[0]{in}, "ZZ");

@args = ({ in => "OKAY" }, 0, -1);
is_deeply([adb_check_response(@args)], [BAD_ADB, 'Truncated ADB response "OKAY"']);
is($args[0]{in}, "OKAY");

@args = ({ in => "FAIL" }, 0, -1);
is_deeply([adb_check_response(@args)], [BAD_ADB, 'Truncated ADB response "FAIL"']);
is($args[0]{in}, "FAIL");

# A previous call will already have recognized this as Bad instead of truncated
@args = ({ in => "OKAZ" }, 0, -1);
is_deeply([adb_check_response(@args)], [BAD_ADB, 'Truncated ADB response "OKAZ"']);
is($args[0]{in}, "OKAZ");

# Actually check that
@args = ({ in => "OKAZ" }, 1, -1);
is_deeply([adb_check_response(@args)], [BAD_ADB, 'Bad ADB status "OKAZ"']);
is($args[0]{in}, "OKAZ");

@args = ({ in => "O" }, 0, -1);
is_deeply([adb_check_response(@args)], [BAD_ADB, 'Truncated ADB status "O"']);
is($args[0]{in}, "O");

@args = ({ in => "FAI" }, 0, -1);
is_deeply([adb_check_response(@args)], [BAD_ADB, 'Truncated ADB status "FAI"']);
is($args[0]{in}, "FAI");

@args = ({ in => "OKAYZ" }, 0, 2);
is_deeply([adb_check_response(@args)], [BAD_ADB, 'Truncated ADB response "OKAYZ"']);
is($args[0]{in}, "OKAYZ");

# FAIL converts $nr=2 to $nr=-1
@args = ({ in => "FAILZ" }, 0, 2);
is_deeply([adb_check_response(@args)], [BAD_ADB, 'Bad ADB hex length "Z"']);
is($args[0]{in}, "FAILZ");

# FAIL converts $nr=2 to $nr=-1
@args = ({ in => "FAIL1" }, 0, 2);
is_deeply([adb_check_response(@args)], [BAD_ADB, 'Truncated hex length "1"']);
is($args[0]{in}, "FAIL1");

# Add bytes to OKAY
@args = ({ in => "OKAYZZ" }, 1, 3);
is_deeply([adb_check_response(@args)], []);
is($args[0]{in}, "OKAYZZ");

# Add bytes to OKAY and finish it
@args = ({ in => "OKAYZZZ" }, 1, 3);
is_deeply([adb_check_response(@args)], [SUCCEEDED, "ZZZ"]);
is($args[0]{in}, "");

# Add too many bytes to OKAY
@args = ({ in => "OKAYZZZZ" }, 2, 3);
is_deeply([adb_check_response(@args)], [SUCCEEDED, "ZZZ"]);
is($args[0]{in}, "Z");

# Add too many bytes to OKAY and expect EOF
@args = ({ in => "OKAYZZZZ" }, 2, 3, 1);
is_deeply([adb_check_response(@args)], [BAD_ADB, 'Spurious bytes in ADB response "OKAYZZZZ"']);
is($args[0]{in}, "OKAYZZZZ");

# Add too few bytes to OKAY and do EOF
@args = ({ in => "FAIL0003ZZ" }, 0, 3);
is_deeply([adb_check_response(@args)], [BAD_ADB, 'Truncated ADB response (expected 3, got 2 bytes)']);
is($args[0]{in}, "FAIL0003ZZ");

# Add too few bytes to FAIL
@args = ({ in => "FAIL0003ZZ" }, 2, 3);
is_deeply([adb_check_response(@args)], []);
is($args[0]{in}, "FAIL0003ZZ");

# Add too few bytes to FAIL and expect EOF
@args = ({ in => "FAIL0003ZZ" }, 2, 3, 1);
is_deeply([adb_check_response(@args)], []);
is($args[0]{in}, "FAIL0003ZZ");

# Add exact bytes to FAIL
@args = ({ in => "FAIL0003ZZZ" }, 2, 3);
is_deeply([adb_check_response(@args)], [FAILED, "ZZZ"]);
is($args[0]{in}, "");

# Add exact bytes to FAIL and expect EOF
@args = ({ in => "FAIL0003ZZZ" }, 2, 3);
is_deeply([adb_check_response(@args)], [FAILED, "ZZZ"]);
is($args[0]{in}, "");

# Add too many bytes to FAIL
@args = ({ in => "FAIL0003ZZZZ" }, 2, 3);
is_deeply([adb_check_response(@args)], [BAD_ADB, 'Spurious bytes in ADB response "FAIL0003ZZZZ"']);
is($args[0]{in}, "FAIL0003ZZZZ");

# Add too many bytes to FAIL and expect EOF
@args = ({ in => "FAIL0003ZZZZ" }, 2, 3);
is_deeply([adb_check_response(@args)], [BAD_ADB, 'Spurious bytes in ADB response "FAIL0003ZZZZ"']);
is($args[0]{in}, "FAIL0003ZZZZ");

# Ask for infinite number of bytes, incomplete status
@args = ({ in => "O" }, 1, INFINITY);
is_deeply([adb_check_response(@args)], []);
is($args[0]{in}, "O");

# Ask for infinite number of bytes, complete status
@args = ({ in => "OKAY" }, 1, INFINITY);
is_deeply([adb_check_response(@args)], []);
is($args[0]{in}, "OKAY");

# Ask for infinite number of bytes, complete status and EOF
@args = ({ in => "OKAY" }, 0, INFINITY);
is_deeply([adb_check_response(@args)], [SUCCEEDED, '']);
is($args[0]{in}, "");

# Ask for infinite number of bytes, complete status and some bytes
@args = ({ in => "OKAYZZZZ" }, 1, INFINITY);
is_deeply([adb_check_response(@args)], []);
is($args[0]{in}, "OKAYZZZZ");

# Ask for infinite number of bytes, complete status and some bytes and EOF
@args = ({ in => "OKAYZZZZ" }, 0, INFINITY);
is_deeply([adb_check_response(@args)], [SUCCEEDED, 'ZZZZ']);
is($args[0]{in}, "");

# Ask for infinite number of bytes, incomplete failed status
@args = ({ in => "F" }, 1, INFINITY);
is_deeply([adb_check_response(@args)], []);
is($args[0]{in}, "F");

# Ask for infinite number of bytes, complete failed status
@args = ({ in => "FAIL" }, 1, INFINITY);
is_deeply([adb_check_response(@args)], []);
is($args[0]{in}, "FAIL");

# Ask for infinite number of bytes, complete failed status and EOF
@args = ({ in => "FAIL" }, 0, INFINITY);
is_deeply([adb_check_response(@args)], [BAD_ADB, 'Truncated ADB response "FAIL"']);
is($args[0]{in}, "FAIL");

# Ask for infinite number of bytes, complete failed status and some bytes
@args = ({ in => "FAILZZZZ" }, 1, INFINITY);
is_deeply([adb_check_response(@args)], [BAD_ADB, 'Bad ADB hex length "ZZZZ"']);
is($args[0]{in}, "FAILZZZZ");

# Ask for infinite number of bytes, complete failed status and some bytes and EOF
@args = ({ in => "FAILZZZZ" }, 0, INFINITY);
is_deeply([adb_check_response(@args)], [ASSERTION, 'Assertion: adb hex length is suddenly "ZZZZ"']);
is($args[0]{in}, "FAILZZZZ");

# Check initial status response
@args = ({ in => "O" }, 1, -1);
is_deeply([adb_check_response(@args)], []);
is($args[0]{in}, "O");

@args = ({ in => "F" }, 1, -1);
is_deeply([adb_check_response(@args)], []);
is($args[0]{in}, "F");

@args = ({ in => "K" }, 1, -1);
is_deeply([adb_check_response(@args)], [BAD_ADB, 'Bad ADB status "K"']);
is($args[0]{in}, "K");

@args = ({ in => "OKA" }, 3, -1);
is_deeply([adb_check_response(@args)], []);
is($args[0]{in}, "OKA");

@args = ({ in => "FAI" }, 3, -1);
is_deeply([adb_check_response(@args)], []);
is($args[0]{in}, "FAI");

@args = ({ in => "OKK" }, 3, -1);
is_deeply([adb_check_response(@args)], [BAD_ADB, 'Bad ADB status "OKK"']);
is($args[0]{in}, "OKK");

@args = ({ in => "OKAY" }, 4, -1);
is_deeply([adb_check_response(@args)], []);
is($args[0]{in}, "OKAY");

@args = ({ in => "FAIL" }, 4, -1);
is_deeply([adb_check_response(@args)], []);
is($args[0]{in}, "FAIL");

@args = ({ in => "OKAQ" }, 4, -1);
is_deeply([adb_check_response(@args)], [BAD_ADB, 'Bad ADB status "OKAQ"']);
is($args[0]{in}, "OKAQ");

@args = ({ in => "OKAY0" }, 5, -1);
is_deeply([adb_check_response(@args)], []);
is($args[0]{in}, "OKAY0");

@args = ({ in => "FAIL0" }, 5, -1);
is_deeply([adb_check_response(@args)], []);
is($args[0]{in}, "FAIL0");

@args = ({ in => "OKAQ0" }, 5, -1);
is_deeply([adb_check_response(@args)], [BAD_ADB, 'Bad ADB status "OKAQ"']);
is($args[0]{in}, "OKAQ0");

# Check progressing status response
@args = ({ in => "OK" }, 1, -1);
is_deeply([adb_check_response(@args)], []);
is($args[0]{in}, "OK");

@args = ({ in => "FA" }, 1, -1);
is_deeply([adb_check_response(@args)], []);
is($args[0]{in}, "FA");

@args = ({ in => "FK" }, 1, -1);
is_deeply([adb_check_response(@args)], [BAD_ADB, 'Bad ADB status "FK"']);
is($args[0]{in}, "FK");

@args = ({ in => "OKAY" }, 1, -1);
is_deeply([adb_check_response(@args)], []);
is($args[0]{in}, "OKAY");

@args = ({ in => "FAIL" }, 1, -1);
is_deeply([adb_check_response(@args)], []);
is($args[0]{in}, "FAIL");

@args = ({ in => "FAIK" }, 1, -1);
is_deeply([adb_check_response(@args)], [BAD_ADB, 'Bad ADB status "FAIK"']);
is($args[0]{in}, "FAIK");

@args = ({ in => "OKAY0" }, 2, -1);
is_deeply([adb_check_response(@args)], []);
is($args[0]{in}, "OKAY0");

@args = ({ in => "FAIL0" }, 2, -1);
is_deeply([adb_check_response(@args)], []);
is($args[0]{in}, "FAIL0");

@args = ({ in => "FAIK0" }, 2, -1);
is_deeply([adb_check_response(@args)], [BAD_ADB, 'Bad ADB status "FAIK"']);
is($args[0]{in}, "FAIK0");

# Check $nr is false
for my $add (1..3) {
    @args = ({ in => "OKA" }, $add, 0);
    is_deeply([adb_check_response(@args)], []);
    is($args[0]{in}, "OKA");

    @args = ({ in => "FAI" }, $add, 0);
    is_deeply([adb_check_response(@args)], []);
    is($args[0]{in}, "FAI");

    @args = ({ in => "FAF" }, $add, 0);
    is_deeply([adb_check_response(@args)], [BAD_ADB, 'Bad ADB status "FAF"']);
    is($args[0]{in}, "FAF");
}

for my $add (1..4) {
    @args = ({ in => "OKAY" }, $add, 0);
    is_deeply([adb_check_response(@args)], [SUCCEEDED, '']);
    is($args[0]{in}, "");

    @args = ({ in => "FAIL" }, $add, 0);
    is_deeply([adb_check_response(@args)], []);
    is($args[0]{in}, "FAIL");

    @args = ({ in => "FAIF" }, $add, 0);
    is_deeply([adb_check_response(@args)], [BAD_ADB, 'Bad ADB status "FAIF"']);
    is($args[0]{in}, "FAIF");
}

for my $add (1..5) {
    @args = ({ in => "OKAY0" }, $add, 0, 1);
    if ($add == 1) {
        # Previous "OKAY" should already have triggered
        is_deeply([adb_check_response(@args)], [ASSERTION, 'Assertion: Should already have processed "OKAY0"']);
    } else {
        is_deeply([adb_check_response(@args)], [BAD_ADB, 'Spurious bytes in ADB response "OKAY0"']);
    }
    is($args[0]{in}, "OKAY0");

    # Don't expect EOF
    @args = ({ in => "OKAY0" }, $add, 0, 0);
    if ($add == 1) {
        # Previous "OKAY" should already have triggered
        is_deeply([adb_check_response(@args)], [ASSERTION, 'Assertion: Should already have processed "OKAY0"']);
        is($args[0]{in}, "OKAY0");
    } else {
        is_deeply([adb_check_response(@args)], [SUCCEEDED, '']);
        is($args[0]{in}, "0");
    }

    @args = ({ in => "FAIL0" }, $add, 0);
    is_deeply([adb_check_response(@args)], []);
    is($args[0]{in}, "FAIL0");

    @args = ({ in => "FAIF0" }, $add, 0);
    if ($add == 1) {
        # Since the previous "FAIF" should already have triggered it's ok
        # to get no error here
        is_deeply([adb_check_response(@args)], []);
    } else {
        is_deeply([adb_check_response(@args)], [BAD_ADB, 'Bad ADB status "FAIF"']);
    }
    is($args[0]{in}, "FAIF0");
}

# Check a spurious long string
@args = ({ in => "OKAY" . $long_string}, 1+length $long_string, 0, 1);
is_deeply([adb_check_response(@args)],
          [BAD_ADB, 'Spurious bytes in ADB response "OKAYABC\nde\"zz\0fg\th\301zwarf\tha' . "-" x (2*DISPLAY_MAX()-length($args[0]{in})) . '"...']);
is($args[0]{in}, "OKAY" . $long_string);

# Check a spurious long string
@args = ({ in => "OKAY0001" . $long_string}, 1+length $long_string, -1, 1);
is_deeply([adb_check_response(@args)],
          [BAD_ADB, 'Spurious bytes in ADB response "OKAY0001ABC\nde\"zz\0fg\th\301zwarf\tha' . "-" x (2*DISPLAY_MAX()-length($args[0]{in})) . '"...']);
is($args[0]{in}, "OKAY0001" . $long_string);

# Check a spurious long string
@args = ({ in => "OKAY" . $long_string}, 1+length $long_string, 0, 0);
is_deeply([adb_check_response(@args)],
          [SUCCEEDED, '']);
is($args[0]{in}, $long_string);

# Check a spurious long string
@args = ({ in => "OKAY0001" . $long_string}, 1+length $long_string, -1, 0);
is_deeply([adb_check_response(@args)],
          [SUCCEEDED, 'A']);
is($args[0]{in}, substr($long_string, 1));

# Check for bad hex length
for my $add (1..5) {
    @args = ({ in => "OKAYQ" }, $add, -1);
    is_deeply([adb_check_response(@args)], [BAD_ADB, 'Bad ADB hex length "Q"']);
    is($args[0]{in}, "OKAYQ");
}

for my $add (1..8) {
    @args = ({ in => "OKAY000Q" }, $add, -1);
    is_deeply([adb_check_response(@args)], [BAD_ADB, 'Bad ADB hex length "000Q"']);
    is($args[0]{in}, "OKAY000Q");
}

for my $add (1..4) {
    # Bad ADB status is ignored since it should have been detected earlier
    @args = ({ in => "ZZZZ000Q" }, $add, -1);
    is_deeply([adb_check_response(@args)], [BAD_ADB, 'Bad ADB hex length "000Q"']);
    is($args[0]{in}, "ZZZZ000Q");
}

for my $add (1..8) {
    @args = ({ in => "OKAY0000" }, $add, -1);
    is_deeply([adb_check_response(@args)], [SUCCEEDED, '']);
    is($args[0]{in}, "");

    @args = ({ in => "FAIL0000" }, $add, -1);
    is_deeply([adb_check_response(@args)], [FAILED, '']);
    is($args[0]{in}, "");

    @args = ({ in => "ZZZZ0000" }, $add, -1);
    if ($add <= 4) {
        # Bad ADB status should have been detected earlier
        is_deeply([adb_check_response(@args)], [ASSERTION, 'Assertion: adb response status is suddenly "ZZZZ"']);
        is($args[0]{in}, "ZZZZ0000");
    } else {
        is_deeply([adb_check_response(@args)], [BAD_ADB, 'Bad ADB status "ZZZZ"']);
        is($args[0]{in}, "ZZZZ0000");
    }
}

for my $add (1..9) {
    # Check spurious bytes
    @args = ({ in => "OKAY0000Z" }, $add, -1, 1);
    if ($add == 1) {
        is_deeply([adb_check_response(@args)], [ASSERTION, 'Assertion: Should already have processed "OKAY0000Z"']);
    } else {
        is_deeply([adb_check_response(@args)], [BAD_ADB, 'Spurious bytes in ADB response "OKAY0000Z"']);
    }
    is($args[0]{in}, "OKAY0000Z");

    @args = ({ in => "FAIL0000Z" }, $add, -1, 1);
    if ($add == 1) {
        is_deeply([adb_check_response(@args)], [ASSERTION, 'Assertion: Should already have processed "FAIL0000Z"']);
    } else {
        is_deeply([adb_check_response(@args)], [BAD_ADB, 'Spurious bytes in ADB response "FAIL0000Z"']);
    }
    is($args[0]{in}, "FAIL0000Z");

    # Fail implies expect_eof
    @args = ({ in => "FAIL0000Z" }, $add, -1, 0);
    if ($add == 1) {
        is_deeply([adb_check_response(@args)], [ASSERTION, 'Assertion: Should already have processed "FAIL0000Z"']);
    } else {
        is_deeply([adb_check_response(@args)], [BAD_ADB, 'Spurious bytes in ADB response "FAIL0000Z"']);
    }
    is($args[0]{in}, "FAIL0000Z");

    @args = ({ in => "ZZZZ0000Z" }, $add, -1);
    if ($add <= 5) {
        if ($add == 1) {
            is_deeply([adb_check_response(@args)], [ASSERTION, 'Assertion: Should already have processed "ZZZZ0000Z"']);
            is($args[0]{in}, "ZZZZ0000Z");
        } else {
            # Bad ADB status should have been detected earlier
            is_deeply([adb_check_response(@args)], [BAD_ADB, 'Spurious bytes in ADB response "ZZZZ0000Z"']);
            is($args[0]{in}, "ZZZZ0000Z");
        }
    } else {
        is_deeply([adb_check_response(@args)], [BAD_ADB, 'Bad ADB status "ZZZZ"']);
        is($args[0]{in}, "ZZZZ0000Z");
    }
}

for my $add (1..9) {
    # Check spurious bytes
    @args = ({ in => "OKAY0001Z" }, $add, -1);
    is_deeply([adb_check_response(@args)], [SUCCEEDED, 'Z']);
    is($args[0]{in}, "");

    @args = ({ in => "FAIL0001Z" }, $add, -1);
    is_deeply([adb_check_response(@args)], [FAILED, 'Z']);
    is($args[0]{in}, "");

    @args = ({ in => "ZZZZ0001Z" }, $add, -1);
    if ($add <= 5) {
        # Bad ADB status should have been detected earlier
        is_deeply([adb_check_response(@args)], [ASSERTION, 'Assertion: adb response status is suddenly "ZZZZ"']);
        is($args[0]{in}, "ZZZZ0001Z");
    } else {
        is_deeply([adb_check_response(@args)], [BAD_ADB, 'Bad ADB status "ZZZZ"']);
        is($args[0]{in}, "ZZZZ0001Z");
    }
}

# Skipped beyong bad code
for my $add (1..2) {
    # Bad ADB hex length
    @args = ({ in => "OKAY000QZZ" }, $add, -1);
    is_deeply([adb_check_response(@args)],
              [ASSERTION, 'Assertion: adb hex length is suddenly "000Q"']);
    is($args[0]{in}, "OKAY000QZZ");

    # Bad status, short hex length
    @args = ({ in => "OKAK0001ZZ" }, $add, -1);
    if ($add == 1) {
        is_deeply([adb_check_response(@args)],
                  [ASSERTION, 'Assertion: Should already have processed "OKAK0001ZZ"']);
    } else {
        is_deeply([adb_check_response(@args)],
                  [BAD_ADB, 'Spurious bytes in ADB response "OKAK0001ZZ"']);
    }
    is($args[0]{in}, "OKAK0001ZZ");

    # Bad status, long hex length
    @args = ({ in => "OKAK0003ZZ" }, $add, -1);
    is_deeply([adb_check_response(@args)], []);
    is($args[0]{in}, "OKAK0003ZZ");

    # Bad status, exact hex length
    @args = ({ in => "OKAK0002ZZ" }, $add, -1);
    is_deeply([adb_check_response(@args)],
              [ASSERTION, 'Assertion: adb response status is suddenly "OKAK"']);
    is($args[0]{in}, "OKAK0002ZZ");

    # Bad status AND bad hex length
    @args = ({ in => "OKAK000QZZ" }, $add, -1);
    is_deeply([adb_check_response(@args)],
              [ASSERTION, 'Assertion: adb hex length is suddenly "000Q"']);
    is($args[0]{in}, "OKAK000QZZ");
}
