package ADB::Client::Command;
use strict;
use warnings;

our $VERSION = '1.000';

use Carp;
# our @CARP_NOT = qw(ADB::Client::Ref);

use ADB::Client::Utils qw(adb_check_response display_string info $DEBUG $QUIET);

use Exporter::Tidy
    other	=>[
        qw(command_check_response
           COMMAND_NAME COMMAND NR_RESULTS FLAGS PROCESS CODE EXPECT_EOF
           MAYBE_EOF SERIAL COMMAND_REF CALLBACK ARGUMENTS STATE)];

use constant {
    # Index in @COMMANDS element
    COMMAND_NAME	=> 0,
    COMMAND	=> 1,
    NR_RESULTS	=> 2,
    FLAGS	=> 3,
    PROCESS	=> 4,
    # CODE is used for SPECIAL commands
    # Cannot reuse PROCESS for this since PROCESS will run at command retirement
    # (and we want that since it could be useful)
    # It's up to the SPECIAL commands to give meaning to elements after this
    CODE	=> 2,

    # FLAGS values:
    # After OKAY we still expect the connection to be closed
    EXPECT_EOF	=> 1,
    # FAIL may or may not close the connection
    # Currently not relevant since we always close the connection ourselves
    # if we see a FAIL
    # Mainly meant for host:transport which may close the connection or not
    # depending on the adb version
    MAYBE_EOF	=> 2,
    SERIAL	=> 4,

    # Index in ADB::Client::Command
    COMMAND_REF	=> 0,
    CALLBACK	=> 1,
    # possibly unify ARGUMENTS and STATE
    ARGUMENTS	=> 2,
    STATE	=> 3,

    EMPTY_ARGUMENTS	=> [],
};

my $objects = 0;

sub new {
    my ($class, $command_ref, $callback, $state, $arguments) = @_;
    ++$objects;
    return bless [$command_ref,
                  $callback,
                  $arguments || EMPTY_ARGUMENTS,
                  $state // ()], $class;
}

sub DESTROY {
    --$objects;
    info("DESTROY @_") if $DEBUG;
}

sub ref : method {
    my $command = shift;
    my $command_ref = shift // return $command->[COMMAND_REF];

    my $out = sprintf($command_ref->[COMMAND], @_);
    utf8::encode($out);
    if (length $out >= 2**16) {
        $out = display_string($out);
        croak "Command too long: $out";
    }
    $command->[COMMAND_REF] = $command_ref;
    $command->[ARGUMENTS] = sprintf("%04X", length $out) . $out;
}

sub command_name {
    return shift->[COMMAND_REF][COMMAND_NAME];
}

sub arguments {
    return shift->[ARGUMENTS];
}

sub command_check_response {
    my ($data, $len_added, $command_ref) = @_;

    return adb_check_response($data, $len_added, $command_ref->[NR_RESULTS],
                              $command_ref->[FLAGS] & EXPECT_EOF);
}

sub objects {
    return $objects;
}

END {
    # $QUIET first for easier code coverage
    info("Still have %d %s objects at program end", $objects, __PACKAGE__) if !$QUIET && $objects;
}

1;
