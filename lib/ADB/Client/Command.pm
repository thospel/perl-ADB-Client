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
           SPECIAL COMMAND_NAME COMMAND NR_RESULTS FLAGS PROCESS CODE
           EXPECT_EOF MAYBE_EOF MAYBE_MORE SERIAL TRANSPORT)];

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
    # After the expected number of bytes more bytes may follow even though we
    # didn't send a new command.
    # Should never appear in combination with EXPECT_EOF
    MAYBE_MORE	=> 2,
    # FAIL may or may not close the connection
    # Currently not relevant since we always close the connection ourselves
    # when we see a FAIL
    # Mainly describes host:transport which may close the connection or not
    # depending on the adb version
    MAYBE_EOF	=> 4,
    # This command needs an active transport
    TRANSPORT	=> 8,
    # Commands like host:features need a transport. But one doesn't need to be
    # active, it will do an implicit host:transport-any first if no transport
    # is active. You can change the implicit transport by using variations like:
    #    host-usb:features
    #    host-local:features
    #    host-serial:<serial>:features
    # Many (all ?) host command like host:version also accept these alternate
    # syntaxes, but typically we don't set the SERIAL flag for them since they
    # ignore the transport given.
    SERIAL	=> 16,

    EMPTY_ARGUMENTS	=> [],

    SPECIAL	=> "",
};

my $objects = 0;

# Need to call command_ref to complete this object
sub new {
    @_ % 2 == 1 || croak "Odd nuber of arguments";
    my ($class, %object) = @_;
    ++$objects;
    return bless \%object, $class;
}

sub DESTROY {
    --$objects;
    info("DESTROY @_") if $DEBUG;
}

sub command_ref : method {
    my $command = shift;
    my $command_ref = shift // return $command->{COMMAND_REF};

    if ($command_ref->[COMMAND] ne SPECIAL) {
        my $out = sprintf($command_ref->[COMMAND], @_);
        utf8::encode($out);
        if (length $out >= 2**16) {
            $out = display_string($out);
            croak "Command too long: $out";
        }
        $command->{OUT} = sprintf("%04X", length $out) . $out;
    }
    $command->{COMMAND_REF} = $command_ref;
}

sub arguments {
    return shift->{ARGUMENTS} if @_ <= 1;

    my ($command, $arguments) = @_;
    $command->{ARGUMENTS} = $arguments;
}

sub command_name {
    return shift->{COMMAND_REF}[COMMAND_NAME];
}

sub out {
    return shift->{OUT};
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
