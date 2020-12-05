package ADB::Client::Command;
use strict;
use warnings;

our $VERSION = '1.000';

use Carp;
our @CARP_NOT = qw(ADB::Client);

use ADB::Client::Utils qw(display_string info $DEBUG $QUIET);

use constant {
    # Index in @COMMANDS element
    COMMAND_NAME	=> 0,
    COMMAND	=> 1,
    NR_BYTES	=> 2,
    FLAGS	=> 3,
    PROCESS	=> 4,
    # CODE is used for SPECIAL commands
    # Cannot reuse PROCESS for this since PROCESS will run at command retirement
    # (and we want that since it could be useful)
    # It's up to the SPECIAL commands to give meaning to elements after this
    CODE	=> 2,

    SPECIAL	=> "",
};

my @FLAGS;
BEGIN {
    # Do the the constants for FLAGS automatically
    # (So I don't have to keep the list of powers of 2 consistent by hand)
    @FLAGS = (
        # FLAGS values:
        # After OKAY we still expect the connection to be closed
        "EXPECT_EOF",

        # After the expected number of bytes more bytes may follow even though
        # we didn't send a new command.
        # Should never appear in combination with EXPECT_EOF
        # The combination of SYNC and MAYBE_MORE implies an on_progress callback
        "MAYBE_MORE",

        # FAIL may or may not close the connection
        # Currently not relevant since we always close the connection ourselves
        # when we see a FAIL
        # Mainly describes host:transport which may close the connection or not
        # depending on the adb version
        "MAYBE_EOF",

        # This command needs an active transport
        "TRANSPORT",

        # Commands like host:features need a transport. But one doesn't need to
        # be active, it will do an implicit host:transport-any first if no
        # transport is active. You can change the implicit transport by using
        # variations like:
        #    host-usb:features
        #    host-local:features
        #    host-serial:<serial>:features
        # Many (all ?) host command like host:version also accept these
        # alternate syntaxes, but typically we don't set the SERIAL flag for
        # them since they ignore the transport given.
        "SERIAL",

        # Convert from UTF-8 after we recieve data from the ADB server
        "UTF8_IN",

        # Convert to UTF-8 before we send arguments to the ADB server
        "UTF8_OUT",

        # Don't set this. It's for internal use
        "PHASE1",

        # First we expect an OKAY and only then do the "normal" processing
        # The stuff after the first OKAY is allowed to arrive immediately
        # Implies a transaction_timeout2 parameter
        "PHASE2",

        # This command needs an active sync
        # The combination of SYNC and MAYBE_MORE implies an on_progress callback
        "SYNC",

        # Special handling for send
        "SEND",

        # Special handling for recv
        "RECV",

        # Needs ROOT (should only appear with TRANSPORT (or SYNC))
        "ROOT",
    );
    my $code = "use constant {\n";
    for my $i (0..$#FLAGS) {
        $code .= sprintf("    %s => %d,\n", $FLAGS[$i], 2**$i);
    }
    $code .= "};\n1";
    eval $code || die $@;
}
use Exporter::Tidy
    other	=> [
        qw(SPECIAL COMMAND_NAME COMMAND NR_BYTES FLAGS PROCESS CODE)],
    flags	=> \@FLAGS;

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
        if ($command_ref->[FLAGS] & SYNC) {
            if (@_) {
                defined $_[0] || croak "Argument is undef";
                if ($command_ref->[FLAGS] & UTF8_OUT) {
                    my $out = $_[0];
                    utf8::encode($out);
                    $command->{OUT} = pack("a4V/a*", $command_ref->[COMMAND], $out);
                } elsif (utf8::is_utf8($_[0])) {
                    my $out = $_[0];
                    if (!utf8::downgrade($out, 1)) {
                        my $out = display_string($out);
                        croak "Argument cannot be converted to native 8 bit encoding: $out";
                    }
                    $command->{OUT} = pack("a4V/a*", $command_ref->[COMMAND], $out);
                } else {
                    $command->{OUT} = pack("a4V/a*", $command_ref->[COMMAND], @_);
                }
            } else {
                $command->{OUT} = pack("a4x4", $command_ref->[COMMAND]);
            }
        } else {
            my $out = sprintf($command_ref->[COMMAND], @_);
            if ($command_ref->[FLAGS] & UTF8_OUT) {
                utf8::encode($out);
            } elsif (!utf8::downgrade($out, 1)) {
                $out = display_string($out);
                croak "Argument cannot be converted to native 8 bit encoding: $out";
            }
            if (length $out >= 2**16) {
                $out = display_string($out);
                croak "Command too long: $out";
            }
            $command->{OUT} = sprintf("%04X", length $out) . $out;
        }
        $command->{ARGUMENTS} = [@_];
    }
    $command->{COMMAND_REF} = $command_ref;
}

sub arguments {
    return shift->{ARGUMENTS} if @_ <= 1;

    my $command = shift;
    $command->{ARGUMENTS} = shift;
    return;
}

sub command_name {
    return shift->{COMMAND_REF}[COMMAND_NAME];
}

sub out {
    return shift->{OUT};
}

sub data {
    my $command = shift;

    return ref $command->{DATA} eq "" ? \$command->{DATA} : $command->{DATA} if !@_;
    $command->{DATA} = shift;
}

sub objects {
    return $objects;
}

END {
    # $QUIET first for easier code coverage
    info("Still have %d %s objects at program end", $objects, __PACKAGE__) if !$QUIET && $objects;
}

1;
