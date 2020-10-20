package ADB::Client::Events;
# A small event core
use strict;
use warnings;
use Carp;
use Errno qw(EINTR);

use ADB::Client::Timer;

my $read_mask  = "";
my $write_mask = "";
my $error_mask = "";
my (%read_refs, %write_refs, %error_refs, $inited);

package ADB::Client;
our ($debug, $verbose);

package ADB::Client::Events;
sub add_read(*$ ) {
    my $fd = fileno(shift) // croak "Not a filehandle";
    ADB::Client::caller_info("add_read $fd") if $debug;
    croak "Descriptor $fd already selected for read" if $read_refs{$fd};
    $read_refs{$fd} = shift;
    vec($read_mask, $fd, 1) = 1;
}

sub add_write(*$ ) {
    my $fd = fileno(shift) // croak "Not a filehandle";
    ADB::Client::caller_info("add_write $fd") if $debug;
    croak "Descriptor $fd already selected for write" if $write_refs{$fd};
    $write_refs{$fd} = shift;
    vec($write_mask, $fd, 1) = 1;
}

sub add_error(*$ ) {
    my $fd = fileno(shift) // croak "Not a filehandle";
    ADB::Client::caller_info("add_error $fd") if $debug;
    croak "Descriptor $fd already selected for error" if $error_refs{$fd};
    $error_refs{$fd} = shift;
    vec($error_mask, $fd, 1) = 1;
}

sub delete_read(*) {
    my $fd = fileno(shift) // croak "Not a filehandle";
    ADB::Client::caller_info("delete_read $fd") if $debug;
    croak "Descriptor $fd wasn't selected for read" unless $read_refs{$fd};
    # This strange assign before delete is to poison the reference @work in
    # sub mainloop may still have
    $read_refs{$fd} = undef;
    delete $read_refs{$fd};
    if (%read_refs) {
        vec($read_mask, $fd, 1) = 0;
        $read_mask =~ s/\x00+\z//;
    } else {
        $read_mask = "";
    }
}

sub delete_write(*) {
    my $fd = fileno(shift) // croak "Not a filehandle";
    ADB::Client::caller_info("delete_write $fd") if $debug;
    croak "Descriptor $fd wasn't selected for write " unless $write_refs{$fd};
    # This strange assign before delete is to poison the reference @work in
    # sub mainloop may still have
    $write_refs{$fd} = undef;
    delete $write_refs{$fd};
    if (%write_refs) {
        vec($write_mask, $fd, 1) = 0;
        $write_mask =~ s/\x00+\z//;
    } else {
        $write_mask = "";
    }
}

sub delete_error(*) {
    my $fd = fileno(shift) // croak "Not a filehandle";
    ADB::Client::caller_info("delete_error $fd") if $debug;
    croak "Descriptor $fd wasn't selected for error " unless $error_refs{$fd};
    # This strange assign before delete is to poison the reference @work in
    # sub mainloop may still have
    $error_refs{$fd} = undef;
    delete $error_refs{$fd};
    if (%error_refs) {
        vec($error_mask, $fd, 1) = 0;
        $error_mask =~ s/\x00+\z//;
    } else {
        $error_mask = "";
    }
}

sub mainloop {
    init() if !$inited;
    while (1) {
        my $timeout = ADB::Client::Timer::run_now();
        last unless defined $timeout ||
            %read_refs || %write_refs || %error_refs;
        if ((select(my $r = $read_mask, my $w = $write_mask, my $e = $error_mask, $timeout) || next) > 0) {
            $$_ && $$_->() for
                \@read_refs{ grep vec($r, $_, 1), keys %read_refs},
                \@write_refs{grep vec($w, $_, 1), keys %write_refs},
                \@error_refs{grep vec($e, $_, 1), keys %error_refs};
        } elsif ($! != EINTR) {
            die "Select failed: $^E";
        }
    }
    ::info("Exiting mainloop") if $verbose;
}

sub init {
    no warnings "once";
    *IO::Handle::add_read     = \&add_read;
    *IO::Handle::add_write    = \&add_write;
    *IO::Handle::add_error    = \&add_error;
    *IO::Handle::delete_read  = \&delete_read;
    *IO::Handle::delete_write = \&delete_write;
    *IO::Handle::delete_error = \&delete_error;
    $inited = 1;
}

1;
