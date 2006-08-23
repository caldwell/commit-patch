#!/usr/bin/perl -w

use strict;
use Cwd qw(abs_path);
use IPC::Run qw(run);
use File::Temp qw(tempfile);
use Getopt::Long;

my $message;
GetOptions("message|m=s" => \$message) and scalar @ARGV == 1 or die "Usage: $0 [--message|-m <message>] <patch-file>\n";
my $patch = shift;

die "bad patch file: $!" if -z $patch;
die "Invalid message" if defined $message && $message eq "";

my ($lsdiff_out, $err);
run ["lsdiff", "--strip", "1", $patch], '|', ["sort"], '|', ["uniq"], \$lsdiff_out, \$err or die "lsdiff|sort|unique : $! ($err)";
my @files = split(/\n/, $lsdiff_out);
die "No files in patch" unless scalar @files;

my %clean;
my $repo=".";
my %vc;
while (!$vc{name}) {
    if (-d "$repo/cvs" && $repo eq '.') {
        $vc{name}         = 'cvs';
        $vc{diff}         = 'cvs diff -u';
        $vc{commit}       = 'cvs commit';
        $vc{message_opt}  = '-m';
        $vc{add}          = 'cvs add';
        $vc{remove}       = 'cvs remove';
    } elsif (-d "$repo/_darcs") {
        $vc{name}         = 'darcs';
        $vc{diff}         = 'darcs diff -u';
        $vc{add}          = 'darcs add';
        $vc{remove}       = 'true';
        $vc{commit}       = 'darcs record --all';
        if ($message) {
            # Darcs doesn't like multiline -m comments so we have to put the log message into a file and use --logfile. Yuck.
            #$vc{message_opt} = '-m';
            $message .= "\n" unless $message =~ /\n$/s; # Darcs does screwey stuff when logfile has no trailing \n.
            my ($message_file, $message_filename) = tempfile("commit-patch-message-XXXXXXXX", UNLINK=>0);
            print $message_file $message;
            close $message_file;
            $clean{$message_filename} = undef; # make sure we delete this file on exit.
            $vc{commit}  .= ' --logfile='.$message_filename;
        } else {
        }
    } elsif (-d "$repo/.hg") {
        $vc{name}         = 'hg';
        $vc{diff}         = 'hg diff';
        $vc{commit}       = 'hg commit';
        $vc{message_opt}  = '-m';
        $vc{add}          = 'hg addremove';
        $vc{remove}       = 'true';
    } else {
        $repo.="/..";
        printf("Trying back a dir: $repo, abs:%s\n", abs_path($repo));
        die "couldn't find repo" if abs_path($repo) eq "/";
    }
}

#print "Found $vc{name} in $repo\n";
#printf("files: %s\n", join(",", @files));

eval {
    for my $f (@files) {
        run ["cp", "-f", $f, "$f.orig.$$"] or die "couldn't make backup of $f: $!";
        $clean{"$f.orig.$$"} = $f;
    }
    $SIG{PIPE} = $SIG{INT} = $SIG{QUIT} = sub { clean(); print "Cleanly aborted\n"; };

    $clean{"working.patch.$$"} = $clean{"non_committed.patch.$$"} = undef;

    my ($out,$err,$non_committed_patch);
    run([split(/ /,$vc{add}),    @files],                                               '>', \$out, '2>', \$err);# Expect these 2 to fail when
    run([split(/ /,$vc{remove}), @files],                                               '>', \$out, '2>', \$err);# there are no new files.
    run([split(/ /,$vc{diff}),   @files], '>', "working.patch.$$",                                  '2>', \$err) or die "$err\n";
    run(["interdiff", "-p1", $patch, "working.patch.$$"], '>', "non_committed.patch.$$",            '2>', \$err) or die "$err\n";
    run([qw(patch -p1 -R)], '<', "working.patch.$$",                                    '>', \$out, '2>', \$err) or die "$out\n$err\n";
    run([qw(patch -p1)], '<', $patch,                                                   '>', \$out, '2>', \$err) or die "$out\n$err\n";
    # Don't capture stdout or stderr because it can be interactive (cough cough darcs)
    run([split(/ /,$vc{commit}), $message && $vc{message_opt} ? ($vc{message_opt}, $message) : (), @files],debug => 0) or die "commit failed.\n";
    run([qw(patch -p1)], '<', "non_committed.patch.$$",                                 '>', \$out, '2>', \$err) or die "$out\n$err\n";
};
sub clean() {
    foreach my $k (keys %clean) { rename $k,$clean{$k} if $clean{$k} }
}
if ($@) {
    clean();
    die "Failed: $@";
}
foreach my $k (keys %clean) { unlink $k }
