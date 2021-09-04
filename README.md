commit-patch
============

Commit patches to Darcs, Git, Mercurial, Bazaar, Monotone, Subversion, or CVS


Installation
------------

On a recent Debian/Ubuntu:

    apt install commit-patch

Anywhere else: Install the prerequisites below and download the release
tarball from the [homepage](https://porkrind.org/commit-patch/). This
tarball contains a "fatpackad" version of the `commit-patch` perl script
with the external perl dependencies embedded. Then put the commit-patch
and commit-partial binaries into your `PATH`.


Prerequisites
-------------

`commit-patch` is known to run on Linux and Mac OS X. It is perl,
so ideally it will run anywhere, but we have never tested in
other environments, most notably Windows. Use at your own risk.

`commit-patch` relies on several programs to get the job done:

- [perl](https://www.perl.org)
- [patch](https://www.gnu.org/software/patch/)
- [interdiff](http://cyberelk.net/tim/software/patchutils/)
- cp - Ideally installed on your system already. :-)

and, of course, one of:

- [git](https://git-scm.com/)
- [mercurial](https://subversion.apache.org/)
- [darcs](http://darcs.net/)
- [subversion](http://subversion.tigris.org/)
- [cvs](https://www.nongnu.org/cvs/)
- [bazaar](https://bazaar.canonical.com/)
- [monotone](https://www.monotone.ca/)

## Installing Prerequisites

On Debian/Ubuntu:

    apt-get install patch patchutils

On Fedora:

    yum install patch patchutils

On Mac OS X w/ [Homebrew](https://brew.sh)

    brew install patchutils


Instructions
------------

## commit-patch

See the man page or perldoc:

    man ./commit-patch.1
    perldoc commit-patch


## commit-patch-buffer.el

commit-patch-buffer.el is an emacs interface to `commit-patch`. It
allows you to just hit `C-c C-c` in any patch buffer to apply and commit
only the changes indicated by the patch, regardless of the changes in
your working directory.

To use commit-patch-buffer with diff mode automatically, add this to
your emacs init file:

    (eval-after-load 'diff-mode
      '(require 'commit-patch-buffer nil 'noerror))

The easy way of working with commit-patch-buffer is to `M-x vc-diff` a
file (or `M-x vc-root-diff` your whole project) then kill, split or edit
the resulting hunks using diff mode's built-in commands and to then hit
`C-c C-c` to commit the patch.


Development
-----------

`commit-patch` uses [Carton](https://github.com/perl-carton/carton) for
local development. Once Carton is installed:

    carton install

After than `commit-patch` and `commit-partial` should work (there is no
need to `carton exec` them—the code autodetects the local libs.


Homepage
--------
https://porkrind.org/commit-patch/

Authors
-------
- David Caldwell <david@porkrind.org>
- Jim Radford <radford@blackbean.org>

Copyright and License
---------------------
Copyright © 2003-2021 by David Caldwell and Jim Radford.

`commit-patch` is distributed under the GNU General Public
License. See the COPYING file in the distribution for more
details.
