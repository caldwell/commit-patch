commit-patch
============

Commit patches to Darcs, Git, Mercurial, Bazaar, Monotone, Subversion, or CVS


Installation
------------

Depends on: 

- [patchutils] (https://github.com/twaugh/patchutils)
  -- Install from your OS pkg manager
- [IPC-Run] (https://github.com/toddr/IPC-Run)
  -- Run following script:

```
git clone https://github.com/toddr/IPC-Run ~/repos/IPC-Run
git clone https://github.com/oneness/commit-patch ~/repos/commit-patch
cd ~/repos/commit-patch && sudo make install
```

Usage
------------

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
the resulting hunks using diff mode's built-in commands and then hit
`C-c C-c` to commit the patch. You can amend previous commit by `C-c C-C` instead.


Notes
-----------

This is my fork of:
- [commit-patch] (https://github.com/caldwell/commit-patch)

I will keep updating this fork to meet my use case. Please fork this
or the original above to change/extend it if your needs are not met.

Copyright and License
---------------------
Copyright © 2003-2021 by David Caldwell and Jim Radford.

`commit-patch` is distributed under the GNU General Public
License. See the COPYING file in the distribution for more
details.
