VERSION := $(shell perl -ne '/VERSION\s*=\s*'"'(.*)'"'/ and print "$$1"' commit-patch)

all: commit-partial commit-patch.1 commit-partial.1 commit-patch.html

commit-partial:
	ln -s commit-patch commit-partial

commit-patch.1: commit-patch
	pod2man -c "User Commands" $< > $@

commit-partial.1:
	ln -s commit-patch.1 commit-partial.1

commit-patch.html: commit-patch
	pod2html --title="commit-patch Documentation" $< > $@

release: commit-patch-$(VERSION).tar.gz

commit-patch-$(VERSION).tar.gz: commit-patch commit-partial commit-patch-buffer.el commit-patch.1 commit-partial.1 commit-patch.html Makefile README COPYING Changes
	mkdir commit-patch-$(VERSION)
	rsync -a $^ commit-patch-$(VERSION)
	tar czf commit-patch-$(VERSION).tar.gz commit-patch-$(VERSION)
	rm -rf commit-patch-$(VERSION)
