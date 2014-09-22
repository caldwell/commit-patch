VERSION := $(shell perl -ne '/VERSION\s*=\s*'"'(.*)'"'/ and print "$$1"' commit-patch)

BIN = commit-patch commit-partial commit-patch.fat
MAN = commit-patch.1 commit-partial.1
ELISP = commit-patch-buffer.el
DOC = commit-patch.html README COPYING Changes
ALL = $(BIN) $(MAN) $(ELISP) $(DOC)

all: $(ALL)

commit-patch.fat: commit-patch
	carton exec fatpack pack $< > $@
	chmod +x $@

commit-partial:
	ln -s commit-patch commit-partial

commit-patch.1: commit-patch
	pod2man -c "User Commands" $< > $@

commit-partial.1:
	ln -s commit-patch.1 commit-partial.1

commit-patch.html: commit-patch
	pod2html --title="commit-patch Documentation" $< > $@

release: commit-patch-$(VERSION).tar.gz

commit-patch-$(VERSION).tar.gz: $(ALL) Makefile
	if ! git diff --quiet -- $^; then tput setab 1; tput setaf 3; /bin/echo -n 'WARNING: Directory is not clean!'; tput sgr0; echo; fi
	mkdir commit-patch-$(VERSION)
	rsync -a $^ commit-patch-$(VERSION)
	mv -f commit-patch-$(VERSION)/commit-patch.fat commit-patch-$(VERSION)/commit-patch
	tar czf commit-patch-$(VERSION).tar.gz commit-patch-$(VERSION)
	rm -rf commit-patch-$(VERSION)

test:
	./test/run-tests
.PHONY: test

PREFIX=/usr/local
install: $(ALL)
	mkdir -p "$(PREFIX)/bin"
	mkdir -p "$(PREFIX)/share/man/man1"
	mkdir -p "$(PREFIX)/share/emacs/site-lisp"
	mkdir -p "$(PREFIX)/share/doc/commit-patch"
	cp -a $(BIN)   "$(PREFIX)/bin"
	cp -a $(MAN)   "$(PREFIX)/share/man/man1"
	cp -a $(ELISP) "$(PREFIX)/share/emacs/site-lisp"
	cp -a $(DOC)   "$(PREFIX)/share/doc/commit-patch"
