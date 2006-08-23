all: executable commit-patch.1

# Darcs loses the x bit. :-(
executable:
	chmod +x commit-patch

commit-patch.1: commit-patch
	pod2man -c "User Commands" $< > $@
