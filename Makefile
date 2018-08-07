.PHONY: check clean

check:
	@for i in *.el; do emacs --batch -Q -L . -f batch-byte-compile "$$i"; done
	@emacs --batch -Q -L . --eval '(check-declare-directory ".")'

clean:
	rm -f *.elc
