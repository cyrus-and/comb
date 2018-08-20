.PHONY: check clean

check: clean
	@for i in *.el; do emacs --batch -Q -L . -f batch-byte-compile "$$i"; done
	@emacs --batch -Q --eval '(check-declare-directory ".")'

clean:
	@rm -f *.elc
