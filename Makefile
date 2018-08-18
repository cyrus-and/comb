SHELL := /bin/bash

.ONESHELL:
.PHONY: check clean

check: clean
	@emacs -Q -L . --batch --eval '
	(dolist (file (directory-files "." nil "\\.el$$"))
	  (check-declare-file file)
	  (byte-compile-file file))'

clean:
	@rm -f *.elc
