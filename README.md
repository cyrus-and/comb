# Comb

Comb is a native Emacs Lisp solution to search, browse and annotate occurrences
of regular expressions in files. The interactive interface allows to perform an
exhaustive classification of all the results to rule out false positives and
asses proper matches during manual static analysis.

<p align="center"><img src="https://i.imgur.com/13toxVQ.png" width="804px" alt="Browse"/></p>

## Installation

### [MELPA] package

```
M-x package-install RET comb
```

[MELPA]: https://melpa.org/#/getting-started

### Local package

```
M-x package-install-file RET /path/to/comb/
```

### Manual

```elisp
(add-to-list 'load-path "/path/to/comb/")
(require 'comb)
```

## Usage

This is a quick walkthrough of some of the features of Comb. To perform a
search:

1. move to the root directory of the repository you want to analyze;

2. run `M-x comb`;

3. press `c` to enter the configuration mode;

4. fill the desired fields and finally perform a search.

<p align="center"><img src="https://i.imgur.com/PLcDGO6.png" width="628px" alt="Configure"/></p>

If there are some results to browse then the `*Comb*` buffer is displayed, from
here it is possible to annotate the results (`!`) and change their status to
approved (`a`/`A`), rejected (`r`/`R`) or undecided (`u`/`U`, the default).

The above actions work on the current result which can be moved to the next
(`n`) or the previous (`p`), in doing so the `*Comb*` buffer is updated to show
the file that contains the result, which is now highlighted. Only results
matching the status filter (cycled with `f`) and the notes filter regexp (set
with `F`) are displayed. In addition to that, `t` spawns a buffer containing the
list of the currently displayed results, this allows to Isearch the snippets and
jump to the result at point.

<p align="center"><img src="https://i.imgur.com/KddKKrQ.png" width="628px" alt="Report"/></p>

Finally it is possible to save the current session to file (`s`) and load it
back to resume the analysis (`l`).

See the help (`h`) for a list of all the features and keybindings.

## Configuration

Some faces can be configured, take a look at the `comb` configuration group
(`M-x configure-group RET comb`).

Additionally, all the keybindings in the `*Comb*` buffer can be altered by
changing the `comb-keymap` keymap. For example, to use the arrows to navigate
the results use:

```elisp
(define-key comb-keymap (kbd "<left>") 'comb-prev)
(define-key comb-keymap (kbd "<right>") 'comb-next)
```

This does not unbind the original keybindings though. It may be convenient to
completely replace the keymap instead so to avoid collisions with existing
modes:

```elisp
(setq comb-keymap (make-sparse-keymap))
(define-key comb-keymap (kbd "x") 'comb-quit)
(define-key comb-keymap (kbd "?") 'comb-help)
(define-key comb-keymap (kbd "<left>") 'comb-prev)
(define-key comb-keymap (kbd "<right>") 'comb-next)
;; ...
```

See the `comb-default-keybindings` alist to obtain the functions used by the
default keybindings.
