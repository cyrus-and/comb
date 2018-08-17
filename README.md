# Comb

Comb is a native Emacs Lisp solution to search, browse and annotate occurrences
of regular expressions in files. The interactive interface allows to perform an
exhaustive classification of all the results to rule out false positives and
asses proper matches during manual static analysis.

![Browse](https://i.imgur.com/tFOsCcW.png)

## Installation

From [MELPA]:

```
M-x package-install RET comb
```

[MELPA]: https://melpa.org/#/getting-started

## Usage

This is a quick walkthrough of some of the features of Comb. To perform a
search:

1. move to the root directory of the repository you want to analyze;

2. run `M-x comb`;

3. press `c` to enter the configuration mode;

4. fill the desired fields and finally perform a search.

![Configuration](https://i.imgur.com/lys6L1u.png)

If there are some results to browse then the interactive menu is displayed, from
here it is possible to annotate the results (`!`) and change their status to
approved (`a`/`A`), rejected (`r`/`R`) or undecided (`u`/`U`, the default).

The above actions work on the current result which can be moved to the next
(`n`) or the previous (`p`), in doing so the current buffer is updated and the
result is highlighted. Only results matching the status filter (cycled with `f`)
and the notes filter regexp (set with `F`) are displayed. In addition to that,
`t` spawns a buffer containing a list of the currently displayed results, this
allows to Isearch the snippets and jump to the result at point.

![Report](https://i.imgur.com/Ixskhr5.png)

Finally it is possible to save the current session to file (`s`) and load it
back to resume the analysis (`l`).

See the help (`h`) for a list of all the features and keybindings.
