# AGENTS.md

Guidance for coding agents working in `~/.emacs.d`.

## Layout

- `early-init.el`: pre-GUI startup tweaks; enables `package-quickstart`.
- `init.el`: the whole config, mostly deferred `use-package` blocks.
- `elisp/`: vendored packages.
- Everything else (`custom.el`, `calc.el`, `elpa/`, caches) is written by Emacs.

## Git

The repo is the dotfiles repo at `~`.  Its `.gitignore` ignores everything, so add files with `git add -f`.  Commit messages look like `init.el += org-move-subtree-to-bottom/top`.

## Contracts

### Fast startup

Fast startup is critical.  Run the startup performance check under Test before and after each change, and report both results.

- Init time, best of three runs, must not grow.
- Never load `package.el` at startup: `package=` must be nil.
- Never use `:demand t` without asking.

### No keybinding clashes

A new or changed binding must not clash with any other.  Outside Emacs, keys are named by Command and Option.  Inside Emacs, Command is Meta and Option is Super.  Check:

- Emacs: global map, active modes, and `init.el`
- Hammerspoon: `~/.hammerspoon`
- AltTab preferences
- macOS keyboard shortcuts

### use-package comments

A comment block in this form sits directly above every `use-package` block:

```elisp
;; Foo - one-line description
;; https://example.com/foo
;;
;; Custom keys:
;;   C-c f  foo-run
;;   M-n    unbound (was foo-next)
;; Requires:
;;   $ brew install foo
(use-package foo
```

- `Custom keys:` is required if the block binds, rebinds, or unbinds any key, anywhere (`:bind`, `define-key`, hooks).  List every one; the comment alone must reveal every custom key.
- `Requires:` is required if the package needs an external tool.

### Off limits

- Edit only `init.el`, `early-init.el`, and this file.
- Do not install or upgrade packages without asking.

## Test

Load check.  Must load with no errors.  `--batch` skips `early-init.el`, hence `package-initialize`:

```console
$ emacs --batch --eval '(package-initialize)' --load ~/.emacs.d/init.el
```

Startup performance check.  Measures init time and whether `package.el` loaded.  Briefly opens an Emacs window:

```console
$ emacs --eval '(progn (princ (format "%s package=%s\n" (emacs-init-time) (featurep (quote package))) (function external-debugging-output)) (kill-emacs))'
0.381755 seconds package=nil
```
