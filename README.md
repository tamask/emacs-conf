# emacs-conf

Bootstraps `straight.el` and uses `use-package` from there on out.

Basic initialization in `.emacs`:

```lisp
(setq
 my-config
 '(:font-family "IBM Plex Mono"
   :font-height 100
   :frame-width 140
   :frame-height 40))

(load "~/.emacs-conf/init")
```
