# emacs-conf

Uses ELPA and `package` to bootstrap `load-relative` and `use-package` to set up everything else.

Basic initialization in `.emacs`:

```lisp
(setq
 my-config
 '((font-family . "IBM Plex Mono")
   (font-height . 100)
   (frame-width . 140)
   (frame-height . 40)))

(load "~/.emacs-conf/init")
```
