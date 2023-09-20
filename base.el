;;
;; base
;;

;; encoding

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; local site-lisp
(setq load-path (append '("~/.site-lisp/" ) load-path))
;; (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
;;     (let* ((my-lisp-dir "~/site-lisp/")
;;            (default-directory my-lisp-dir))
;;       (setq load-path (cons my-lisp-dir load-path))
;;       (normal-top-level-add-subdirs-to-load-path)))


(tool-bar-mode -1)
(menu-bar-mode -1)

;; default major mode
(setq default-major-mode 'text-mode)

;; *scratch* buffer will use this
(setq initial-major-mode 'text-mode)

;; don't make backup files like this.txt~
(setq make-backup-files nil)

;; font lock mode for all major modes
(global-font-lock-mode t)

;; only spaces for indentation
(setq-default indent-tabs-mode nil)

;; display tab characters as 4 spaces
(setq default-tab-width 4)

;; indentation width
(setq-default standard-indent 4)

;; linear mouse wheel scroll
(setq-default mouse-wheel-progressive-speed nil)

;; show column numbers next to line numbers
(setq column-number-mode t)

;; number of lines to overlap w/ C-v & M-v
(setq next-screen-context-lines 4)

;; show region highlighting
(transient-mark-mode t)

;; highlight matching parenthesis
(show-paren-mode t)

;; do not show splash screen
(setq inhibit-splash-screen t)

;; display timeclock info in modeline
;;(display-time-mode)

;; show trailing whitespace
(setq show-trailing-whitespace t)

;; smooth vertical scrolling
(setq scroll-conservatively 100)

;; allow narrowing
(put 'narrow-to-region 'disabled nil)

;; tramp
(setq tramp-default-method "ssh")

;; truncate long lines
(setq-default truncate-lines t)

;; blink cursor
(blink-cursor-mode)

;; better buffer naming (complimentary to ido)
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-min-dir-content 1)

;; allow editing of permissions in wdired mode
(setq wdired-allow-to-change-permissions t)

;; ido!
(setq ido-enter-matching-directory 'only)
(setq ido-enable-flex-matching t)
(setq ido-decorations
      '(" (" ")" ", " ", ..." " [" "]"
        " [No match]" " [Matched]"
        " [Not readable]" " [Too big]" " [Confirm]"))

(ido-mode `buffer)

;; sgml
(setq sgml-basic-offset 4)

;; initial scratch message
(setq initial-scratch-message "")

;; dired formatting (humanized dates, no owner)
(setq dired-listing-switches "-halG")

;; hide some files in dired
(require 'dired-x)
(setq-default dired-omit-files-p t) ; this is buffer-local variable
(setq dired-omit-files (concat dired-omit-files "\\|\\.meta$"))
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode)))

;; case insensitive completion for find-file

(setq read-file-name-completion-ignore-case t)

;; stop on subwords
(define-globalized-minor-mode global-subword-mode subword-mode
  (lambda () (subword-mode 1)))
(global-subword-mode 1)

;; disable electric indent
(electric-indent-mode 0)

;; don't require trailing newlines
(setq mode-require-final-newline nil)
(setq require-final-newline nil)

;; global code styling
(setq c-default-style "gnu")
(setq default-tab-width 2)
(setq js-indent-level 2)

;; stop annoying autosave
(setq auto-save-default nil)

;; grep setup
(grep-compute-defaults)
(setq grep-save-buffers nil)

;; css
(setq css-fontify-colors nil)

;; quick yes-no
(fset 'yes-or-no-p 'y-or-n-p)

;; disable annoying bell sound
(setq ring-bell-function 'ignore)