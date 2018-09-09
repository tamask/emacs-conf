;;
;; shortcuts
;;

;; window positioning
(global-set-key (kbd "C-=") 'enlarge-window)
(global-set-key (kbd "C--") 'shrink-window)
(global-set-key (kbd "C-+") 'enlarge-window-horizontally)
(global-set-key (kbd "C-_") 'shrink-window-horizontally)

;; window configuration
(global-set-key (kbd "\C-cl") 'jump-to-register)
(global-set-key (kbd "\C-cs") 'window-configuration-to-register)

;; indentation
(global-set-key (kbd "\C-ci") 'increase-left-margin)
(global-set-key (kbd "\C-cd") 'decrease-left-margin)

;; misc
(global-set-key (kbd "C-`") 'rename-buffer)

;; terminal
(global-set-key
 (kbd "C-c t")
 (lambda () (interactive) (ansi-term "/bin/bash")))

;; sort lines
(global-set-key (kbd "C-c C-s") 'sort-lines)

;; old emacs newline keybindings
(define-key global-map (kbd "C-j") 'newline-and-indent)
(define-key global-map (kbd "RET") 'newline)

;; occur
(global-set-key (kbd "C-c o") 'occur)

;; reuse dired buffer
(put 'dired-find-alternate-file 'disabled nil)
(add-hook
 'dired-mode-hook
 (lambda ()
   (define-key dired-mode-map (kbd "^")
     (lambda () (interactive) (find-alternate-file "..")))
   (define-key dired-mode-map (kbd "M-o")
     (lambda () (interactive) (find-alternate-file "..")))
   (define-key dired-mode-map (kbd "M-i") `dired-find-alternate-file)
   ))

;; keyboard macros
(defun toggle-kbd-macro-recording-on ()
  "One-key keyboard macros: turn recording on."
  (interactive)
  (define-key global-map (this-command-keys)
    'toggle-kbd-macro-recording-off)
  (start-kbd-macro nil))

(defun toggle-kbd-macro-recording-off ()
  "One-key keyboard macros: turn recording off."
  (interactive)
  (define-key global-map (this-command-keys)
    'toggle-kbd-macro-recording-on)
  (end-kbd-macro))

(global-set-key '[(f1)] 'call-last-kbd-macro)
(global-set-key '[(shift f1)] 'toggle-kbd-macro-recording-on)
