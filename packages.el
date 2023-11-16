;;
;; packages
;;

;; company mode

(use-package company
  :straight t
  :hook (prog-mode . company-mode)
  :bind ("C-;" . company-complete)
  :config
  (setq company-icon-margin 4)
  (setq company-idle-delay nil)
  (setq company-tooltip-width-grow-only t))

;; flymake

(use-package flymake
  :bind ("C-<return>" . flymake-start)
  :config
  (setq flymake-no-changes-timeout nil)
  (setq flymake-start-on-save-buffer nil))

;; eglot

(setq eldoc-echo-area-use-multiline-p nil)

(setq
 eglot-ignored-server-capabilities
 '(:documentOnTypeFormattingProvider
   :inlayHintProvider))

;; (setq
;;  eglot-ignored-server-capabilities
;;  '(:codeActionProvider
;;    :documentFormattingProvider
;;    :documentRangeFormattingProvider
;;    :documentOnTypeFormattingProvider
;;    :colorProvider
;;    :foldingRangeProvider
;;    :executeCommandProvider
;;    :inlayHintProvider))

;; (add-hook
;;  'eglot-managed-mode-hook
;;  (lambda () (remove-hook 'flymake-diagnostic-functions 'eglot-flymake-backend)))

;; magit

(use-package magit :straight t :defer t)

;; for writing text documents without hard wrapping

(use-package visual-fill-column :straight t)

;; markdown

(use-package markdown-mode :straight t :mode "\\.md\\'")

;; sass/scss

(use-package scss-mode
  :straight
  (scss-mode
   :type git
   :host github
   :repo "antonj/scss-mode")
  :mode "\\.scss\\'"
  :config
  (setq scss-compile-at-save nil))

;;
;; deferred packages
;;

;; avoid loading these during initialization, but provide a
;; convenience method/keybinding to load them on demand

(defun use-multiple-cursors ()
  (interactive)
  (use-package multiple-cursors :straight t)
  (message "multiple-cursors loaded"))

(global-set-key (kbd "C-c C-x 1") 'use-multiple-cursors)
