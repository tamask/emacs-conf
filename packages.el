;;
;; packages
;;

(setq
 package-list
 '(
   load-relative
   use-package
   company
   visual-fill-column
   ))

;; install the missing packages

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; use-package

(require 'use-package)

;; company mode

(use-package company
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

;; for writing text documents without hard wrapping

(use-package visual-fill-column)

;; markdown

(use-package markdown-mode :mode "\\.md\\'")

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
