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

(setq-default
 eglot-workspace-configuration
 '(:pylsp (:plugins ( :pycodestyle (:enabled :json-false)
                      :autopep8 (:enabled :json-false)
                      :mccabe (:enabled :json-false)
                      :yapf (:enabled :json-false)))))

;; (add-hook
;;  'eglot-managed-mode-hook
;;  (lambda () (remove-hook 'flymake-diagnostic-functions 'eglot-flymake-backend)))

;; (global-set-key (kbd "C-.") 'eldoc-print-current-symbol-info)

;; magit

(use-package magit :straight t :defer t)

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

;; multiple-cursors

(use-package multiple-cursors :straight t :defer t)

(global-set-key (kbd "C-c n") 'mc/mark-next-like-this)

;; age-mode

(use-package age
  :straight
  (age
   :type git
   :host github
   :repo "anticomputer/age.el")
  :ensure t
  :demand t
  :config
  (age-file-enable))

;; treesitter

;; guide: https://www.masteringemacs.org/article/how-to-get-started-tree-sitterf

(setq treesit-font-lock-level 4)

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(setq major-mode-remap-alist
      '((sh-mode . bash-ts-mode)
        (python-mode . python-ts-mode)
        (javascript-mode . js-ts-mode)
        (js-json-mode . json-ts-mode)))

(defun treesit-fetch-grammars ()
  (interactive)
  (dolist (val treesit-language-source-alist)
    (treesit-install-language-grammar (car val))))

;; treesitter packages

(use-package yaml-ts-mode :defer t :mode "\\.yaml\\'")

;; visual-line-mode that can wrap at a given column, plus a patch for
;; a couple functions to accomodate non-zero left/right-margin-width

(use-package visual-fill-column :straight t)

(defun visual-fill-column--set-margins (window)
  "Set window margins for WINDOW."
  ;; Calculate left & right margins.
  (let* ((total-margin-width (+ left-margin-width right-margin-width))
         (total-width (- (visual-fill-column--window-max-text-width window) total-margin-width))
         (width (or visual-fill-column-width
                    fill-column))
         (margins (if (< (- total-width width) 0) ; margins must be >= 0
                      0
                    (- total-width width)))
         (left (if visual-fill-column-center-text
                   (/ margins 2)
                 left-margin-width))
         (right (- margins left)))

    (if visual-fill-column-extra-text-width
        (let ((add-width (visual-fill-column--add-extra-width left right visual-fill-column-extra-text-width)))
          (setq left (car add-width)
                right (cdr add-width))))

    ;; put an explicitly R2L buffer on the right side of the window
    (when (and (eq bidi-paragraph-direction 'right-to-left)
               (= left 0))
      (setq left right)
      (setq right 0))

    (set-window-margins window left right)))

(defun visual-fill-column-mode--disable ()
  "Disable `visual-fill-column-mode' for the current buffer."
  (remove-hook 'window-configuration-change-hook #'visual-fill-column--adjust-window 'local)

  (let ((window (get-buffer-window (current-buffer))))
    (remove-hook 'window-state-change-functions #'visual-fill-column--adjust-window 'local)
    (set-window-margins window left-margin-width right-margin-width)
    (set-window-parameter window 'min-margins nil)
    (set-window-fringes window nil)))
