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
