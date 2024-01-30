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

;; ses-mode tweaks

(defun ses-calculate-lineno-width ()
  "Calculate the width needed for the line number display."
  (let ((line-count (count-lines (point-min) (point-max))))
    (length (number-to-string line-count))))

(defun ses-overrides ()
  (defun ses-create-header-string ()
    "(Overridden) Set up `ses--header-string' as the buffer's header line.
Based on the current set of columns and `window-hscroll' position."
    (let ((totwidth (- (window-hscroll)))
	      result width x)
      ;; Leave room for the left-side fringe and scrollbar.
      (push (propertize " " 'display '((space :align-to 0))) result)
      (dotimes (col ses--numcols)
        (setq width    (ses-col-width col)
	          totwidth (+ totwidth width 1))
        (if (= totwidth 1)
	        ;; Scrolled so intercolumn space is leftmost.
	        (push " " result))
        (when (> totwidth 1)
	      (if (> ses--header-row 0)
	          (save-excursion
	            (ses-goto-print (1- ses--header-row) col)
	            (setq x (buffer-substring-no-properties (point)
						                                (+ (point) width)))
	            ;; Strip trailing space.
	            (if (string-match "[ \t]+\\'" x)
		            (setq x (substring x 0 (match-beginning 0))))
	            ;; Cut off excess text.
	            (if (>= (length x) totwidth)
		            (setq x (substring x 0 (- totwidth -1)))))
	        (setq x (ses-column-letter col)))
          (setq x (concat (make-string (+ 2 (ses-calculate-lineno-width)) ?\s) x))
	      (push (propertize x 'face ses-box-prop) result)
	      (push (propertize "."
			                'display    `((space :align-to ,(1- totwidth)))
			                'face       ses-box-prop)
	            result)
	      ;; Allow the following space to be squished to make room for the 3-D box
	      ;; Coverage test ignores properties, thinks this is always a space!
	      (push (1value (propertize " " 'display `((space :align-to ,totwidth))))
	            result)))
      (if (> ses--header-row 0)
	      (push (propertize (format "  [row %d]" ses--header-row)
			                'display '((height (- 1))))
	            result))
      (setq ses--header-string (apply #'concat (nreverse result)))))
  )

;; (add-hook 'ses-mode-hook 'display-line-numbers-mode)

(defun my-ses-mode-hook ()
  (setq ses-box-prop '(:box unspecified :inherit 'line-number))
  (display-line-numbers-mode)
  (ses-overrides))

(add-hook 'ses-mode-hook 'my-ses-mode-hook)
