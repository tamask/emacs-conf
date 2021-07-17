;;
;; functions
;;

;; rename buffers to <filename>:<parent-dir>
(require 'eshell)

(defun rename-buffer-with-directory-postfix ()
  (interactive)
  (let ((dir-name (car (reverse (eshell-split-path 
       (file-name-directory (buffer-file-name)))))))
    (rename-buffer
     (concat
      (file-name-nondirectory
       (buffer-file-name)) ":" dir-name))))

(global-set-key (kbd "C-c r") 'rename-buffer-with-directory-postfix)

;; A function for quickly toggling the display of trailing whitespace.
(defun toggle-trailing-whitespace-display ()
  (interactive)
  (save-excursion
    (if show-trailing-whitespace
        (setq show-trailing-whitespace nil)
      (setq show-trailing-whitespace t))
    (force-window-update (current-buffer)))
  (message (concat
            "Display of EOL spaces "
            (if show-trailing-whitespace "enabled" "disabled"))))

(global-set-key "\C-ce" 'toggle-trailing-whitespace-display)

;; Replace all whitespace in the region with single spaces
(defun collapse-whitespace (beg end)
  "replace all whitespace in the region with single spaces"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (re-search-forward "\\s-+" nil t)
        (replace-match " ")))))

;; visual wrap/unwrap
(defun toggle-visual-wrap ()
  (interactive)
  (if (bound-and-true-p visual-line-mode)
      (progn
        (visual-line-mode -1)
        (visual-fill-column-mode -1))
    (progn
      (visual-line-mode)
      (visual-fill-column-mode))))

(global-set-key "\C-cw" 'toggle-visual-wrap)

;; undo fill paragraph
(defun undo-fill-paragraph ()
  "fill individual paragraphs with large fill column"
  (interactive)
  (let ((fill-column most-positive-fixnum))
    (fill-individual-paragraphs (point-min) (point-max))))

(global-set-key "\M-Q" 'undo-fill-paragraph)

;; magnify text for presentations
(defun magnify ()
  "Large text for presentations"
  (interactive)
  (set-face-attribute 'default (selected-frame) :height my-font-height-magnified))

(defun demagnify ()
  "Large text for presentations"
  (interactive)
  (set-face-attribute 'default (selected-frame) :height my-font-height))

;; sum numbers in region
(require 'cl-lib)
(defun sum-numbers-in-region (start end)
  (interactive "r")
  (message "%s"
           (cl-reduce #'+
                      (split-string (buffer-substring start
                                                      end))
                      :key #'string-to-number)))

(defun increment-number-at-point ()
      (interactive)
      (skip-chars-backward "0-9")
      (or (looking-at "[0-9]+")
          (error "No number at point"))
      (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

(defun decrement-number-at-point ()
      (interactive)
      (skip-chars-backward "0-9")
      (or (looking-at "[0-9]+")
          (error "No number at point"))
      (replace-match (number-to-string (1- (string-to-number (match-string 0))))))

(global-set-key (kbd "M-n") 'increment-number-at-point)
(global-set-key (kbd "M-p") 'decrement-number-at-point)

(global-set-key (kbd "C-<tab>") 'dabbrev-expand)
(define-key minibuffer-local-map (kbd "C-<tab>") 'dabbrev-expand)

(defalias 'q 'kill-emacs)