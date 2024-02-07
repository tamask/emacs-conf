;;
;; functions
;;

;; center frame

(defun frame-recenter (&optional frame)
  "Center FRAME on the screen.
FRAME can be a frame name, a terminal name, or a frame.
If FRAME is omitted or nil, use currently selected frame."
  (interactive)
  (unless (eq 'maximised (frame-parameter nil 'fullscreen))
    (modify-frame-parameters
     frame '((user-position . t) (top . 0.5) (left . 0.5)))))

;; deleting without adding to kill-ring

(defun my-delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (delete-region
   (point)
   (progn
     (forward-word arg)
     (point))))

(defun my-backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (my-delete-word (- arg)))

(defun my-delete-line ()
  "Delete text from current position to end of line char.
This command does not push text to `kill-ring'."
  (interactive)
  (delete-region
   (point)
   (progn (end-of-line 1) (point)))
  (delete-char 1))

(defun my-delete-line-backward ()
  "Delete text between the beginning of the line to the cursor position.
This command does not push text to `kill-ring'."
  (interactive)
  (let (p1 p2)
    (setq p1 (point))
    (beginning-of-line 1)
    (setq p2 (point))
    (delete-region p1 p2)))

; bind them to emacs's default shortcut keys:
;(global-set-key (kbd "C-S-k") 'my-delete-line-backward) ; Ctrl+Shift+k
;(global-set-key (kbd "C-k") 'my-delete-line)
;(global-set-key (kbd "M-d") 'my-delete-word)

(global-set-key (kbd "<M-backspace>") 'my-backward-delete-word)
(global-set-key (kbd "<C-backspace>") 'my-backward-delete-word)

;; rename buffers to <filename>:<parent-dir>

(defun rename-buffer-with-directory-postfix ()
  (interactive)
  (let ((dir-name (car (reverse (eshell-split-path
       (file-name-directory (buffer-file-name)))))))
    (rename-buffer
     (concat
      (file-name-nondirectory
       (buffer-file-name)) ":" dir-name))))

(global-set-key (kbd "C-c r") 'rename-buffer-with-directory-postfix)

;; a function for quickly toggling the display of trailing whitespace.

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

;; replace all whitespace in the region with single spaces

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

(global-set-key "\C-cq" 'toggle-truncate-lines)
(global-set-key "\C-cw" 'toggle-visual-wrap)
(global-set-key "\C-cv" 'visual-line-mode)

;; undo fill paragraph

(defun undo-fill-paragraph ()
  "fill individual paragraphs with large fill column"
  (interactive)
  (let ((fill-column most-positive-fixnum))
    (fill-individual-paragraphs (point-min) (point-max))))

(global-set-key "\M-Q" 'undo-fill-paragraph)

;; sum numbers in region

(defun sum-numbers-region (start end)
  (interactive "r")
  (require 'cl-lib)
  (message "%s" (cl-reduce #'+ (split-string (buffer-substring start end)) :key #'string-to-number)))

;; increment/decrement number under point

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

;; swap left and right buffers

(defun win-swap ()
  "Swap windows using buffer-move.el"
  (interactive)
  (use-package buffer-move
    :straight
    (buffer-move
     :type git
     :host github
     :repo "lukhas/buffer-move"))
  (if (null (windmove-find-other-window 'right))
      (buf-move-left) (buf-move-right)))

(global-set-key (kbd "\C-c TAB") 'win-swap)

;; evaluate ralgebraic expression in the region & replace with result

(defun calc-eval-region-or-line ()
  "Evaluate algebraic expression in the region and replace with
 the result, or evaluate the line and insert the result on a new
line"
  (interactive)
  (if (region-active-p)
      (let* ((expr (buffer-substring-no-properties (region-beginning) (region-end)))
             (result (calc-eval expr)))
        (delete-region (region-beginning) (region-end))
        (insert result))
    (let* ((expr (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
           (result (calc-eval expr)))
      (newline-and-indent)
      (insert result))))

(global-set-key (kbd "\C-c RET") 'calc-eval-region-or-line)
(global-set-key '[(f12)] 'calc-eval-region-or-line) ;; in case there's a mode kbd override for above

;; flymake

(defun flymake-show-and-resize-diagnostics ()
  (interactive)
  ;; Call flymake-show-buffer-diagnostics
  (flymake-show-project-diagnostics)
  ;; Iterate over all buffers to find the Flymake diagnostics buffer
  (let ((buffers (buffer-list))
        diag-buffer)
    (while (and buffers (not diag-buffer))
      (let ((buffer (car buffers)))
        (when (and (buffer-live-p buffer)
                   (string-match-p "^\\*Flymake diagnostics for " (buffer-name buffer)))
          (setq diag-buffer buffer)))
      (setq buffers (cdr buffers)))
    (when diag-buffer
      ;; Switch to the diagnostics buffer
      (switch-to-buffer-other-window diag-buffer)
      ;; Resize window to fit the buffer content
      (fit-window-to-buffer (get-buffer-window diag-buffer) nil 1))))

(global-set-key (kbd "\C-c f") 'flymake-show-and-resize-diagnostics)
