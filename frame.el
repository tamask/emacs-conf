;;
;; frame
;;

(let ((my-config ())) (boundp 'my-config))

;; no scrollbars

(set-scroll-bar-mode nil)

;; hide toolbar

(tool-bar-mode -1)

;; hide menubar

(menu-bar-mode -1)

;; disable fringe and use margins instead

(set-fringe-style 0)

(when window-system
  (setq-default left-margin-width 1 right-margin-width 1)
  (set-window-buffer nil (current-buffer))
  (dolist (window (window-list))
    (set-window-margins window left-margin-width right-margin-width))
  (set-window-margins (minibuffer-window) left-margin-width right-margin-width))

;; set frame geometry

(set-frame-size
 (selected-frame)
 (plist-get-or-default my-config :frame-width 140)
 (plist-get-or-default my-config :frame-height 35))

;; set global font settings that affect frame geometry

(set-face-attribute
 'default nil
 :family (plist-get-or-default my-config :font-family "monospace")
 :height (plist-get-or-default my-config :font-height 100))

;; center initial frame on current display now that the font is set

(unless (eq 'maximised (frame-parameter nil 'fullscreen))
  (modify-frame-parameters
   (selected-frame) '((user-position . t) (top . 0.5) (left . 0.5))))
