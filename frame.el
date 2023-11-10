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
