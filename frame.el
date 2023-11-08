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
 (alist-get 'frame-width my-config 140)
 (alist-get 'frame-height my-config 35))

;; set global font settings that affect frame geometry

(set-face-attribute
 'default nil
 :family (alist-get 'font-family my-config "monospace")
 :height (alist-get 'font-height my-config 100))

;; center initial frame on current display now that the font is set

(unless (eq 'maximised (frame-parameter nil 'fullscreen))
  (modify-frame-parameters
   (selected-frame) '((user-position . t) (top . 0.5) (left . 0.5))))
