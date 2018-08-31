;;
;; display
;;

(when window-system
  ;; variables
  (let ((my-font-family "monospace")) (boundp 'my-font))
  (let ((my-font-height 100)) (boundp 'my-font-height))
  (let ((my-font-height 100)) (boundp 'my-font-height))
  (let ((my-frame-top 30)) (boundp 'my-frame-top))
  (let ((my-frame-left 200)) (boundp 'my-frame-left))
  (let ((my-frame-width 100)) (boundp 'my-frame-width))
  (let ((my-frame-height 40)) (boundp 'my-frame-height))

  ;; default frame attributes and initial position
  (setq my-frame-alist `
        ((width . ,my-frame-width)
         (height . ,my-frame-height)))

  ;; You can specify geometry-related options for the initial frame,
  ;; however they won't take effect until Emacs reads `.emacs', which
  ;; happens after first creating the frame. Therefore, set the selected
  ;; frame's position instead.
  (setq default-frame-alist (append my-frame-alist default-frame-alist))
  (set-frame-position (selected-frame) my-frame-left my-frame-top)
  (set-frame-size (selected-frame) my-frame-width my-frame-height)

  ;; no scrollbars
  (set-scroll-bar-mode nil)

  ;; hide toolbar
  (tool-bar-mode -1)

  ;; hide menubar
  (menu-bar-mode -1)

  ;; colors

  (setq my-fgcolor "black")
  (setq my-bgcolor "white")

  ;; basic

  (set-face-attribute
   'default t
   :family my-font-family :height my-font-height
   :background my-bgcolor :foreground my-fgcolor)

  (set-face-attribute
   'variable-pitch t :family my-font-family)

  (set-face-attribute
   'fixed-pitch t :family my-font-family)

  (set-face-attribute
   'border t :background "grey")

  (set-face-attribute
   'vertical-border t :foreground "#cccccc")

  (set-face-attribute
   'fringe t :background my-bgcolor :foreground "#777777")

  (set-face-attribute
   'cursor t :background my-fgcolor :foreground "black")

  (set-face-attribute
   'highlight t :background "aquamarine4" :foreground "white")

  (set-face-attribute
   'match t :background "#dddddd" :foreground "black")

  (set-face-attribute
   'shadow t :foreground "grey40")

  (set-face-attribute
   'show-paren-match t :background "aquamarine4" :foreground "white")

  (set-face-attribute
   'show-paren-mismatch t :background "red" :foreground "white")

  (set-face-attribute
   'tooltip t :background my-bgcolor :foreground my-fgcolor)

  (set-face-attribute
   'region t :background "#dddddd")

  (set-face-attribute
   'header-line t :background my-bgcolor :foreground "grey")

  (set-face-attribute
   'isearch t :background "palevioletred1" :foreground "brown4")

  (set-face-attribute
   'lazy-highlight t :background "aquamarine4" :foreground "white")

  (set-face-attribute
   'link t :foreground "aquamarine4")

  (set-face-attribute
   'link-visited t :foreground "dimgrey")

  ;; modeline

  (set-face-attribute
   'mode-line t :background "aquamarine4" :foreground my-bgcolor
   :box '(:line-width 1 :color "aquamarine4"))

  (set-face-attribute
   'mode-line-highlight nil)

  (set-face-attribute
   'mode-line-inactive t :background "#dddddd" :foreground "#333333"
   :box '(:line-width 1 :color "#dddddd"))

  (set-face-attribute
   'mode-line-highlight nil)

  ;; minibuffer

  (set-face-attribute
   'minibuffer-prompt t :foreground "aquamarine4")

  ;; basic text

  (set-face-attribute
   'trailing-whitespace t :background "#675e46")

  (set-face-attribute
   'escape-glyph t :foreground "aquamarine4")

  ;; font-lock

  (set-face-attribute
   'font-lock-comment-face t :foreground "dimgray")

  (set-face-attribute
   'font-lock-string-face t :foreground "dimgray")

  (set-face-attribute
   'font-lock-builtin-face t :foreground "aquamarine4") ;; :foreground my-fgcolor :weight 'bold)

  (set-face-attribute
   'font-lock-constant-face t :foreground "aquamarine4") ;; :foreground my-fgcolor :weight 'bold)

  (set-face-attribute
   'font-lock-function-name-face t :foreground "aquamarine4" :weight 'bold)

  (set-face-attribute
   'font-lock-keyword-face t :foreground "aquamarine4")

  (set-face-attribute
   'font-lock-type-face t :foreground "aquamarine4" :weight 'bold)

  (set-face-attribute
   'font-lock-variable-name-face t :foreground my-fgcolor)

  ;; (set-face-attribute
  ;;  'font-lock-variable-name-face t :foreground nil :inherit nil)

  (set-face-attribute
   'font-lock-preprocessor-face t :foreground "aquamarine4" :inherit nil)

  (set-face-attribute
   'font-lock-warning-face t :foreground "orangered" :weight 'normal)

  ;; ansi term

  (setq ansi-term-color-vector
        [nil "#000000" "#ffa965" "#42ffb6" "#fff485"
             "#93Ceff" "#ffa3CC" "#aeffff"])

  ;; using customize for lazy-loaded modes

  (custom-set-faces
   `(erc-prompt-face
     ((t (:background unspecified :foreground "aquamarine4" :weight normal))))

   `(erc-notice-face
     ((t (:foreground "orangered" :weight normal))))

   `(erc-direct-msg-face
     ((t (:foreground "white" :weight normal :slant italic))))

   `(erc-error-face
     ((t (:foreground "#aa0000" :weight normal))))

   `(erc-timestamp-face
     ((t (:foreground "aquamarine4" :weight normal))))

   `(erc-nick-msg-face
     ((t (:foreground "orangered"))))

   `(erc-my-nick-face
     ((t (:foreground ,my-fgcolor))))

   `(erc-current-nick-face
     ((t (:foreground unspecified))))

   `(erc-input-face
     ((t (:foreground "#fff9bc"))))

   ;; gnus

   `(gnus-header-name
     ((t (:foreground "aquamarine4"))))

   `(gnus-header-content
     ((t (:foreground ,my-fgcolor :slant normal))))

   `(gnus-header-from
     ((t (:foreground ,my-fgcolor))))

   `(gnus-header-subject
     ((t (:foreground ,my-fgcolor))))

   `(gnus-summary-cancelled
     ((t (:background ,my-bgcolor :foreground "#777777"))))

   `(gnus-summary-normal-read
     ((t (:foreground "orangered" :weight normal))))

   `(gnus-summary-normal-unread
     ((t (:foreground "aquamarine4"))))

   `(gnus-summary-normal-ancient
     ((t (:foreground ,my-fgcolor))))

   `(gnus-summary-normal-ticked
     ((t (:foreground "yellow"))))

   `(gnus-summary-low-read
     ((t (:foreground "orangered"))))

   `(gnus-summary-low-unread
     ((t (:foreground "aquamarine4"))))

   `(gnus-summary-low-ancient
     ((t (:foreground ,my-fgcolor))))

   `(gnus-summary-low-ticked
     ((t (:foreground "yellow"))))

   `(gnus-summary-high-read
     ((t (:foreground "orangered"))))

   `(gnus-summary-high-unread
     ((t (:foreground "aquamarine4"))))

   `(gnus-summary-high-ancient
     ((t (:foreground ,my-fgcolor))))

   `(gnus-summary-high-ticked
     ((t (:foreground "yellow"))))

   `(gnus-summary-selected
     ((t (:underline unspecified :foreground "aquamarine4"))))

   ;; mail message

   `(message-header-name
     ((t (:foreground "aquamarine4"))))

   `(message-header-to
     ((t (:foreground ,my-fgcolor :weight normal))))

   `(message-header-subject
     ((t (:foreground ,my-fgcolor))))

   `(message-header-other
     ((t (:foreground ,my-fgcolor))))

   `(message-header-xheader
     ((t (:foreground ,my-fgcolor))))

   `(message-header-cc
     ((t (:foreground ,my-fgcolor))))

   `(message-header-newsgroups
     ((t (:foreground ,my-fgcolor))))

   `(message-cited-text
     ((t (:foreground "orangered"))))

   `(message-separator
     ((t (:foreground "#777777"))))

   ;; info reader

   `(info-title-1
     ((t (:weight normal :height 280 :foreground "aquamarine4"))))

   `(info-title-2
     ((t (:weight normal :height 240 :foreground "aquamarine4"))))

   `(info-title-3
     ((t (:weight normal :height 200 :foreground "aquamarine4"))))

   `(info-node
     ((t (:weight normal :slant normal :foreground "aquamarine4"))))

   `(info-menu-star
     ((t (:foreground "aquamarine4"))))

   ;; compilation

   `(compilation-info
     ((t (:foreground "aquamarine4" :weight normal ))))

   ;; markdown

   `(markdown-header-face
     ((t (:weight normal :foreground "aquamarine4"))))

   `(markdown-header-face-1
     ((t (:height 260 :foreground "aquamarine4"))))

   `(markdown-header-face-2
     ((t (:height 220 :foreground "aquamarine4"))))

   `(markdown-header-face-3
     ((t (:height 180 :foreground "aquamarine4"))))

   `(markdown-header-face-4
     ((t (:height 140 :foreground "aquamarine4"))))

   `(markdown-header-face-5
     ((t (:underline t :foreground "aquamarine4"))))

   `(markdown-header-face-6
     ((t (:slant italic :foreground "aquamarine4"))))

   `(markdown-bold-face
     ((t (:foreground ,my-fgcolor))))

   `(markdown-italic-face
     ((t (:foreground ,my-fgcolor))))

   `(markdown-inline-code-face
     ((t (:foreground "orangered"))))

   `(markdown-pre-face
     ((t (:foreground "orangered"))))

   `(markdown-url-face
     ((t (:foreground "aquamarine4"))))

   `(markdown-link-face
     ((t (:foreground "aquamarine4"))))

   ;; iswitchb

   `(iswitchb-current-match
     ((t (:foreground "darkgoldenrod"))))

   `(iswitchb-single-match
     ((t (:foreground "aquamarine4"))))

   ;; ido

   `(ido-subdir
     ((t (:foreground "darkgoldenrod"))))

   `(ido-only-match
     ((t (:foreground "aquamarine4"))))

   `(ido-indicator
     ((t (:foreground "tomato" :background nil))))

   ;; table

   `(table-cell
     ((t (:foreground ,my-fgcolor :background "#333333"))))

   ;; django html mode

   `(django-html-font-lock-keywords
     ((t (:foreground "tomato"))))

   ;; diff mode

   `(diff-file-header
     ((t (:background unspecified :foreground unspecified :weight bold))))

   `(diff-function
     ((t (:background unspecified :foreground unspecified :weight bold))))

   `(diff-header
     ((t (:background unspecified :foreground unspecified :weight bold))))

   `(diff-hunk-header
     ((t (:background unspecified :foreground "orangered"))))

   `(diff-index
     ((t (:background unspecified :foreground ,my-fgcolor))))

   `(diff-indicator-added
     ((t (:foreground "yellowgreen"))))

   `(diff-indicator-removed
     ((t (:foreground "orangered"))))

   `(diff-indicator-changed
     ((t (:foreground "darkgoldenrod"))))

   `(diff-removed
     ((t (:foreground "orangered"))))

   `(diff-refine-change
     ((t (:background unspecified))))

   ;; rst

   `(rst-level-1-face
     ((t (:background unspecified))))

   `(rst-level-2-face
     ((t (:background unspecified))))

   `(rst-level-3-face
     ((t (:background unspecified))))

   `(rst-level-4-face
     ((t (:background unspecified))))

   `(rst-level-5-face
     ((t (:background unspecified))))

   `(rst-level-6-face
     ((t (:background unspecified))))

   )
)
