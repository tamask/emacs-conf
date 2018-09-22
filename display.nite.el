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

  ;; (setq my-fgcolor "#fff9bc")
  ;; (setq my-fgcolor "#f0f0dc")
  (setq my-fgcolor "beige")
  (setq my-bgcolor "#222222")

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
   'vertical-border t :foreground "#191919")

  (set-face-attribute
   'fringe t :background my-bgcolor :foreground "#777777")

  (set-face-attribute
   'cursor t :background my-fgcolor :foreground "black")

  (set-face-attribute
   'highlight t :background "aquamarine" :foreground "black")

  (set-face-attribute
   'match t :background "#444444" :foreground "cyan")

  (set-face-attribute
   'shadow t :foreground "grey40")

  (set-face-attribute
   'show-paren-match t :background "aquamarine" :foreground "black")

  (set-face-attribute
   'show-paren-mismatch t :background "red" :foreground "black")

  (set-face-attribute
   'tooltip t :background my-bgcolor :foreground my-fgcolor)

  (set-face-attribute
   'region t :background "#364b4f")

  (set-face-attribute
   'header-line t :background my-bgcolor :foreground "grey")

  (set-face-attribute
   'isearch t :background "palevioletred1" :foreground "brown4")

  (set-face-attribute
   'lazy-highlight t :background "#403a30")

  (set-face-attribute
   'link t :foreground "cyan")

  (set-face-attribute
   'link-visited t :foreground "lightsalmon")

  ;; modeline

  (set-face-attribute
   'mode-line t :box nil :background "#444444" :foreground my-fgcolor)

  (set-face-attribute
   'mode-line-highlight t :box nil :background "#48666f")

  (set-face-attribute
   'mode-line-inactive t :box nil :background "#444444" :foreground "#888888")

  (set-face-attribute
   'mode-line-highlight nil)

  ;; compilation

  (set-face-attribute
   'compilation-mode-line-exit t :foreground "aquamarine" :weight 'normal)

  (set-face-attribute
   'compilation-mode-line-run t :foreground "khaki" :weight 'normal)

  (set-face-attribute
   'compilation-mode-line-fail t :foreground "lightsalmon" :weight 'normal)

  ;; minibuffer

  (set-face-attribute
   'minibuffer-prompt t :foreground "cyan")

  ;; basic text

  (set-face-attribute
   'trailing-whitespace t :background "#675e46")

  (set-face-attribute
   'escape-glyph t :foreground "cyan")

  ;; font-lock

  (set-face-attribute
   'font-lock-type-face t :foreground "aquamarine")

  (set-face-attribute
   'font-lock-comment-face t :foreground "chocolate1")

  (set-face-attribute
   'font-lock-string-face t :foreground "lightsalmon")

  (set-face-attribute
   'font-lock-builtin-face t :foreground "aquamarine" :weight 'normal)

  (set-face-attribute
   'font-lock-constant-face t :foreground "aquamarine" :weight 'normal)

  (set-face-attribute
   'font-lock-function-name-face t :foreground "khaki")

  (set-face-attribute
   'font-lock-keyword-face t :foreground "cyan")

  (set-face-attribute
   'font-lock-variable-name-face t :foreground "aquamarine")

  ;; (set-face-attribute
  ;;  'font-lock-variable-name-face t :foreground nil :inherit nil)

  (set-face-attribute
   'font-lock-preprocessor-face t :foreground "cyan" :inherit nil)

  (set-face-attribute
   'font-lock-warning-face t :foreground "khaki" :weight 'normal)

  ;; ansi term

  (setq ansi-term-color-vector
        [nil "#000000" "#ffa965" "#42ffb6" "#fff485"
             "#93Ceff" "#ffa3CC" "#aeffff"])

  ;; using customize for lazy-loaded modes

  (custom-set-faces
   `(erc-prompt-face
     ((t (:background unspecified :foreground "cyan" :weight normal))))

   `(erc-notice-face
     ((t (:foreground "chocolate1" :weight normal))))

   `(erc-direct-msg-face
     ((t (:foreground "white" :weight normal :slant italic))))

   `(erc-error-face
     ((t (:foreground "#aa0000" :weight normal))))

   `(erc-timestamp-face
     ((t (:foreground "aquamarine" :weight normal))))

   `(erc-nick-msg-face
     ((t (:foreground "chocolate1"))))

   `(erc-my-nick-face
     ((t (:foreground ,my-fgcolor))))

   `(erc-current-nick-face
     ((t (:foreground unspecified))))

   `(erc-input-face
     ((t (:foreground ,my-fgcolor))))

   ;; gnus

   `(gnus-header-name
     ((t (:foreground "aquamarine"))))

   `(gnus-header-content
     ((t (:foreground ,my-fgcolor :slant normal))))

   `(gnus-header-from
     ((t (:foreground ,my-fgcolor))))

   `(gnus-header-subject
     ((t (:foreground ,my-fgcolor))))

   `(gnus-summary-cancelled
     ((t (:background ,my-bgcolor :foreground "#777777"))))

   `(gnus-summary-normal-read
     ((t (:foreground "chocolate1" :weight normal))))

   `(gnus-summary-normal-unread
     ((t (:foreground "aquamarine"))))

   `(gnus-summary-normal-ancient
     ((t (:foreground ,my-fgcolor))))

   `(gnus-summary-normal-ticked
     ((t (:foreground "khaki"))))

   `(gnus-summary-low-read
     ((t (:foreground "chocolate1"))))

   `(gnus-summary-low-unread
     ((t (:foreground "aquamarine"))))

   `(gnus-summary-low-ancient
     ((t (:foreground ,my-fgcolor))))

   `(gnus-summary-low-ticked
     ((t (:foreground "khaki"))))

   `(gnus-summary-high-read
     ((t (:foreground "chocolate1"))))

   `(gnus-summary-high-unread
     ((t (:foreground "aquamarine"))))

   `(gnus-summary-high-ancient
     ((t (:foreground ,my-fgcolor))))

   `(gnus-summary-high-ticked
     ((t (:foreground "khaki"))))

   `(gnus-summary-selected
     ((t (:underline unspecified :foreground "cyan"))))

   ;; mail message

   `(message-header-name
     ((t (:foreground "aquamarine"))))

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
     ((t (:foreground "lightsalmon"))))

   `(message-separator
     ((t (:foreground "#777777"))))

   ;; info reader

   `(info-title-1
     ((t (:weight normal :height 280))))

   `(info-title-2
     ((t (:weight normal :height 240))))

   `(info-title-3
     ((t (:weight normal :height 200))))

   `(info-node
     ((t (:weight normal :slant normal :foreground "aquamarine"))))

   `(info-menu-star
     ((t (:foreground "tomato"))))

   ;; compilation

   `(compilation-info
     ((t (:foreground "khaki" :weight normal ))))

   ;; markdown

   `(markdown-header-face
     ((t (:weight normal :foreground "cyan"))))

   `(markdown-header-face-1
     ((t (:height 260 :foreground "cyan"))))

   `(markdown-header-face-2
     ((t (:height 220 :foreground "cyan"))))

   `(markdown-header-face-3
     ((t (:height 180 :foreground "cyan"))))

   `(markdown-header-face-4
     ((t (:height 140 :foreground "cyan"))))

   `(markdown-header-face-5
     ((t (:underline t :foreground "cyan"))))

   `(markdown-header-face-6
     ((t (:slant italic :foreground "cyan"))))

   `(markdown-bold-face
     ((t (:foreground ,my-fgcolor))))

   `(markdown-italic-face
     ((t (:foreground ,my-fgcolor))))

   `(markdown-inline-code-face
     ((t (:foreground "lightsalmon"))))

   `(markdown-pre-face
     ((t (:foreground "lightsalmon"))))

   `(markdown-url-face
     ((t (:foreground "cyan"))))

   `(markdown-link-face
     ((t (:foreground "aquamarine"))))

   ;; iswitchb

   `(iswitchb-current-match
     ((t (:foreground "khaki"))))

   `(iswitchb-single-match
     ((t (:foreground "aquamarine"))))

   ;; ido

   `(ido-subdir
     ((t (:foreground "khaki"))))

   `(ido-only-match
     ((t (:foreground "aquamarine"))))

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
     ((t (:background unspecified :foreground "chocolate1"))))

   `(diff-index
     ((t (:background unspecified :foreground ,my-fgcolor))))

   `(diff-indicator-added
     ((t (:foreground "yellowgreen"))))

   `(diff-indicator-removed
     ((t (:foreground "chocolate1"))))

   `(diff-indicator-changed
     ((t (:foreground "khaki"))))

   `(diff-removed
     ((t (:foreground "lightsalmon"))))

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

  ;; (setq mode-line-front-space " ")
  ;; (setq-default mode-line-format (cons (propertize "\u200b" 'display '((raise -0.1) (height 1.2))) mode-line-format))
)
