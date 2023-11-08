(defun theme-light ()
  (interactive)

  (let ((my-config ())) (boundp 'my-config))

  (setq theme-font-family (alist-get 'font-family my-config "monospace"))
  (setq theme-font-height (alist-get 'font-height my-config 100))

  (setq theme-foreground "black")
  (setq theme-background "#eeeeee")
  (setq theme-accent "#6865dd")
  (setq theme-second "deep pink")

  ;; basic

  (set-face-attribute
   'default nil
   :family theme-font-family :height theme-font-height
   :background theme-background :foreground theme-foreground)

  (set-face-attribute
   'variable-pitch nil :family theme-font-family)

  (set-face-attribute
   'fixed-pitch nil :family theme-font-family)

  (set-face-attribute
   'border nil :background "grey")

  (set-face-attribute
   'vertical-border nil :foreground "#cccccc")

  (set-face-attribute
   'fringe nil :background theme-background :foreground "#777777")

  (set-face-attribute
   'cursor nil :background theme-foreground :foreground "black")

  (set-face-attribute
   'highlight nil :background theme-second :foreground "white")

  (set-face-attribute
   'match nil :background "#dddddd" :foreground "black")

  (set-face-attribute
   'shadow nil :foreground "grey40")

  (set-face-attribute
   'show-paren-match nil :background theme-second :foreground "white")

  (set-face-attribute
   'show-paren-mismatch nil :background "red" :foreground "white")

  (set-face-attribute
   'tooltip nil :background theme-background :foreground theme-foreground)

  (set-face-attribute
   'region nil :background "#ffffbb")

  (set-face-attribute
   'header-line nil :background theme-background :foreground "grey")

  (set-face-attribute
   'isearch nil :background "palevioletred1" :foreground "brown4")

  (set-face-attribute
   'lazy-highlight nil :background "yellow" :foreground "firebrick")

  (set-face-attribute
   'link nil :foreground theme-accent)

  (set-face-attribute
   'link-visited nil :foreground "#888888")

  ;; dired

  (set-face-attribute
   'dired-header nil :foreground theme-second)

  (set-face-attribute
   'dired-directory nil :foreground theme-accent)

  ;; modeline

  (set-face-attribute
   'mode-line nil :background "white" :foreground theme-accent :box 'unspecified)

  (set-face-attribute
   'mode-line-highlight nil :background theme-accent :foreground "white" :box 'unspecified)

  (set-face-attribute
   'mode-line-inactive nil :background "white" :foreground "honeydew3" :box 'unspecified)

  (set-face-attribute
   'mode-line-highlight nil)

  ;; minibuffer

  (set-face-attribute
   'minibuffer-prompt nil :foreground theme-second)

  ;; basic text

  (set-face-attribute
   'trailing-whitespace nil :background "#675e46")

  (set-face-attribute
   'escape-glyph nil :foreground theme-second)

  ;; font-lock

  (set-face-attribute
   'font-lock-comment-face nil :foreground "#ababab" :slant 'italic)

  (set-face-attribute
   'font-lock-string-face nil :foreground theme-second)

  (set-face-attribute
   'font-lock-builtin-face nil :foreground theme-accent) ;; :foreground theme-foreground :weight 'bold)

  (set-face-attribute
   'font-lock-constant-face nil :foreground theme-accent) ;; :foreground theme-foreground :weight 'bold)

  (set-face-attribute
   'font-lock-function-name-face nil :foreground 'unspecified)

  (set-face-attribute
   'font-lock-keyword-face nil :foreground theme-accent)

  (set-face-attribute
   'font-lock-type-face nil :foreground theme-accent)

  (set-face-attribute
   'font-lock-variable-name-face nil :foreground theme-foreground)

  ;; (set-face-attribute
  ;;  'font-lock-variable-name-face nil :foreground nil :inherit nil)

  (set-face-attribute
   'font-lock-preprocessor-face nil :foreground theme-accent :inherit nil)

  (set-face-attribute
   'font-lock-warning-face nil :foreground "orangered" :weight 'normal)

  ;; ansi term

  (setq ansi-term-color-vector
        [nil "#000000" "#ffa965" "#42ffb6" "#fff485"
             "#93Ceff" "#ffa3CC" "#aeffff"])

  ;; using customize for lazy-loaded modes

  (custom-set-faces
   `(erc-prompt-face
     ((t (:background unspecified :foreground ,theme-second :weight normal))))

   `(erc-notice-face
     ((t (:foreground "orangered" :weight normal))))

   `(erc-direct-msg-face
     ((t (:foreground "white" :weight normal :slant italic))))

   `(erc-error-face
     ((t (:foreground "#aa0000" :weight normal))))

   `(erc-timestamp-face
     ((t (:foreground ,theme-second :weight normal))))

   `(erc-nick-msg-face
     ((t (:foreground "orangered"))))

   `(erc-theme-nick-face
     ((t (:foreground ,theme-foreground))))

   `(erc-current-nick-face
     ((t (:foreground unspecified))))

   `(erc-input-face
     ((t (:foreground "#fff9bc"))))

   ;; gnus

   `(gnus-header-name
     ((t (:foreground ,theme-second))))

   `(gnus-header-content
     ((t (:foreground ,theme-foreground :slant normal))))

   `(gnus-header-from
     ((t (:foreground ,theme-foreground))))

   `(gnus-header-subject
     ((t (:foreground ,theme-foreground))))

   `(gnus-summary-cancelled
     ((t (:background ,theme-background :foreground "#777777"))))

   `(gnus-summary-normal-read
     ((t (:foreground "orangered" :weight normal))))

   `(gnus-summary-normal-unread
     ((t (:foreground ,theme-second))))

   `(gnus-summary-normal-ancient
     ((t (:foreground ,theme-foreground))))

   `(gnus-summary-normal-ticked
     ((t (:foreground "yellow"))))

   `(gnus-summary-low-read
     ((t (:foreground "orangered"))))

   `(gnus-summary-low-unread
     ((t (:foreground ,theme-second))))

   `(gnus-summary-low-ancient
     ((t (:foreground ,theme-foreground))))

   `(gnus-summary-low-ticked
     ((t (:foreground "yellow"))))

   `(gnus-summary-high-read
     ((t (:foreground "orangered"))))

   `(gnus-summary-high-unread
     ((t (:foreground ,theme-second))))

   `(gnus-summary-high-ancient
     ((t (:foreground ,theme-foreground))))

   `(gnus-summary-high-ticked
     ((t (:foreground "yellow"))))

   `(gnus-summary-selected
     ((t (:underline unspecified :foreground ,theme-second))))

   ;; mail message

   `(message-header-name
     ((t (:foreground ,theme-second))))

   `(message-header-to
     ((t (:foreground ,theme-foreground :weight normal))))

   `(message-header-subject
     ((t (:foreground ,theme-foreground))))

   `(message-header-other
     ((t (:foreground ,theme-foreground))))

   `(message-header-xheader
     ((t (:foreground ,theme-foreground))))

   `(message-header-cc
     ((t (:foreground ,theme-foreground))))

   `(message-header-newsgroups
     ((t (:foreground ,theme-foreground))))

   `(message-cited-text
     ((t (:foreground "orangered"))))

   `(message-separator
     ((t (:foreground "#777777"))))

   ;; info reader

   `(info-title-1
     ((t (:weight normal :height 280 :foreground ,theme-second))))

   `(info-title-2
     ((t (:weight normal :height 240 :foreground ,theme-second))))

   `(info-title-3
     ((t (:weight normal :height 200 :foreground ,theme-second))))

   `(info-node
     ((t (:weight normal :slant normal :foreground ,theme-second))))

   `(info-menu-star
     ((t (:foreground ,theme-second))))

   ;; compilation

   `(compilation-info
     ((t (:foreground ,theme-second :weight normal ))))

   ;; markdown

   `(markdown-header-face
     ((t (:weight normal :foreground ,theme-second))))

   `(markdown-header-face-1
     ((t (:height 260 :foreground ,theme-second))))

   `(markdown-header-face-2
     ((t (:height 220 :foreground ,theme-second))))

   `(markdown-header-face-3
     ((t (:height 180 :foreground ,theme-second))))

   `(markdown-header-face-4
     ((t (:height 140 :foreground ,theme-second))))

   `(markdown-header-face-5
     ((t (:underline t :foreground ,theme-second))))

   `(markdown-header-face-6
     ((t (:slant italic :foreground ,theme-second))))

   `(markdown-bold-face
     ((t (:foreground ,theme-foreground))))

   `(markdown-italic-face
     ((t (:foreground ,theme-foreground))))

   `(markdown-inline-code-face
     ((t (:foreground "orangered"))))

   `(markdown-pre-face
     ((t (:foreground "orangered"))))

   `(markdown-url-face
     ((t (:foreground ,theme-second))))

   `(markdown-link-face
     ((t (:foreground ,theme-second))))

   ;; iswitchb

   `(iswitchb-current-match
     ((t (:foreground "darkgoldenrod"))))

   `(iswitchb-single-match
     ((t (:foreground ,theme-second))))

   ;; ido

   `(ido-subdir
     ((t (:foreground "darkgoldenrod"))))

   `(ido-only-match
     ((t (:foreground ,theme-second))))

   `(ido-indicator
     ((t (:foreground "tomato" :background nil))))

   ;; table

   `(table-cell
     ((t (:foreground ,theme-foreground :background "#333333"))))

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
     ((t (:background unspecified :foreground ,theme-foreground))))

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

   ))
