(defun theme-light ()
  (interactive)

  (let ((my-config ())) (boundp 'my-config))

  (setq theme-font-family (plist-get-or-default my-config :font-family "monospace"))
  (setq theme-font-height (plist-get-or-default my-config :font-height 100))

  (setq theme-foreground "black")
  (setq theme-background "#eeeeee")

  (setq theme-accent "#6865dd")
  (setq theme-second "deep pink")
  (setq theme-third "#009062")

  (setq theme-highlight "#ffffbb")
  (setq theme-error "deep pink")
  (setq theme-warning "#fc8749")

  ;; ansi term

  (setq ansi-term-color-vector
        [nil "#000000" "#ffa965" "#42ffb6" "#fff485"
             "#93Ceff" "#ffa3CC" "#aeffff"])

  ;; using customize for lazy-loaded modes

  (custom-set-faces

   ;; basic

   `(default
     ((t ( :family ,theme-font-family
           :height ,theme-font-height
           :foreground ,theme-foreground
           :background ,theme-background))))

   `(error
     ((t (:foreground ,theme-error :weight normal))))

   `(warning
     ((t (:foreground ,theme-warning :weight normal))))

   `(variable-pitch
     ((t (:family ,theme-font-family))))

   `(fixed-pitch
     ((t (:family ,theme-font-family))))

   `(border
     ((t (:background "grey"))))

   `(vertical-border
     ((t (:foreground "#cccccc"))))

   `(fringe
     ((t (:background ,theme-background :foreground "#777777"))))

   `(cursor
     ((t (:background ,theme-foreground :foreground "black"))))

   `(highlight
     ((t (:background ,theme-accent :foreground "white"))))

   `(match
     ((t (:background "#dddddd" :foreground "black"))))

   `(shadow
     ((t (:foreground "grey40"))))

   `(show-paren-match
     ((t (:background ,theme-second :foreground "white"))))

   `(show-paren-mismatch
     ((t (:background "red" :foreground "white"))))

   `(tooltip
     ((t (:background ,theme-background :foreground ,theme-foreground))))

   `(region
     ((t (:background "#ffffbb"))))

   `(header-line
     ((t (:background ,theme-background :foreground "grey"))))

   `(isearch
     ((t (:background "palevioletred1" :foreground "brown4"))))

   `(lazy-highlight
     ((t (:background "yellow" :foreground "firebrick"))))

   `(link
     ((t (:foreground ,theme-accent))))

   `(link-visited
     ((t (:foreground "#888888"))))

   ;; dired

   `(dired-header
     ((t (:foreground ,theme-second))))

   `(dired-directory
     ((t (:foreground ,theme-accent))))

   ;; modeline

   `(mode-line
     ((t (:background "white" :foreground ,theme-accent :box unspecified))))

   `(mode-line-highlight
     ((t (:background ,theme-accent :foreground "white" :box unspecified))))

   `(mode-line-inactive
     ((t (:background "white" :foreground "honeydew3" :box unspecified))))

   `(mode-line-highlight
     ((t ())))

   ;; minibuffer

   `(minibuffer-prompt
     ((t (:foreground ,theme-second))))

   ;; basic text

   `(trailing-whitespace
     ((t (:background "#675e46"))))

   `(escape-glyph
     ((t (:foreground ,theme-second))))

   ;; font-lock

   `(font-lock-comment-face
     ((t (:foreground "#ababab" :slant italic))))

   `(font-lock-string-face
     ((t (:foreground ,theme-second))))

   `(font-lock-builtin-face
     ((t (:foreground ,theme-accent))))

   `(font-lock-constant-face
     ((t (:foreground ,theme-accent))))

   `(font-lock-function-name-face
     ((t (:foreground unspecified))))

   `(font-lock-keyword-face
     ((t (:foreground ,theme-accent))))

   `(font-lock-type-face
     ((t (:foreground ,theme-accent))))

   `(font-lock-variable-name-face
     ((t (:foreground ,theme-foreground))))

   ;; `(font-lock-variable-name-face
   ;;   ((t (:foreground nil :inherit nil))))

   `(font-lock-preprocessor-face
     ((t (:foreground ,theme-accent :inherit nil))))

   `(font-lock-warning-face
     ((t (:foreground "orangered" :weight normal))))

   ;; company

   `(company-tooltip
     ((t (:background "white"))))

   `(company-tooltip-mouse
     ((t (:background unspecified :foreground ,theme-accent))))

   `(company-tooltip-selection
     ((t (:background "aquamarine2"))))

   `(company-tooltip-scrollbar-track
     ((t (:background "white"))))

   `(company-tooltip-scrollbar-thumb
     ((t (:background "#f4f4f4"))))

   ;; info reader

   `(info-title-1
     ((t (:weight bold :height unspecified))))

   `(info-title-2
     ((t (:weight bold :height unspecified))))

   `(info-title-3
     ((t (:weight bold :height unspecified))))

   `(info-node
     ((t (:weight normal :slant normal :foreground ,theme-accent))))

   `(info-menu-header
     ((t (:inherit unspecified :weight bold))))

   `(Info-quoted ; not a typo
     ((t (:family ,theme-font-family :foreground "#888"))))

   `(info-menu-star
     ((t (:foreground ,theme-second))))

   ;; compilation

   `(compilation-info
     ((t (:foreground ,theme-second :weight normal ))))

   ;; markdown

   `(markdown-header-face
     ((t (:height unspecified :weight normal :foreground ,theme-accent))))

   `(markdown-header-face-1
     ((t (:height unspecified :foreground ,theme-accent))))

   `(markdown-header-face-2
     ((t (:height unspecified :foreground ,theme-accent))))

   `(markdown-header-face-3
     ((t (:height unspecified :foreground ,theme-accent))))

   `(markdown-header-face-4
     ((t (:height unspecified :foreground ,theme-accent))))

   `(markdown-header-face-5
     ((t (:height unspecified :foreground ,theme-accent))))

   `(markdown-header-face-6
     ((t (:height unspecified :foreground ,theme-accent))))

   `(markdown-bold-face
     ((t (:foreground ,theme-foreground :weight bold))))

   `(markdown-italic-face
     ((t (:foreground ,theme-foreground :slant italic))))

   `(markdown-inline-code-face
     ((t (:foreground "#33967a"))))

   `(markdown-pre-face
     ((t (:foreground "#33967a"))))

   `(markdown-url-face
     ((t (:foreground ,theme-accent))))

   `(markdown-link-face
     ((t (:foreground ,theme-accent))))

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

   ;; customize

   `(custom-button
     ((t (:box nil :background "#fff" :foreground ,theme-foreground))))

   `(custom-button-mouse
     ((t (:box nil :background ,theme-accent :foreground "white"))))

   `(custom-button-pressed
     ((t (:box nil :background ,theme-second :foreground "white"))))

   `(custom-group-tag
     ((t (:height unspecified))))

   `(custom-group-tag-1
     ((t (:height unspecified))))

   `(custom-visibility
     ((t (:inherit 'link :height unspecified :underline t))))

   `(custom-variable-tag
     ((t (:inherit 'link))))

   ;; widget

   `(widget-field
     ((t (:box nil :background "#ddd"))))

   `(widget-single-line-field
     ((t (:box nil :background "#ddd"))))

   ;; flymake

   `(flymake-note
     ((t (:foreground ,theme-third :background "#D2ECE4" :underline unspecified))))

   `(flymake-error
     ((t (:foreground ,theme-error :background "#FFE0DF" :underline unspecified))))

   `(flymake-warning
     ((t (:foreground ,theme-warning :background "#ffffbb" :underline unspecified))))

   ))