(defun theme-night ()
  (interactive)

  (let ((my-config ())) (boundp 'my-config))

  (setq theme-font-family (plist-get-or-default my-config :font-family "monospace"))
  (setq theme-font-height (plist-get-or-default my-config :font-height 100))

  (setq theme-foreground "#dddddd")
  (setq theme-background "#202020")

  (setq theme-accent "#b0bcfc")
  (setq theme-second "#ff9595")
  (setq theme-third "#57d88f")
  (setq theme-dim "#aaa")

  (setq theme-highlight "#364b4f")
  (setq theme-error "#ff446b")
  (setq theme-warning "#ffe386")

  (setq theme-red "#ff9595")
  (setq theme-green "#1ed897")
  (setq theme-blue "#3d99db")
  (setq theme-orange "#d69500")

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

   `(fixed-pitch-serif
     ((t (:family ,theme-font-family))))

   `(border
     ((t (:background "grey"))))

   `(vertical-border
     ((t (:foreground "#191919"))))

   `(fringe
     ((t (:background ,theme-background :foreground "#777777"))))

   `(cursor
     ((t (:background ,theme-foreground :foreground "black"))))

   `(highlight
     ((t (:background ,theme-accent :foreground "black"))))

   `(match
     ((t (:background ,theme-highlight :foreground ,theme-accent))))

   `(lazy-highlight
     ((t (:background "skyblue4"))))

   `(shadow
     ((t (:foreground "grey40"))))

   `(show-paren-match
     ((t (:background ,theme-second :foreground "brown4"))))

   `(show-paren-mismatch
     ((t (:background "dimgrey" :foreground ,theme-background))))

   `(tooltip
     ((t (:background ,theme-background :foreground ,theme-foreground))))

   `(region
     ((t (:background ,theme-highlight))))

   `(header-line
     ((t (:background ,theme-background :foreground "grey"))))

   `(isearch
     ((t (:background "palevioletred1" :foreground "brown4"))))

   `(link
     ((t (:foreground ,theme-accent))))

   `(link-visited
     ((t (:foreground "#7381ce"))))

   `(success
     ((t (:foreground "palegreen"))))

   ;; mode-line

   `(mode-line
     ((t (:box nil :background "grey20" :foreground ,theme-foreground))))

   `(mode-line-highlight
     ((t (:box nil :background "grey25"))))

   `(mode-line-inactive
     ((t (:box nil :background "grey20" :foreground "dimgrey"))))

   ;; compilation

   `(compilation-mode-line-exit
     ((t (:foreground ,theme-accent :weight normal))))

   `(compilation-mode-line-run
     ((t (:foreground "khaki" :weight normal))))

   `(compilation-mode-line-fail
     ((t (:foreground "wheat" :weight normal))))

   `(compilation-warning
     ((t (:foreground "khaki" :weight normal))))

   `(compilation-info
     ((t (:foreground "khaki" :weight normal))))

   `(compilation-info
     ((t (:foreground "khaki" :weight normal))))

   ;; dired

   `(dired-header
     ((t (:foreground ,theme-accent))))

   `(dired-directory
     ((t (:foreground ,theme-accent))))

   `(dired-flagged
     ((t (:foreground "#ff9595" :weight normal))))

   ;; minibuffer

   `(minibuffer-prompt
     ((t (:foreground ,theme-accent))))

   ;; basic text

   `(trailing-whitespace
     ((t (:background "palevioletred1"))))

   `(escape-glyph
     ((t (:foreground ,theme-accent))))

   `(glyphless-char
     ((t (:height ,theme-font-height))))

   ;; font-lock

   `(font-lock-type-face
     ((t (:foreground ,theme-accent :weight normal))))

   `(font-lock-comment-face
     ((t (:foreground "dimgrey" :slant italic))))

   `(font-lock-string-face
     ((t (:foreground ,theme-second))))

   `(font-lock-builtin-face
     ((t (:foreground ,theme-accent :weight normal))))

   `(font-lock-constant-face
     ((t (:foreground unspecified :weight normal))))

   `(font-lock-function-name-face
     ((t (:foreground "#fff" :weight normal))))

   `(font-lock-function-call-face
     ((t (:foreground ,theme-foreground :weight normal))))

   `(font-lock-keyword-face
     ((t (:foreground ,theme-accent))))

   `(font-lock-variable-name-face
     ((t (:foreground ,theme-foreground))))

   `(font-lock-preprocessor-face
     ((t (:foreground ,theme-accent :inherit nil))))

   `(font-lock-warning-face
     ((t (:foreground "khaki" :weight normal))))

   `(font-lock-delimiter-face
     ((t (:foreground ,theme-dim))))

   `(font-lock-bracket-face
     ((t (:foreground ,theme-dim))))

   `(font-lock-operator-face
     ((t (:foreground ,theme-dim))))

   `(font-lock-doc-face
     ((t (:foreground "cadetblue4"))))

   ;; company

   `(company-tooltip
     ((t (:background "#151515" :foreground "#999"))))

   `(company-tooltip-mouse
     ((t (:background unspecified :foreground "#fff"))))

   `(company-tooltip-selection
     ((t (:background "#48559F" :foreground "#eee"))))

   `(company-tooltip-scrollbar-track
     ((t (:background "#151515"))))

   `(company-tooltip-scrollbar-thumb
     ((t (:background "#1c1c1c"))))

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
     ((t (:family ,theme-font-family :foreground "#e8a0a"))))

   `(info-menu-star
     ((t (:foreground unspecified))))

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
     ((t (:foreground "#fcd7b3"))))

   `(markdown-pre-face
     ((t (:foreground "#fcd7b3"))))

   `(markdown-url-face
     ((t (:foreground ,theme-accent))))

   `(markdown-link-face
     ((t (:foreground ,theme-accent))))

   ;; iswitchb

   `(iswitchb-current-match
     ((t (:foreground "khaki"))))

   `(iswitchb-single-match
     ((t (:foreground ,theme-accent))))

   ;; ido

   `(ido-subdir
     ((t (:foreground "khaki"))))

   `(ido-only-match
     ((t (:foreground ,theme-accent))))

   `(ido-indicator
     ((t (:foreground "tomato" :background nil))))

   ;; diff mode

   `(diff-file-header
     ((t (:background unspecified :foreground unspecified :weight bold))))

   `(diff-function
     ((t (:background unspecified :foreground unspecified :weight bold))))

   `(diff-header
     ((t (:background unspecified :foreground unspecified :weight bold))))

   `(diff-hunk-header
     ((t (:background unspecified :foreground "dimgrey"))))

   `(diff-index
     ((t (:background unspecified :foreground ,theme-foreground))))

   `(diff-indicator-added
     ((t (:foreground "yellowgreen"))))

   `(diff-indicator-removed
     ((t (:foreground "dimgrey"))))

   `(diff-indicator-changed
     ((t (:foreground "khaki"))))

   `(diff-removed
     ((t (:foreground "dimgrey"))))

   `(diff-refine-change
     ((t (:background unspecified))))

   ;; customize

   `(custom-button
     ((t (:box nil :background "#333" :foreground "beige"))))

   `(custom-button-mouse
     ((t (:box nil :background ,theme-accent :foreground "white"))))

   `(custom-button-pressed
     ((t (:box nil :background ,theme-accent :foreground "white"))))

   `(custom-group-tag
     ((t (:height unspecified))))

   `(custom-group-tag-1
     ((t (:height unspecified))))

   `(custom-visibility
     ((t (:height unspecified :underline t))))

   ;; widget

   `(widget-field
     ((t (:box nil :background "#151515"))))

   `(widget-single-line-field
     ((t (:box nil :background "#151515"))))

   ;; bookmark

   `(bookmark-face
     ((t (:background unspecified :foreground "dimgrey"))))

   ;; flymake

   `(flymake-note
     ((t (:foreground "#57d88f" :background unspecified :underline unspecified))))

   `(flymake-error
     ((t (:foreground ,theme-error :background unspecified :underline unspecified))))

   `(flymake-warning
     ((t (:foreground ,theme-warning :background unspecified :underline unspecified))))

   ;; help

   `(help-for-help-header
     ((t (:font-height ,theme-font-height))))

   `(help-key-binding
     ((t (:box unspecified :foreground "lightskyblue1"))))

   ;; magit

   `(magit-hash
     ((t (:foreground "#888"))))

   `(magit-branch-local
     ((t (:foreground ,theme-green))))

   `(magit-branch-current
     ((t (:inherit 'magit-branch-local :box unspecified :weight bold))))

   `(magit-branch-remote
     ((t (:foreground ,theme-blue))))

   `(magit-branch-remote-head
     ((t (:inherit 'magit-branch-remote :box unspecified :weight bold))))

   `(magit-section-heading
     ((t (:foreground unspecified :weight bold))))

   `(magit-section-highlight
     ((t (:background unspecified))))

   `(magit-diff-file-heading
     ((t (:background unspecified :foreground "white" :weight normal))))

   `(magit-diff-file-heading-highlight
     ((t (:background unspecified :inherit 'magit-diff-file-heading))))

   `(magit-diff-hunk-heading
     ((t (:background unspecified :foreground ,theme-blue))))

   `(magit-diff-hunk-heading-highlight
     ((t (:background unspecified :inherit 'magit-diff-hunk-heading))))

   `(magit-diff-context-highlight
     ((t (:background unspecified :inherit 'magit-diff-context))))

   `(magit-diff-added
     ((t (:background unspecified :foreground ,theme-green))))

   `(magit-diff-added-highlight
     ((t (:background unspecified :inherit 'magit-diff-added))))

   `(magit-diff-removed
     ((t (:background unspecified :foreground ,theme-red))))

   `(magit-diff-removed-highlight
     ((t (:background unspecified :inherit 'magit-diff-removed))))

   `(magit-diff-base
     ((t (:background unspecified :foreground ,theme-orange))))

   `(magit-diff-base-highlight
     ((t (:background unspecified :inherit 'magit-diff-base))))

   `(magit-diff-our-highlight
     ((t (:background unspecified :inherit 'magit-diff-our))))

   `(magit-diff-their-highlight
     ((t (:background unspecified :inherit 'magit-diff-their))))

   `(magit-diff-revision-summary
     ((t (:foreground unspecified :weight bold))))

   `(magit-diff-revision-summary-highlight
     ((t (:inherit 'magit-diff-revision-summary))))

   ))