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
  (setq theme-dim "#666")

  (setq theme-highlight "#ffffbb")
  (setq theme-info "#d65c7e")
  (setq theme-error "deep pink")
  (setq theme-warning "#fc8749")

  (setq theme-red "#e5425d")
  (setq theme-green "#00ad70")
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
     ((t (:background ,theme-background :foreground "#555"))))

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

   `(dired-symlink
     ((t (:foreground ,theme-green))))

   ;; modeline

   `(mode-line
     ((t (:background "white" :foreground ,theme-accent :box unspecified))))

   `(mode-line-highlight
     ((t (:background ,theme-accent :foreground "white" :box unspecified))))

   `(mode-line-inactive
     ((t (:background "white" :foreground "#888" :box unspecified))))

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
     ((t (:foreground unspecified :weight normal))))

   `(font-lock-function-name-face
     ((t (:foreground unspecified :weight normal))))

   `(font-lock-function-call-face
     ((t (:foreground unspecified :weight normal))))

   `(font-lock-keyword-face
     ((t (:foreground ,theme-accent))))

   `(font-lock-type-face
     ((t (:foreground ,theme-accent))))

   `(font-lock-variable-name-face
     ((t (:foreground ,theme-foreground))))

   `(font-lock-preprocessor-face
     ((t (:foreground ,theme-accent :inherit nil))))

   `(font-lock-warning-face
     ((t (:foreground "orangered" :weight normal))))

   `(font-lock-delimiter-face
     ((t (:foreground ,theme-dim))))

   `(font-lock-bracket-face
     ((t (:foreground ,theme-dim))))

   `(font-lock-operator-face
     ((t (:foreground ,theme-dim))))

   `(font-lock-doc-face
     ((t (:foreground "#008770"))))

   `(font-lock-escape-face
     ((t (:foreground ,theme-second :weight normal))))

   `(font-lock-property-use-face
     ((t (:foreground ,theme-accent))))

   ;; sh

   `(sh-heredoc
     ((t (:foreground "#008770"))))

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
     ((t (:family ,theme-font-family :foreground "#d65c7e"))))

   `(info-menu-star
     ((t (:foreground ,theme-second))))

   ;; apropos

   `(apropos-symbol
     ((t (:foreground ,theme-accent))))

   ;; compilation

   `(compilation-mode-line-exit
     ((t (:foreground ,theme-accent :weight normal))))

   `(compilation-mode-line-run
     ((t (:foreground ,theme-green :weight normal))))

   `(compilation-mode-line-fail
     ((t (:foreground ,theme-error :weight normal))))

   `(compilation-info
     ((t (:foreground ,theme-info :weight normal))))

   `(compilation-warning
     ((t (:foreground ,theme-warning :weight normal))))

   `(compilation-error
     ((t (:foreground ,theme-error :weight normal))))

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

   `(diff-context
     ((t (:background unspecified :foreground ,theme-dim))))

   `(diff-header
     ((t (:background unspecified :foreground unspecified :weight bold))))

   `(diff-hunk-header
     ((t (:background unspecified :foreground ,theme-blue))))

   `(diff-index
     ((t (:background unspecified :foreground ,theme-foreground))))

   `(diff-indicator-added
     ((t (:background "#C2F2CD" :foreground ,theme-green))))

   `(diff-indicator-removed
     ((t (:background "#FFD5D9" :foreground ,theme-red))))

   `(diff-indicator-changed
     ((t (:background "#FFE89D" :foreground ,theme-orange))))

   `(diff-added
     ((t (:background "#C2F2CD" :foreground unspecified))))

   `(diff-refine-added
     ((t (:background "#8BE39F" :foreground unspecified))))

   `(diff-removed
     ((t (:background "#FFD5D9" :foreground unspecified))))

   `(diff-refine-removed
     ((t (:background "#FFA5AE" :foreground unspecified))))

   `(diff-changed
     ((t (:background "#FFE89D" :foreground unspecified))))

   `(diff-refine-changed
     ((t (:background "#F0BF5B" :foreground unspecified))))

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

   ;; help

   `(help-for-help-header
     ((t (:font-height ,theme-font-height))))

   `(help-key-binding
     ((t (:box unspecified :foreground "cadetblue4"))))

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
     ((t (:background unspecified :weight normal))))

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

   ;; openai

   `(chatgpt-info
     ((t (:height: unspecified))))

   ))