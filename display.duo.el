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

  ;; core colors

  (setq duo-gradient-1   "#ffffff")
  (setq duo-gradient-1_5 "#dddddd")
  (setq duo-gradient-2   "#bbbbbb")
  (setq duo-gradient-2_5 "#888888")
  (setq duo-gradient-3   "#555555")
  (setq duo-gradient-3_5 "#444444")
  (setq duo-gradient-4   "#333333")
  (setq duo-gradient-4_5 "#282828")
  (setq duo-gradient-5   "#202020")
  (setq duo-error        "palevioletred1")

  ;; default frame attributes and initial position
  (setq my-frame-alist `
        ((width . ,my-frame-width)
         (height . ,my-frame-height)))

  ;; ansi term
  (setq ansi-term-color-vector
        [nil "#000000" "#ffa965" "#42ffb6" "#fff485"
             "#93Ceff" "#ffa3CC" "#aeffff"])

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

  (custom-set-faces
   ;; basic

   `(default
     ((t (:family ,my-font-family :height ,my-font-height :background ,duo-gradient-5 :foreground ,duo-gradient-2))))

   `(error
     ((t (:foreground ,duo-error :weight normal))))

   `(variable-pitch
     ((t (:family ,my-font-family))))

   `(fixed-pitch
     ((t (:family ,my-font-family))))

   `(fixed-pitch-serif
     ((t (:family ,my-font-family))))

   `(border
     ((t (:background "grey"))))

   `(vertical-border
     ((t (:foreground "#191919"))))

   `(fringe
     ((t (:background ,duo-gradient-5 :foreground "#777777"))))

   `(cursor
     ((t (:background ,duo-gradient-1 :foreground ,duo-gradient-5))))

   `(highlight
     ((t (:background ,duo-gradient-1 :foreground ,duo-gradient-5))))

   `(match
     ((t (:background ,duo-gradient-4 :foreground ,duo-gradient-1))))

   `(lazy-highlight
     ((t (:background "skyblue4"))))

   `(shadow
     ((t (:foreground "grey40"))))

   `(show-paren-match
     ((t (:background ,duo-gradient-3_5 :foreground ,duo-gradient-1))))

   `(show-paren-mismatch
     ((t (:background ,duo-error :foreground ,duo-gradient-5))))

   `(tooltip
     ((t (:background ,duo-gradient-5 :foreground ,duo-gradient-2))))

   `(region
     ((t (:background ,duo-gradient-4))))

   `(header-line
     ((t (:background ,duo-gradient-5 :foreground "grey"))))

   `(isearch
     ((t (:background "palevioletred1" :foreground "brown4"))))

   `(link
     ((t (:foreground ,duo-gradient-1))))

   `(link-visited
     ((t (:foreground "wheat"))))

   `(success
     ((t (:foreground "palegreen"))))

   ;; modeline

   `(mode-line
     ((t (:box (:line-width 7 :color ,duo-gradient-4) :background ,duo-gradient-4 :foreground ,duo-gradient-2))))

   `(mode-line-highlight
     ((t (:box nil :background ,duo-gradient-3))))

   `(mode-line-inactive
     ((t (:box (:line-width 7 :color ,duo-gradient-4) :background ,duo-gradient-4 :foreground ,duo-gradient-2_5))))

   ;; compilation

   `(compilation-mode-line-exit
     ((t (:foreground ,duo-gradient-1 :weight normal))))

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
     ((t (:foreground ,duo-gradient-1))))

   `(dired-directory
     ((t (:foreground ,duo-gradient-1))))

   `(dired-flagged
     ((t (:foreground "#ff9595" :weight normal))))

   ;; minibuffer

   `(minibuffer-prompt
     ((t (:foreground ,duo-gradient-1))))

   ;; basic text

   `(trailing-whitespace
     ((t (:background "palevioletred1"))))

   `(escape-glyph
     ((t (:foreground ,duo-gradient-1))))

   `(glyphless-char
     ((t (:height ,my-font-height))))

   ;; font-lock

   `(font-lock-type-face
     ((t (:foreground ,duo-gradient-1))))

   `(font-lock-comment-face
     ((t (:foreground ,duo-gradient-3))))

   `(font-lock-string-face
     ((t (:foreground ,duo-gradient-2_5 :slant italic))))

   `(font-lock-builtin-face
     ((t (:foreground ,duo-gradient-1 :weight normal))))

   `(font-lock-constant-face
     ((t (:foreground ,duo-gradient-1 :weight normal))))

   `(font-lock-function-name-face
     ((t (:foreground ,duo-gradient-2))))

   `(font-lock-keyword-face
     ((t (:foreground ,duo-gradient-1))))

   `(font-lock-variable-name-face
     ((t (:foreground ,duo-gradient-2))))

   `(font-lock-preprocessor-face
     ((t (:foreground ,duo-gradient-1 :inherit nil))))

   `(font-lock-warning-face
     ((t (:foreground "khaki" :weight normal))))

   ;; info reader

   `(info-title-1
     ((t (:weight normal :height unspecified :weight bold :underline t))))

   `(info-title-2
     ((t (:weight normal :height unspecified :weight bold :underline t))))

   `(info-title-3
     ((t (:weight normal :height unspecified :weight bold :underline t))))

   `(info-node
     ((t (:weight normal :slant normal :foreground ,duo-gradient-1))))

   `(info-menu-header
     ((t (:inherit unspecified :weight bold))))

   `(info-menu-star
     ((t (:foreground unspecified))))

   ;; markdown

   `(markdown-header-face
     ((t (:weight normal :foreground ,duo-gradient-1))))

   `(markdown-header-face-1
     ((t (:height unspecified :foreground ,duo-gradient-1))))

   `(markdown-header-face-2
     ((t (:height unspecified :foreground ,duo-gradient-1))))

   `(markdown-header-face-3
     ((t (:height unspecified :foreground ,duo-gradient-1))))

   `(markdown-header-face-4
     ((t (:height unspecified :foreground ,duo-gradient-1))))

   `(markdown-header-face-5
     ((t (:underline t :foreground ,duo-gradient-1))))

   `(markdown-header-face-6
     ((t (:slant italic :foreground ,duo-gradient-1))))

   `(markdown-bold-face
     ((t (:foreground ,duo-gradient-2))))

   `(markdown-italic-face
     ((t (:foreground ,duo-gradient-2))))

   `(markdown-inline-code-face
     ((t (:foreground "#ff9595"))))

   `(markdown-pre-face
     ((t (:foreground "#ff9595"))))

   `(markdown-url-face
     ((t (:foreground ,duo-gradient-1))))

   `(markdown-link-face
     ((t (:foreground ,duo-gradient-1))))

   ;; iswitchb

   `(iswitchb-current-match
     ((t (:foreground "khaki"))))

   `(iswitchb-single-match
     ((t (:foreground ,duo-gradient-1))))

   ;; ido

   `(ido-subdir
     ((t (:foreground "khaki"))))

   `(ido-only-match
     ((t (:foreground ,duo-gradient-1))))

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
     ((t (:background unspecified :foreground ,duo-gradient-2))))

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
     ((t (:box nil :background "#444444" :foreground "beige"))))

   `(custom-button-mouse
     ((t (:box nil :background "skyblue4" :foreground "beige"))))

   `(custom-button-pressed
     ((t (:box nil :background ,duo-gradient-1 :foreground "brown4"))))

   `(custom-group-tag
     ((t (:height unspecified))))

   `(custom-group-tag-1
     ((t (:height unspecified))))

   `(custom-visibility
     ((t (:height unspecified))))

   ;; widget

   `(widget-field
     ((t (:box nil :background "#444444"))))

   `(widget-single-line-field
     ((t (:box nil :background "#444444"))))

   ;; bookmark

   `(bookmark-face
     ((t (:background unspecified :foreground "dimgrey"))))
   )
)
