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
  (let ((my-bgcolor "#202020")) (boundp 'my-bgcolor))
  (let ((my-fgcolor "gainsboro")) (boundp 'my-fgcolor))

  ;; core colors

  (setq my-fgcolor "#cccccc")
  (setq my-bgcolor "#202020")
  ;;(setq my-maincolor "cadetblue2")
  ;;(setq my-maincolor "#97a4ef")
  ;; (setq my-maincolor "#9fadfc")
  (setq my-maincolor "#b0bcfc")
  (setq my-auxcolor "#ff9595")
  (setq my-hilite "#364b4f")
  (setq my-stringcolor "#ff9595")

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
     ((t (:family ,my-font-family :height ,my-font-height :background ,my-bgcolor :foreground ,my-fgcolor))))

   `(error
     ((t (:foreground ,my-maincolor :weight normal))))

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
     ((t (:background ,my-bgcolor :foreground "#777777"))))

   `(cursor
     ((t (:background ,my-fgcolor :foreground "black"))))

   `(highlight
     ((t (:background ,my-maincolor :foreground "black"))))

   `(match
     ((t (:background ,my-hilite :foreground ,my-maincolor))))

   `(lazy-highlight
     ((t (:background "skyblue4"))))

   `(shadow
     ((t (:foreground "grey40"))))

   `(show-paren-match
     ((t (:background ,my-maincolor :foreground "brown4"))))

   `(show-paren-mismatch
     ((t (:background "dimgrey" :foreground ,my-bgcolor))))

   `(tooltip
     ((t (:background ,my-bgcolor :foreground ,my-fgcolor))))

   `(region
     ((t (:background ,my-hilite))))

   `(header-line
     ((t (:background ,my-bgcolor :foreground "grey"))))

   `(isearch
     ((t (:background "palevioletred1" :foreground "brown4"))))

   `(link
     ((t (:foreground ,my-maincolor))))

   `(link-visited
     ((t (:foreground "wheat"))))

   `(success
     ((t (:foreground "palegreen"))))

   ;; modeline

   `(mode-line
     ((t (:box nil :background "grey20" :foreground ,my-fgcolor))))

   `(mode-line-highlight
     ((t (:box nil :background "grey25"))))

   `(mode-line-inactive
     ((t (:box nil :background "grey20" :foreground "dimgrey"))))

   ;; compilation

   `(compilation-mode-line-exit
     ((t (:foreground ,my-maincolor :weight normal))))

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
     ((t (:foreground ,my-maincolor))))

   `(dired-directory
     ((t (:foreground ,my-maincolor))))

   `(dired-flagged
     ((t (:foreground "#ff9595" :weight normal))))

   ;; minibuffer

   `(minibuffer-prompt
     ((t (:foreground ,my-maincolor))))

   ;; basic text

   `(trailing-whitespace
     ((t (:background "palevioletred1"))))

   `(escape-glyph
     ((t (:foreground ,my-maincolor))))

   `(glyphless-char
     ((t (:height ,my-font-height))))

   ;; font-lock

   `(font-lock-type-face
     ((t (:foreground ,my-maincolor))))

   `(font-lock-comment-face
     ((t (:foreground "dimgrey"))))

   `(font-lock-string-face
     ((t (:foreground ,my-stringcolor))))

   `(font-lock-builtin-face
     ((t (:foreground ,my-maincolor :weight normal))))

   `(font-lock-constant-face
     ((t (:foreground ,my-fgcolor :weight normal))))

   `(font-lock-function-name-face
     ((t (:foreground ,my-fgcolor))))

   `(font-lock-keyword-face
     ((t (:foreground ,my-maincolor))))

   `(font-lock-variable-name-face
     ((t (:foreground ,my-fgcolor))))

   `(font-lock-preprocessor-face
     ((t (:foreground ,my-maincolor :inherit nil))))

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
     ((t (:weight normal :slant normal :foreground ,my-maincolor))))

   `(info-menu-header
     ((t (:inherit unspecified :weight bold))))

   `(info-menu-star
     ((t (:foreground unspecified))))

   ;; markdown

   `(markdown-header-face
     ((t (:weight normal :foreground ,my-maincolor))))

   `(markdown-header-face-1
     ((t (:height unspecified :foreground ,my-maincolor))))

   `(markdown-header-face-2
     ((t (:height unspecified :foreground ,my-maincolor))))

   `(markdown-header-face-3
     ((t (:height unspecified :foreground ,my-maincolor))))

   `(markdown-header-face-4
     ((t (:height unspecified :foreground ,my-maincolor))))

   `(markdown-header-face-5
     ((t (:underline t :foreground ,my-maincolor))))

   `(markdown-header-face-6
     ((t (:slant italic :foreground ,my-maincolor))))

   `(markdown-bold-face
     ((t (:foreground ,my-fgcolor))))

   `(markdown-italic-face
     ((t (:foreground ,my-fgcolor))))

   `(markdown-inline-code-face
     ((t (:foreground "#ff9595"))))

   `(markdown-pre-face
     ((t (:foreground "#ff9595"))))

   `(markdown-url-face
     ((t (:foreground ,my-maincolor))))

   `(markdown-link-face
     ((t (:foreground ,my-maincolor))))

   ;; iswitchb

   `(iswitchb-current-match
     ((t (:foreground "khaki"))))

   `(iswitchb-single-match
     ((t (:foreground ,my-maincolor))))

   ;; ido

   `(ido-subdir
     ((t (:foreground "khaki"))))

   `(ido-only-match
     ((t (:foreground ,my-maincolor))))

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
     ((t (:background unspecified :foreground ,my-fgcolor))))

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
     ((t (:box nil :background ,my-maincolor :foreground "brown4"))))

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
