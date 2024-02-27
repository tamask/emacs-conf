;;
;; godot
;;

(defun use-godot-dev ()
  (interactive)

  (setq godot-app-path "godot")

  (if (string= system-type "darwin")
      (setq godot-app-path "/Applications/Godot.app/Contents/MacOS/Godot"))

  ;; gdscript

  (use-package gdscript-mode
    :straight
    (gdscript-mode
     :type git
     :host github
     :repo "godotengine/emacs-gdscript-mode")
    ;; :hook (gdscript-mode . eglot-ensure)
    :mode "\\.gd\\'"
    :config
    (setq gdscript-use-tab-indents nil)
    (setq gdscript-godot-executable godot-app-path)
    (define-key gdscript-mode-map (kbd "C-c i") 'increase-left-margin))

  ;; gdshader

  (use-package gdshader-mode
    :straight
    (gdshader-mode
     :type git
     :host github
     :repo "bbbscarter/gdshader-mode")
    :mode ("\\.gdshader\\'" "\\.gdshaderinc\\'"))

  ;; use conf mode (for now) for resource files

  (use-package conf-mode
    :mode ("\\.godot\\'" "\\.import\\'" "\\.tres\\'" "\\.tscn\\'"))

  ;; treesitter for godot

  (add-to-list
   'treesit-language-source-alist
   '(gdscript "https://github.com/PrestonKnopp/tree-sitter-gdscript"))

  (add-to-list
   'major-mode-remap-alist
   '(gdscript-mode . gdscript-ts-mode)))
