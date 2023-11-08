;;
;; godot
;;

(defun use-godot-dev ()
  (interactive)

  (setq godot-app-path "godot")

  (if (string= system-type "darwin")
      (setq godot-app-path "/Applications/Godot.app/Contents/MacOS/Godot"))

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
    (setq gdscript-godot-executable godot-app-path))

  (use-package gdshader-mode
    :straight
    (gdshader-mode
     :type git
     :host github
     :repo "bbbscarter/gdshader-mode")
    :mode ("\\.gdshader\\'" "\\.gdshaderinc\\'"))

  (use-package conf-mode
    :mode ("\\.godot\\'" "\\.import\\'" "\\.tres\\'" "\\.tscn\\'")))
