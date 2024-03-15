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
   '(gdscript-mode . gdscript-ts-mode))

  ;; headless editor

  (defvar godot-editor-command
    "godot -e --headless --debug-server tcp://127.0.0.1:6007 --lsp-port 6005 \"%s\"")

  (defvar godot-editor-buffer-height 6)

  (defun godot-project ()
    "Run a command on a selected Godot project directory."
    (interactive)
    (let* ((project-dir (read-directory-name "Select Godot project directory: "))
           (project-file (expand-file-name "project.godot" project-dir))
           (project-name (file-name-nondirectory (directory-file-name project-dir))))
      ;; Check if the project.godot file exists in the selected directory.
      (if (file-exists-p project-file)
          (let* ((command (format godot-editor-command project-file))
                 (buffer-name (generate-new-buffer-name (format "*godot editor: %s*" project-name))))
            ;; Run the command asynchronously and specify a *unique* buffer name.
            (async-shell-command command buffer-name)
            (fit-window-to-buffer (get-buffer-window buffer-name) nil godot-editor-buffer-height))
        (message "The selected directory does not contain a project.godot file."))))
  )
