;;
;; mac
;;

;; command key as meta

(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

(defun mac-ui-dark-mode ()
  (string= (string-trim (shell-command-to-string "defaults read -g AppleInterfaceStyle")) "Dark"))
