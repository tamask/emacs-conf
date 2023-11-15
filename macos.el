;;
;; macos
;;

;; command key as meta

(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

;; minimalist titlebar

(setq ns-use-proxy-icon nil)
(setq frame-title-format nil)
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

;; detect whether we're in dark mode

(defun macos-ui-dark-mode ()
  (string= (string-trim (shell-command-to-string "defaults read -g AppleInterfaceStyle")) "Dark"))
