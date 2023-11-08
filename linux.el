;;
;; linux
;;

(setq-default left-margin-width 1 right-margin-width 1)
(set-window-buffer nil (current-buffer))
(set-window-margins (selected-window) 1 1)

(defun linux-ui-dark-mode ()
  (string= (string-trim (shell-command-to-string "gsettings get org.gnome.desktop.interface color-scheme")) "'prefer-dark'"))