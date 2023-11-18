;;
;; linux
;;

(defun linux-ui-dark-mode ()
  (string= (string-trim (shell-command-to-string "gsettings get org.gnome.desktop.interface color-scheme")) "'prefer-dark'"))