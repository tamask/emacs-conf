;;
;; themes
;;

(when window-system
  (load-relative "./theme-light")
  (load-relative "./theme-night")
)

(defun theme-dark-mode ()
  (or
    (if (string= system-type "gnu/linux") (linux-ui-dark-mode)
    (if (string= system-type "darwin") (macos-ui-dark-mode)))))

(defun theme-auto ()
  (interactive)
  (when window-system
    (if (theme-dark-mode) (theme-night) (theme-light))))

(theme-auto)