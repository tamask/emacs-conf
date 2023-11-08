;;
;; init
;;

;; straight.el bootstrap

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;; bootstrap load-relative package

(use-package load-relative :straight t)

;; platform specifics

(when (string= system-type "gnu/linux") (load-relative "./linux"))
(when (string= system-type "darwin") (load-relative "./macos"))
(when (string= system-type "windows-nt") (load-relative "./windows"))

;; load core scripts

(load-relative "./frame")
(load-relative "./themes")
(load-relative "./general")
(load-relative "./packages")
(load-relative "./codestyle")
(load-relative "./functions")
(load-relative "./shortcuts")


;; dev environments

(load-relative "./godot")
(load-relative "./unity")
