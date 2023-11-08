;;
;; init
;;

;; bootstrap load-relative package

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'load-relative)
  (package-install 'load-relative))

(require 'load-relative)

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
