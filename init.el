;;
;; init
;;

;; load-relative

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'load-relative)
  (package-install 'load-relative))

(require 'load-relative)

;; load core scripts

(load-relative "./base")
(load-relative "./packages")
(load-relative "./codestyle")
(load-relative "./functions")
(load-relative "./shortcuts")

;; load theme

(when (uname "Darwin")
  (load-relative "./mac")
  (if (mac-ui-dark-mode)
    (load-relative "./display.nite")
  (load-relative "./display.lite")))

(when (uname "Linux")
  (load-relative "./display.nite"))
