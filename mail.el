;;
;; mail
;;

(load-library "smtpmail")
(load-library "nnimap")
(load-library "starttls")

;; ignore subject, use in-reply-to header
(setq gnus-thread-ignore-subject t)

;; always show groups
(setq gnus-permanently-visible-groups "^.*$")

;; gnus group line that excludes the server name 
(setq gnus-group-line-format "%M%S%5y:%B%(%G%)\n")

;; avoid arbitrary grouping by subject
(setq gnus-summary-make-false-root nil)

(setq gnus-thread-sort-functions
      '(gnus-thread-sort-by-number
        gnus-thread-sort-by-most-recent-date))

;; terse date formatting
(setq gnus-user-date-format-alist '(( t . "%Y-%m-%d")))

;; gnus summary buffer tree styling (ascii)
(setq gnus-sum-thread-tree-indent "  ")
(setq gnus-sum-thread-tree-root "")
(setq gnus-sum-thread-tree-false-root "o ")
(setq gnus-sum-thread-tree-single-indent "")
(setq gnus-sum-thread-tree-leaf-with-other "+-> ")
(setq gnus-sum-thread-tree-vertical "| ") 
(setq gnus-sum-thread-tree-single-leaf "`-> ")

(setq gnus-summary-line-format
      "%O : %U %R | %&user-date;%-20= | %-15,15f | %B %S \n")

;; gnus summary buffer tree styling (unicode)
(when window-system
  (setq gnus-sum-thread-tree-indent "  ")
  (setq gnus-sum-thread-tree-root "●")
  (setq gnus-sum-thread-tree-false-root "◯")
  (setq gnus-sum-thread-tree-single-indent "◎")
  (setq gnus-sum-thread-tree-leaf-with-other "├─►")
  (setq gnus-sum-thread-tree-vertical "│")
  (setq gnus-sum-thread-tree-single-leaf "╰─►")

  (setq gnus-summary-line-format
        "%O : %U %R │ %&user-date;%-20= │ %-15,15f │ %B %S \n"))

;; w3m
(setq mm-text-html-renderer 'w3m)

;; contacts via bbdb
(require 'bbdb)
(bbdb-initialize 'gnus 'message)
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
(setq bbdb-default-area-code "416")

;; multiple smtp
(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      mail-from-style nil)
      ;; smtpmail-debug-info t
      ;; smtpmail-debug-verb t)

(defun set-smtp-plain (server port)
  "Set related SMTP variables for supplied parameters."
  (setq smtpmail-smtp-server server
        smtpmail-smtp-service port
        smtpmail-auth-credentials "~/.authinfo"
        smtpmail-starttls-credentials nil)
  (message "Setting SMTP server to `%s:%s’."
           server port address))

(defun set-smtp-ssl (server port key cert)
  "Set related SMTP and SSL variables for supplied parameters."
  (setq ;; starttls-use-gnutls t
        ;; starttls-gnutls-program "gnutls-cli"
        starttls-extra-arguments nil
        smtpmail-smtp-server server
        smtpmail-smtp-service port
        smtpmail-starttls-credentials (list (list server port key cert))
        smtpmail-auth-credentials "~/.authinfo")
  (message
   "Setting SMTP server to `%s:%s’. (SSL enabled.)"
   server port address))

(defun change-smtp ()
  "Change the SMTP server according to the current from line."
  (save-excursion
    ;; cl required for 'loop' function
    (require 'cl) 
    (loop with from = (save-restriction
                        (message-narrow-to-headers)
                        (message-fetch-field "from"))
          for (acc-type address . auth-spec) in smtp-accounts
          when (string-match address from)
          do (cond
              ((eql acc-type 'plain)
               (return (apply 'set-smtp-plain auth-spec)))
              ((eql acc-type 'ssl)
               (return (apply 'set-smtp-ssl auth-spec)))
              (t (error "Unrecognized SMTP account type: `%s'." acc-type)))
          finally (error "Cannot infer SMTP information."))))

(add-hook 'message-send-hook 'change-smtp)


;; example ~/.authinfo:

    ;; machine mail.example1.com   login user   password p455w0rd
    ;; machine mail.example2.com   login user   password p455w0rd

;; example mail config in .emacs

    ;; (setq user-mail-address "email@example1.com")
    ;; (setq user-full-name "Your Name")

    ;; (setq gnus-posting-styles
    ;;       '(("example1.com"
    ;;          (address "user@example1.com"))
    ;;         ("example2.com"
    ;;          (address "user@example2.com"))))

    ;; (setq gnus-select-method '(nnfolder "local"))

    ;; (setq gnus-secondary-select-methods
    ;;       '((nnimap "user@example1.com"
    ;;                 (nnimap-address "mail.example1.com")
    ;;                 (nnimap-server-port 993)
    ;;                 (nnimap-stream ssl))
    ;;         (nnimap "user@example2.com"
    ;;                 (nnimap-address "mail.example2.com")
    ;;                 (nnimap-server-port 143))))

    ;; (defvar smtp-accounts
    ;;   '((ssl "user@example1.com" "mail.example1.com" 587 "key" nil)
    ;;     (plain "user@example2.com" "mail.example2.com" 25)))

    ;; (defun gnus-outgoing-message-group-func ()
    ;;   (cond
    ;;    ((string-match
    ;;      "user@example1.com"
    ;;      gnus-newsgroup-name)
    ;;     "nnimap+user@example1.com:Sent")
    ;;    ((string-match
    ;;      "user@example2.com"
    ;;      gnus-newsgroup-name)
    ;;     "nnimap+user@example2.com:Sent")))

    ;; (setq gnus-outgoing-message-group 'gnus-outgoing-message-group-func)
