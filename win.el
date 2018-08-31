;;
;; windows
;;

(defun unix-eol ()
  (set-buffer-file-coding-system 'utf-8-unix)
  (set-buffer-modified-p nil))

(add-hook 'find-file-hook 'unix-eol)
