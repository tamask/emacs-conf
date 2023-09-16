;; (add-hook 'csharp-mode-hook (lambda () (setq comment-start "/*" comment-end "*/")))

(add-hook 'html-mode-hook (lambda () (set (make-local-variable 'sgml-basic-offset) 4)))

(setq shader-indent-offset 4)

(defun my-c-mode-hook ()
  (setq c-basic-offset 4)
  (c-set-offset 'substatement-open '0)
  (c-set-offset 'arglist-cont-nonempty '+)
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-close '0))

(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'csharp-mode-hook 'my-c-mode-hook)
