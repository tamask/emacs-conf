(setq c-default-style "stroustrup")

(setq default-tab-width 4)

(setq sgml-basic-offset 4)

(setq js-indent-level 4)

(setq nxml-child-indent 4 nxml-attribute-indent 4)

(add-hook 'html-mode-hook (lambda () (set (make-local-variable 'sgml-basic-offset) 4)))

(setq shader-indent-offset 4)

(defun my-c-mode-hook ()
  (setq c-basic-offset 4)
  (c-set-offset 'substatement-open '0)
  (c-set-offset 'arglist-cont-nonempty '+)
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-close '0)
  (c-set-offset 'func-decl-cont '0))

(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'c++-mode-hook 'my-c-mode-hook)
(add-hook 'csharp-mode-hook 'my-c-mode-hook)

;; (add-hook 'csharp-mode-hook (lambda () (setq comment-start "/*" comment-end "*/")))
