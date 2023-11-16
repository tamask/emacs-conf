;;
;; unity
;;

(defun use-unity-dev ()
  (interactive)

  (use-package csharp-mode :mode "\\.cs\\'")

  (use-package blocking-mode
    :straight
    (blocking-mode
     :type git
     :host github
     :repo "cardboardcomputer/blocking-mode")
    :mode "\\.blocking.txt\\'")

  (use-package shader-mode
    :straight
    (shader-mode
     :type git
     :host github
     :repo "midnightsuyama/shader-mode")
    :mode ("\\.shader\\'" "\\.cginc\\'"))

  (setq dired-omit-files (concat dired-omit-files "\\|\\.meta$")))
