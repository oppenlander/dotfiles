;; Scala
(require 'scala-mode2)

;; ensime
(add-to-list 'load-path (concat user-emacs-directory "ensime"))
(require 'ensime)

;; start ensime with scala mode
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(provide 'setup-scala-mode)
