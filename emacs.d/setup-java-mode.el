
(require 'cedet)
(require 'semantic)
(load "semantic/loaddefs.el")
(semantic-mode 1)
;; (require 'malabar-mode)

;; ;; Compile on save
;; (add-hook 'malabar-mode-hook
;;           (lambda ()
;;             (add-hook 'after-save-hook 'malabar-compile-file-silently
;;                       nil t)))

(provide 'setup-java-mode)
