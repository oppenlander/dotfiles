
(require 'rainbow-delimiters)

;; Lisp configuration
(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)

;; wrap keybindings
(define-key lisp-mode-shared-map (kbd "M-(") (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "(")))
(define-key lisp-mode-shared-map (kbd "M-[") (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "[")))
(define-key lisp-mode-shared-map (kbd "M-\"") (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "\"")))

(defun my-lisp-coding-defaults ()
	(smartparens-strict-mode +1)
	(rainbow-delimiters +1))

(setq my-lisp-coding-hook 'my-lisp-coding-defaults)

;; interactive modes don't need whitespace checks
(defun my-interactive-lisp-coding-defaults ()
	(smartparens-strict-mode +1)
	(rainbow-delimiters +1)
	(whitespace-mode -1))

(setq my-interactive-lisp-coding-hook 'my-interactive-lisp-coding-defaults)

(provide 'setup-lisp-mode)
