;; LaTeX

;; Auto Complete
(require 'ac-math)
(add-to-list 'ac-modes 'latex-mode) ; make auto-complete aware of `latex-mode`
(defun ac-latex-mode-setup () ; add ac-sources to default ac-sources
	(setq ac-sources
				(append '(ac-source-math-unicode ac-source-math-latex ac-source-latex-commands)
								ac-sources))
	)
(add-hook 'latex-mode-hook 'ac-latex-mode-setup)

;; Make auto-complete work with flyspell
(ac-flyspell-workaround)
