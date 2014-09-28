;; Check code style
(require 'python-pep8)

;; Check for errors
(require 'python-pylint)

;; Virtualenv
(require 'virtualenv)

;; Completion
(setq jedi:setup-keys t)
(require 'jedi)
(add-hook 'python-mode-hook 'jedi:setup)

																				;(setq jedi:server-command (list "python2" jedi:server-script))

(defun my-jedi-server-setup ()
	(let ((bin "python2"))
		(set (make-local-variable 'jedi:server-command) (list bin jedi:server-script))))
(add-hook 'python-mode-hook 'my-jedi-server-setup)

;; IPython
																				;(require 'ipython)
																				;(require 'ein)
																				;(add-hook 'ein:connect-mode-hook 'ein:jedi-setup)

(provide 'setup-python-mode)
