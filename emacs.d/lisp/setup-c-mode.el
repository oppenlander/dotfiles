
(add-hook 'c-mode-hook
					(lambda ()
						(add-to-list 'ac-sources 'ac-sources-c-headers)
						(add-to-list 'ac-sources 'ac-sources-c-header-symbols t)))

(provide 'setup-c-mode)
