(require 'web-mode)

(add-hook 'web-mode-hook '(lambda () (setq indent-tabs-mode 1)))

;; HTML offset
(setq web-mode-markup-indent-offset 2)
;; CSS offset
(setq web-mode-css-indent-offset 2)
;; Script (JavaScript, Java, PHP) offset
(setq web-mode-code-indent-offset 2)

;(add-hook 'css-mode-hook 'skewer-css-mode)
;(add-hook 'html-mode-hook 'skewer-html-mode)

(provide 'setup-web-mode)
