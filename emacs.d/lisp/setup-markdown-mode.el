(require 'markdown-mode)

(require 'pandoc-mode)
(add-hook 'markdown-mode-hook 'turn-on-pandoc)
(setq pandoc-major-modes (quote ((haskell-mode . "native") (text-mode . "markdown") (markdown-mode . "markdown_github") (mediawiki-mode . "mediawiki") (textile-mode . "textile") (rst-mode . "rst") (html-mode . "html") (latex-mode . "latex") (json-mode . "json"))))
(add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)

(provide 'setup-markdown-mode)