(require 'markdown-mode)

(require 'pandoc-mode)
(add-hook 'markdown-mode-hook 'turn-on-pandoc)
(setq pandoc-major-modes (quote ((haskell-mode . "native") (text-mode . "markdown") (markdown-mode . "markdown_github") (mediawiki-mode . "mediawiki") (textile-mode . "textile") (rst-mode . "rst") (html-mode . "html") (latex-mode . "latex") (json-mode . "json"))))
(add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)

;; Preview inspired by https://gist.github.com/Javran/9181746

(defun my-pandoc-markdown-to-html (file-src file-dst)
  "convert markdown files into HTML files."
  (shell-command
   (format "pandoc -s -t html5 %s -o %s" file-src file-dst)))

(defun my-markdown-preview-file ()
    "generate HTML file for current editing file
    using pandoc, and the open browser to preview
    the resulting HTML file"
    (interactive)
    ;; create place to store the temp HTML file output
    (mkdir "/tmp/markdown_tmps/" t)
    (let* ((dst-dir "/tmp/markdown_tmps/")
           (file-dst
            (concat dst-dir
                    (file-name-base (buffer-file-name))
                    ".html"))
           (url-dst
            (concat "file://" file-dst)))
      (my-pandoc-markdown-to-html (buffer-file-name)
                               file-dst)
      (split-window-below)
      (other-window 1)
      (eww url-dst)))

(define-key pandoc-mode-map "\C-c/h" 'my-markdown-preview-file)

(provide 'setup-markdown-mode)