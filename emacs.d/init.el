;; emacs <= 24.3
(unless (fboundp 'with-eval-after-load)
  (defmacro with-eval-after-load (file &rest body)
    (declare (indent 1) (debug t))
    `(eval-after-load ,file '(progn ,@body))))

;; Turn off tool-bar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; Turn off menu-bar
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; No splash screen
(setq inhibit-startup-message t)

;; Set custom (built in) theme
(load-theme 'wombat t)

;; Set font
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 90 :width normal)))))

;; Set path to dependencies
(setq site-lisp-dir
			(expand-file-name "site-lisp" user-emacs-directory))

;; Set up load path
(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path site-lisp-dir)

;; Write backup files to own directory
(setq backup-directory-alist
			`(("." . ,(expand-file-name
								 (concat user-emacs-directory "backups")))))
(setq auto-save-file-name-transforms
			`((".*" ,(expand-file-name
								(concat user-emacs-directory "tempfiles")) t)))
(setq backup-by-copying t)
(setq delete-old-versions t
			kept-new-versions 6
			kept-old-versions 2
			version-control t)
(setq create-lockfiles nil)

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; Install extensions if they're missing
(require 'setup-package)
(defun init--install-packages ()
	(packages-install
	 (cons 'smex melpa)
	 (cons 'browse-kill-ring melpa)
	 (cons 'projectile melpa)
	 (cons 'magit melpa)
	 (cons 'slime-js marmalade)
	 (cons 'clojure-mode melpa)
	 (cons 'clojure-test-mode melpa)
	 (cons 'cider melpa)
	 (cons 'rainbow-delimiters melpa)
	 (cons 'js2-mode melpa)
	 (cons 'js2-refactor melpa)
	 (cons 'ac-js2 melpa)
	 (cons 'ido-ubiquitous melpa)
	 (cons 'ido-vertical-mode melpa)
	 (cons 'ido-at-point melpa)
	 (cons 'flx-ido melpa)
	 (cons 'fill-column-indicator melpa)
	 (cons 'auto-complete melpa)
	 (cons 'web-mode melpa)
	 (cons 'nlinum gnu)
	 (cons 'evil melpa)
	 (cons 'markdown-mode melpa)
	 (cons 'lua-mode melpa)
	 (cons 'coffee-mode melpa)
	 (cons 'jade-mode melpa)
	 (cons 'python-pep8 marmalade)
	 (cons 'python-pylint marmalade)
	 (cons 'virtualenv melpa)
	 (cons 'jedi melpa)
	 (cons 'jedi-direx melpa)
	 (cons 'ein melpa)
	 (cons 'nav melpa)
	 (cons 'org melpa)
	 (cons 'goto-last-change melpa)
	 (cons 'direx melpa)
	 (cons 'ac-math melpa)
	 (cons 'prolog melpa)
	 (cons 'ediprolog gnu)
	 (cons 'rust-mode melpa)
	 (cons 'flymake-easy melpa)
	 (cons 'flymake-rust melpa)
	 (cons 'go-mode melpa)
	 (cons 'go-autocomplete melpa)
	 (cons 'go-direx melpa)
	 (cons 'go-errcheck melpa)
	 (cons 'flymake-go marmalade)
	 (cons 'ac-c-headers melpa)
	 (cons 'sr-speedbar melpa)
	 (cons 'howdoi melpa)
	 (cons 'ack-and-a-half melpa)
	 (cons 'yasnippet melpa)
	 (cons 'popwin melpa)
	 (cons 'floobits melpa)
	 (cons 'geiser melpa)
	 (cons 'tern melpa)
	 (cons 'tern-auto-complete melpa)
	 (cons '2048-game melpa)
	 (cons 'powerline melpa)
	 (cons 'sublimity melpa)
	 (cons 'ag melpa)
	 (cons 'multi-term melpa)
	 (cons 'pandoc-mode melpa)
	 (cons 'handlebars-mode melpa)
	 (cons 'less-css-mode melpa)
	 (cons 'flymake-less marmalade)
	 (cons 'flymake-jshint melpa)
	 (cons 'discover melpa)
	 (cons 'discover-js2-refactor melpa)
	 (cons 'grunt melpa)
	 (cons 'smartparens melpa)
	 (cons 'diminish melpa)
	 (cons 'helm-spotify melpa)
	 (cons 'helm-dash melpa)
	 (cons 'helm-themes melpa)
	 (cons 'w3 gnu)
	 (cons 'w3m melpa)
	 (cons 'guide-key melpa)
	 (cons 'visual-regexp melpa)
	 (cons 'visual-regexp-steroids melpa)
	 (cons 'js-comint melpa)
	 (cons 'flycheck melpa)
	 (cons 'json-mode melpa)
	 (cons 'ace-jump-mode melpa)
	 (cons 'csv-mode marmalade)
	 (cons 'password-store melpa)
	 (cons 'fish-mode melpa)
   (cons 'sudo-edit marmalade)
   (cons 'math-at-point marmalade)
   (cons 'move-line marmalade)
   (cons 'workgroups2  melpa)
	 ))
(condition-case nil
		(init--install-packages)
	(error
	 (package-refresh-contents)
	 (init--install-packages)))

;; Misc
(require 'my-misc)

;; Setup extensions
(eval-after-load 'ido '(require 'setup-ido))

;; Snippets
(eval-after-load 'yasnippet '(require 'setup-yasnippet))

;; Language specific extensions
(eval-after-load 'js2-mode '(require 'setup-js2-mode))
(eval-after-load 'clojure-mode '(require 'setup-clojure-mode))
(eval-after-load 'python '(require 'setup-python-mode))
(eval-after-load 'web-mode '(require 'setup-web-mode))
(require 'typescript)
(eval-after-load 'scala-mode2 '(require 'setup-scala-mode))
(require 'markdown-mode)
(eval-after-load 'lua-mode '(require 'setup-lua-mode))
(require 'coffee-mode)
(setq-default coffee-tab-width 2)
(require 'jade-mode)
(eval-after-load 'latex-mode '(require 'setup-latex-mode))
(eval-after-load 'prolog-mode '(require 'setup-prolog-mode))
(eval-after-load 'octave-mode '(require 'setup-octave-mode))
(eval-after-load 'rust-mode '(require 'setup-rust-mode))
(eval-after-load 'go-mode '(require 'setup-go-mode))
																				;(require 'setup-java-mode)
(eval-after-load 'markdown-mode '(require 'setup-markdown-mode))
(require 'setup-helm-mode)
(require 'fish-mode)

;; Map files to modes
(require 'mode-mappings)

;; Smart M-x
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; Emacs server
(require 'server)
(unless (server-running-p)
	(server-start))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(js-indent-level 2)
 '(typescript-auto-indent-flag t)
 '(typescript-indent-level 2))
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
