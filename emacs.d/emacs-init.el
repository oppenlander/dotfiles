
(package-initialize)
(unless (require 'quelpa nil t)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
    (eval-buffer)))

(quelpa 'bind-key)
(require 'bind-key)

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please
(setq inhibit-startup-message t)

;; No alarms.
(setq ring-bell-function 'ignore)

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Backup files correctly and clean up versions
(setq backup-by-copying t)
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)
(setq version-control t)

;; Disable lock files
(setq create-lockfiles nil)

;; Write auto-saves to own directory
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name
                (concat user-emacs-directory "tempfiles")) t)))


;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name "places" user-emacs-directory))

;; Fix empty pasteboard error
(setq save-interprogram-paste-before-kill nil)

;; Enable some commands
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'erase-buffer 'disabled nil)

;; Allow pasting selection outside Emacs
(setq x-select-enable-clipboard t)

;; Move deleted fiels to trash
(setq delete-by-moving-to-trash t)

;; No shift marking/selecting
(setq shift-select-mode nil)

;; Answer yes/no with y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Set coding system to utf-8 everywhere
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Use spaces by defult
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; Line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; The mnemonic is C-x REALLY QUIT
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-c") 'delete-frame)

;; Home and End Keys:
(global-set-key (kbd "<home>") 'move-beginning-of-line)
(global-set-key (kbd "<end>") 'move-end-of-line)

;; Joins Lines into one
(global-set-key (kbd  "M-j") '(lambda () (interactive) (join-line -1)))

;; Kill line from the left
(global-set-key (kbd "<s-backspace>") '(lambda () (interactive) (kill-line 0)))

;; Quickly jump to last change
(global-set-key (kbd "C-x C-\\") 'goto-last-change)

;; Clean up whitespace
(global-set-key (kbd "C-c @ c") 'delete-trailing-whitespace)

;; Replace string
(global-set-key (kbd "C-c r") 'replace-string)

(dolist (keys '("<M-up>" "<M-down>" "<s-left>" "<s-right>"
                "s-c" "s-v" "s-x" "s-v" "s-q" "s-s" "s-w"
                "s-a" "s-o" "s-n" "s-p" "s-k" "s-u" "s-m"
                "s-f" "s-z" "s-g" "s-d" "s-," "s-:" "s-e"
                "s-t" "C-z"))
  (global-unset-key (kbd keys)))

(set-face-attribute 'default nil :family "DejaVu Sans Mono" :height 98)

(setq custom-theme-directory (concat user-emacs-directory "themes/"))

(load-theme 'smyx-custom t)

(setq display-time-day-and-date t
                display-time-format "%a %b %d %R"
                display-time-interval 60
                display-time-default-load-average nil)
             (display-time)

(quelpa 'powerline)
(powerline-default-theme)

(defun add-operator-hl ()
  (font-lock-add-keywords
   nil
   '(("\s[-]\s\\|\s[/]\s\\|[%]\\|[+]\\|[*]\\|[!=]\\|[/=]\\|[<=]\\|[>=]" . font-lock-keyword-face))))
;; prog-mode applies to all programming modes
(add-hook 'prog-mode-hook 'add-operator-hl)

(setq redisplay-dont-pause t)

(quelpa 'ido)
(quelpa 'flx-ido)
(quelpa 'ido-vertical-mode)
(quelpa 'ido-ubiquitous)

(require 'ido)
(require 'flx-ido)
(require 'ido-vertical-mode)
(require 'ido-ubiquitous)

(ido-mode t)
(flx-ido-mode 1)
(setq ido-use-faces nil)
(ido-vertical-mode)
(ido-ubiquitous-mode 1)

(quelpa 'smex)
(require 'smex)
(bind-key "M-x" 'smex)
(bind-key "M-X" 'smex-major-mode-commands)

(quelpa 'diminish)
(require 'diminish)

(quelpa 'company)
(require 'company)
(diminish 'company-mode)
(setq company-idle-delay 0.3)
(setq company-tooltip-limit 20)
(setq company-minimum-prefix-length 2)
(global-company-mode t)

(quelpa 'ace-jump-mode)
(require 'ace-jump-mode)
(bind-key "C-c SPC" 'ace-jump-mode)

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 100)
(setq recentf-max-menu-items 15)

(quelpa 'yasnippet)
(require 'yasnippet)

(setq yas-snippet-dirs '("~/.emacs.d/snippets/"))
(add-to-list 'auto-mode-alist '("yasnippet/snippets" . snippet-mode))
(add-to-list 'auto-mode-alist '("\\.yasnippet$" . snippet-mode))
(yas-global-mode 1)
(diminish 'yas-minor-mode)
;; No need to be so verbose
(setq yas-verbosity 1)
;; Wrap around region
(setq yas-wrap-around-region t)
;; Bind only during snippet
(bind-key "<return>" 'yas/exit-all-snippets yas-keymap)
(bind-key "C-e" 'yas/goto-end-of-active-field yas-keymap)
(bind-key "C-a" 'yas/goto-start-of-active-field yas-keymap)

;; Interactive-Field navigation
(defun yas/goto-end-of-active-field ()
  (interactive)
  (let* ((snippet (car (yas--snippets-at-point)))
         (position (yas--field-end (yas--snippet-active-field snippet))))
    (if (= (point) position)
        (move-end-of-line 1)
      (goto-char position))))

(defun yas/goto-start-of-active-field ()
  (interactive)
  (let* ((snippet (car (yas--snippets-at-point)))
         (position (yas--field-start (yas--snippet-active-field snippet))))
    (if (= (point) position)
        (move-beginning-of-line 1)
      (goto-char position))))

;; fix some org-mode + yasnippet conflicts:
(defun yas/org-very-safe-expand ()
  (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

(quelpa 'undo-tree)
(require 'undo-tree)
(diminish 'undo-tree-mode)

(global-undo-tree-mode 1)

(bind-key "C-x x u" 'undo-tree-visualize)
(bind-key "C-x x r u" 'undo-tree-save-state-to-register)
(bind-key "C-x x r U" 'undo-tree-restore-state-from-register)

(quelpa 'move-text)
(require 'move-text)
(bind-key "C-S-<up>" 'move-text-up)
(bind-key "C-S-<down>" 'move-text-down)

(quelpa 'webjump)
(require 'webjump)
(bind-key "C-c j" 'webjump)

(require 'browse-url)
(bind-key "C-c C-j" 'browse-url)

(quelpa 'smartparens)
(require 'smartparens)
(diminish 'smartparens-mode)

(smartparens-global-mode t)
;; The '' pair will autopair UNLESS the point is right after a word,
;; in which case you want to insert a single apostrophe.
(sp-pair "'" nil :unless '(sp-point-after-word-p))

;; disable single quote completion in
;; emacs-lisp-mode WHEN point is inside a string. In other modes, the
;; global definition is used.
(sp-local-pair 'emacs-lisp-mode "'" nil :when '(sp-in-string-p))
(sp-local-pair 'lisp-interaction-mode "'" nil :when '(sp-in-string-p))

(quelpa 'smart-compile)
(require 'smart-compile)

(bind-key "C-x c c" 'smart-compile)

(remove '("\\.c\\'" . "gcc -O2 %f -lm -o %n") 'smart-compile-alist)
;; compile and run programs
(add-to-list 'smart-compile-alist '("\\.c\\'" . "gcc -O2 -Wall %f -lm -o %n"))
(add-to-list 'smart-compile-alist '("\\.cpp\\'" . "g++ -Wall -ggdb %f -lm -o %n"))
(add-to-list 'smart-compile-alist '("\\.py\\'" . "python %f"))
(add-to-list 'smart-compile-alist '("\\.hs\\'" . "ghc -o %n %f"))
(add-to-list 'smart-compile-alist '("\\.js\\'" . "node %f"))

(quelpa '(rainbow-mode :url "http://git.savannah.gnu.org/cgit/emacs/elpa.git/plain/packages/rainbow-mode/rainbow-mode.el" :fetcher url))
(require 'rainbow-mode)
(diminish 'rainbow-mode)
(add-hook 'prog-mode-hook 'rainbow-mode)

(quelpa 'flyspell)
(require 'flyspell)
(diminish 'flyspell-mode)

;; Enable spell check in program comments
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
;; Enable spell check in plain text / org-mode
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'flyspell-mode)

(setq flyspell-issue-welcome-flag nil)
(setq flyspell-issue-message-flag nil)

;; ignore repeated words
(setq flyspell-mark-duplications-flag nil)

(setq-default ispell-list-command "list")

;; Make spell check on right click.
(define-key flyspell-mouse-map [down-mouse-3] 'flyspell-correct-word)
(define-key flyspell-mouse-map [mouse-3] 'undefined)

(quelpa 'flycheck)
(require 'flycheck)
(diminish 'flycheck-mode)

(add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)) ; disable the annoying doc checker
(setq flycheck-indication-mode 'left-fringe)
(defun magnars/adjust-flycheck-automatic-syntax-eagerness ()
  "Adjust how often we check for errors based on if there are any.

This lets us fix any errors as quickly as possible, but in a
clean buffer we're an order of magnitude laxer about checking."
  (setq flycheck-idle-change-delay
        (if flycheck-current-errors 0.5 30.0)))

;; Each buffer gets its own idle-change-delay because of the
;; buffer-sensitive adjustment above.
(make-variable-buffer-local 'flycheck-idle-change-delay)

(add-hook 'flycheck-after-syntax-check-hook
          'magnars/adjust-flycheck-automatic-syntax-eagerness)

;; Remove newline checks, since they would trigger an immediate check
;; when we want the idle-change-delay to be in effect while editing.
(setq flycheck-check-syntax-automatically '(save
                                            idle-change
                                            mode-enabled))

(defun flycheck-handle-idle-change ()
  "Handle an expired idle time since the last change.

This is an overwritten version of the original
flycheck-handle-idle-change, which removes the forced deferred.
Timers should only trigger inbetween commands in a single
threaded system and the forced deferred makes errors never show
up before you execute another command."
  (flycheck-clear-idle-change-timer)
  (flycheck-buffer-automatically 'idle-change))

(quelpa 'popwin)
(require 'popwin)
(popwin-mode 1)
(setq helm-popwin
      '("*helm mini*" :height 10))

(quelpa 'helm)
(quelpa 'helm-dash)
(quelpa 'helm-spotify)
(quelpa 'popwin)
(require 'helm-config)
(require 'helm-dash)
(require 'helm-spotify)
(require 'popwin)

(bind-key "C-c h" 'helm-mini)
(bind-key "C-c C-h m" 'helm-spotify)
(bind-key "C-c C-h d" 'helm-dash)
(bind-key "C-c C-h C-d" 'helm-dash-at-point)
(bind-key "C-c ! h" 'helm-flycheck)

(setq popwin:special-display-config
      (push helm-popwin
            popwin:special-display-config))

(setq helm-dash-browser-func 'eww)

;; Mark by keyword
(quelpa 'multiple-cursors)
(require 'multiple-cursors)
(bind-key "C-c C->" 'mc/mark-next-like-this)
(bind-key "C-c C-<" 'mc/mark-previous-like-this)
(bind-key "C-c c s" 'mc/mark-all-like-this)
(bind-key "M-<mouse-1>" 'mc/add-cursor-on-click)

;; Create new cursor by marking region with up / down arrows.
(quelpa 'rectangular-region-mode)
(require 'rectangular-region-mode)
(bind-key "C-c C-SPC" 'set-rectangular-region-anchor)

(quelpa 'expand-region)
(require 'expand-region)
(bind-key "C-=" 'er/expand-region)

;; Turn on winner mdoe by defautl
(winner-mode 1)

;; Transparently open compressed files
(auto-compression-mode t)

;; Global line numbers
(global-linum-mode 1)

(quelpa 'flex-isearch)
(require 'flex-isearch)
(global-flex-isearch-mode 1)

;; Remove test in active region if inserting text
(quelpa 'delsel)
(require 'delsel)
(delete-selection-mode 1)

;; Add parts of each file's directory to the buffer name if not unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(quelpa 'projectile)
(require 'projectile)
(projectile-global-mode)

;; Show matchin parentheses
(show-paren-mode 1)

(require 'eww)
(setq browse-url-browser-function 'eww)

;; Make tramp work nicely with sudo
(set-default 'tramp-default-proxies-alist (quote ((".*" "\\`root\\'" "/ssh:%h:"))))

(quelpa 'guide-key)
(require 'guide-key)
(diminish 'guide-key-mode)
(setq guide-key/guide-key-sequence '("C-x" "C-c"))
(setq guide-key/recursive-key-sequence-flag t)
(guide-key-mode 1)

(quelpa 'password-store)
(require 'password-store)

(quelpa 'comment-dwim-2)
(require 'comment-dwim-2)
(bind-key "M-;" 'comment-dwim-2)

(quelpa 'workgroups2)
(require 'workgroups2)
(setq wg-prefix-key (kbd "C-z"))
(setq wg-session-file "~/.emacs.d/.emacs_workgroups")
(workgroups-mode 1)
(diminish 'workgroups-mode)

(quelpa 'smart-forward)
(require 'smart-forward)
(bind-key "M-<up>" 'smart-up)
(bind-key "M-<down>" 'smart-down)
(bind-key "M-<left>" 'smart-left)
(bind-key "M-<right>" 'smart-right)

(quelpa 'diff-hl)
(require 'diff-hl)
(global-diff-hl-mode)

(quelpa 'dedicated)
(require 'dedicated)

(quelpa 'evil)
(require 'evil)
(bind-key "M-<f2>" 'evil-mode)
(setq evil-default-cursor t)

(quelpa 'grunt)
(require 'grunt)
(bind-key "C-c C-M-g" 'grunt-exec)

(quelpa 'htmlize)
(require 'htmlize)
;; Turn rainbow-delimiters off while doing htmlize
(defadvice htmlize-buffer-1 (around ome-htmlize-buffer-1 disable)
  (rainbow-delimiters-mode -1)
  ad-do-it
  (rainbow-delimiters-mode t))

(quelpa 'god-mode)
(require 'god-mode)
(bind-key "M-<f1>" 'god-mode-all)
(bind-key "C-x C-1" 'delete-other-windows)
(bind-key "C-x C-2" 'split-window-below)
(bind-key "C-x C-3" 'split-window-right)
(bind-key "C-x C-0" 'delete-window)

(defun setup-lisp-mode ()
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode))
(eval-after-load 'lisp-mode '(progn (setup-lisp-mode)))

(defun setup-elpy-mode ()
  (elpy-enable)
  ;; Use Flycheck instead of Flymake
  (when (require 'flycheck nil t)
    (remove-hook 'elpy-modules 'elpy-module-flymake)
    (add-hook 'elpy-mode-hook 'flycheck-mode))
  ;; jedi is great
  (setq elpy-rpc-backend "jedi"))
(eval-after-load 'python-mdoe '(progn (setup-elpy-mode)))

(quelpa 'web-mode)
(defun setup-web-mode ()
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4)

  ;; Set bindings for web-mode
  (define-key web-mode-map (kbd "<return>") 'newline-and-indent)
  (define-key web-mode-map (kbd "C-c w t") 'web-mode-element-wrap)
  (define-key web-mode-map (kbd "C-c C-v") 'browse-url-of-buffer))
(eval-after-load 'web-mode '(progn (setup-web-mode)))

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))

(defun setup-css-mode ()
  (add-hook 'css-mode-hook 'turn-on-css-eldoc)
  (add-hook 'css-mode-hook 'rainbow-mode)
  (autoload 'turn-on-css-eldoc "css-eldoc")
  (define-key css-mode-map (kbd "C-{") 'brace-ret-brace))
(eval-after-load 'css-mode '(progn (setup-css-mode)))

;; Use css-mode for compiled languages as well
(add-to-list 'auto-mode-alist '("\\.scss$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.sass$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.less" . css-mode))

;; Insert curly-braces
(defun brace-ret-brace ()
  (interactive)
  (insert "{") (newline-and-indent)
  (newline-and-indent)
  (insert "}") (indent-for-tab-command)
  (newline-and-indent) (newline-and-indent)
  (previous-line) (previous-line) (previous-line)
  (indent-for-tab-command))

(quelpa 'emmet-mode)
(defun setup-emmet-mode ()
  (setq emmet-indentation 2)
  (define-key emmet-mode-keymap "C-j" 'emmet-expand-line)
  (define-key emmet-mode-keymap "<C-return>" 'emmet-expand)
  ;; Remove purple <, >.
  (defadvice emmet-preview-accept (after expand-and-fontify activate)
    "Update the font-face after an emmet expantion."
    (font-lock-fontify-buffer)))
(eval-after-load 'emmet-mode '(progn (setup-emmet-mode)))

(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)

(defun skip-to-next-blank-line ()
  (interactive)
  (let ((inhibit-changing-match-data t))
    (skip-syntax-forward " >")
    (unless (search-forward-regexp "^\\s *$" nil t)
      (goto-char (point-max)))))

(defun skip-to-previous-blank-line ()
  (interactive)
  (let ((inhibit-changing-match-data t))
    (skip-syntax-backward " >")
    (unless (search-backward-regexp "^\\s *$" nil t)
      (goto-char (point-min)))))

(defun html-wrap-in-tag (beg end)
  (interactive "r")
  (let ((oneline? (= (line-number-at-pos beg) (line-number-at-pos end))))
    (deactivate-mark)
    (goto-char end)
    (unless oneline? (newline-and-indent))
    (insert "</div>")
    (goto-char beg)
    (insert "<div>")
    (unless oneline? (newline-and-indent))
    (indent-region beg (+ end 11))
    (goto-char (+ beg 4))))

(eval-after-load "sgml-mode"
  '(progn
     ;; don't include equal sign in symbols
     (modify-syntax-entry ?= "." html-mode-syntax-table)

     (define-key html-mode-map [remap forward-paragraph] 'skip-to-next-blank-line)
     (define-key html-mode-map [remap backward-paragraph] 'skip-to-previous-blank-line)
     ;;(define-key html-mode-map (kbd "C-c C-w") 'html-wrap-in-tag)
     (define-key html-mode-map (kbd "/") nil) ; no buggy matching of slashes
     (define-key html-mode-map (kbd "C-c C-d") 'ng-snip-show-docs-at-point)))

;; after deleting a tag, indent properly
(defadvice sgml-delete-tag (after reindent activate)
  (indent-region (point-min) (point-max)))

(defun setup-guiser ()
  (setq geiser-racket-binary "/usr/bin/racket")
  (setq geiser-guile-binary "/usr/bin/guile"))
(eval-after-load 'scheme '(progn (setup-geiser)))

(quelpa 'tex-site)
(defun setup-latex-mode ()
  (setq TeX-PDF-mode t)
  (setq LaTeX-command "latex -shell-escape"))
(eval-after-load 'latex-mode '(progn (setup-latex-mode)))

(quelpa 'org)
(quelpa 'ob-core)
(quelpa 'ox-md)
(quelpa 'ox-latex)
(quelpa 'yasnippet)

(defun setup-org-mode ()
  (message "setting up org mode")
  ;; Unbind from org-mode only
  (unbind-key "<C-S-up>" org-mode-map)
  (unbind-key "<C-S-down>" org-mode-map)
  ;; Bind new keys to org-mode only
  (bind-key "<s-up>" 'org-metaup org-mode-map)
  (bind-key "<s-down>" 'org-metadown org-mode-map)
  (bind-key "<s-left>" 'org-promote-subtree org-mode-map)
  (bind-key "<s-right>" 'org-demote-subtree org-mode-map)

  ;; Fontify org-mode code blocks
  (setq org-src-fontify-natively t)

  ;; Essential Settings
  (setq org-log-done 'time)
  (setq org-html-doctype "html5")
  (setq org-export-headline-levels 6)

  ;; Custom TODO keywords
  (setq org-todo-keywords
        '((sequence "TODO(t)" "PROGRESS(p!)" "REVIEW(r@/!)" "QA(q!)" "|" "DONE(d!)" "CANCELED(c@!)" "ROADBLOCK(o@!)")))

  ;; Set up latex
  (setq org-export-with-LaTeX-fragments t)
  (setq org-latex-create-formula-image-program 'imagemagick)

  ;; Add minted to the defaults packages to include when exporting.
  (add-to-list 'org-latex-packages-alist '("" "minted"))

  ;; Tell the latex export to use the minted package for source
  ;; code coloration.
  (setq org-latex-listings 'minted)

  ;; Let the exporter use the -shell-escape option to let latex
  ;; execute external programs.
  (setq org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

  ;; Set up babel source-block execution
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (haskell . t)
     (C . t)
     (js . t)))

  ;; fix org-mode + yasnippet conflicts:
  (add-hook 'org-mode-hook
            (lambda ()
              (require 'yasnippet)
              (make-variable-buffer-local 'yas/trigger-key)
              (setq yas/trigger-key [tab])
              (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
              (bind-key [tab] 'yas/next-field yas/keymap)))

  ;; Prevent Weird LaTeX class issue
  (unless (boundp 'org-latex-classes)
    (setq org-latex-classes nil))
  (add-to-list 'org-latex-classes
               '("per-file-class"
                 "\\documentclass{article}
                    [NO-DEFAULT-PACKAGES]
                    [EXTRA]"))

  (defun myorg-update-parent-cookie ()
    (when (equal major-mode 'org-mode)
      (save-excursion
        (ignore-errors
          (org-back-to-heading)
          (org-update-parent-todo-statistics)))))

  (defadvice org-kill-line (after fix-cookies activate)
    (myorg-update-parent-cookie))

  (defadvice kill-whole-line (after fix-cookies activate)
    (myorg-update-parent-cookie)))
(eval-after-load 'org '(progn (setup-org-mode)))

(quelpa 'dired+)
(require 'dired+)

(quelpa 'pandoc-mode)
(quelpa 'markdown-mode)

(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("README$" . markdown-mode))

(defun setup-markdown-mode ()
  (require 'pandoc-mode)
  (add-hook 'markdown-mode-hook 'turn-on-pandoc)
  (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)
  (setq markdown-command "pandoc --smart -f markdown -t html5")
  (setq markdown-css-path (file-truename (concat user-emacs-directory "themes/markdown.css")))
  (bind-key "C-c h" 'my-markdown-preview-file markdown-mode-map))
(eval-after-load 'markdown-mode '(progn (setup-markdown-mode)))

;;TODO: set up a save hook to auto-reload the converted markdown on save

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

(quelpa 'js2-mode)
(quelpa 'js2-refactor)
(quelpa 'js-comint)
(quelpa 'rainbow-delimiters)

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))

(defun setup-js2-mode ()
  ;; set up js2-refactor map
  (require 'js2-refactor)
  (js2r-add-keybindings-with-prefix "C-c C-r")

  ;; set up comint keys
  (require 'js-comint)
  (bind-key "C-c C-c e" 'js-send-last-sexp)
  (bind-key "C-c C-c x" 'js-send-last-sexp-and-go)
  (bind-key "C-c C-c b" 'js-send-buffer)
  (bind-key "C-c C-c C-b" 'js-send-buffer-and-go)
  (bind-key "C-c C-c n" 'js-send-region)
  (bind-key "C-c C-c C-n" 'js-send-region-and-go)
  (bind-key "C-c C-c l" 'js-load-file-and-go)



  ;; Set default js2 settings
  (setq-default js2-enter-indents-newline nil)
  (setq-default js2-bounce-indent-p t)
  (setq-default js2-global-externs '("module" "require" "jQuery" "$" "_" "buster" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON" "process" "setImmediate" "exports" "enum"))

  ;; Let Flycheck handle errors until js2 mode supports ES6
  (setq-default js2-show-parse-errors nil)
  (setq-default js2-strict-missing-semi-warning nil)
  (setq-default js2-strict-trailing-comma-warning t)

  (setq-default js-indent-level 2)
  (setq-default js2-strict-inconsistent-return-warning nil)
  (setq-default js2-include-node-externs t)
  (setq-default js2-include-jslint-globals t)
  (setq-default js2-basic-offset 2)

  ;; Set up js-comint for node
  (setq-default inferior-js-program-command "node")
  (setq-default inferior-js-mode-hook
        (lambda ()
          (ansi-color-for-comint-mode-on)
          (add-to-list
           'comint-preoutput-filter-functions
           (lambda (output)
             (replace-regexp-in-string "\033\\[[0-9]+[A-Z]" "" output)))))

  ;; Extra configuration that needs to be run on the js2-mode-hook
  (add-hook 'js2-mode-hook
            (lambda ()
              ;; Rainbows
              (require 'rainbow-delimiters)
              (rainbow-delimiters-mode)
              ;; Electric indent hates bouncies
              (electric-indent-mode -1)
              ;; Turn on tabs for JavaScript files
              ;;TODO: set up folder-local settings for this kind of customization
              (setq indent-tabs-mode 1)

              ;; Set default js2 settings
              (setq js2-enter-indents-newline nil)
              (setq js2-bounce-indent-p t)
              (setq js2-global-externs '("module" "require" "jQuery" "$" "_" "buster" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON" "process" "setImmediate" "exports" "enum"))

              ;; Let Flycheck handle errors until js2 mode supports ES6
              (setq js2-show-parse-errors nil)
              (setq js2-strict-missing-semi-warning nil)
              (setq js2-strict-trailing-comma-warning t)

              (setq js-indent-level 2)
              (setq js2-strict-inconsistent-return-warning nil)
              (setq js2-include-node-externs t)
              (setq js2-include-jslint-globals t)
              (setq js2-mode-indent-ignore-first-tab t)
              (setq js2-basic-offset 2))))
(eval-after-load 'js2-mode '(progn (setup-js2-mode)))

(quelpa 'magit)
(require 'magit)
(diminish 'magit-auto-revert-mode)
(bind-key "C-c g" 'magit-status)

(defun eshell/clear ()
  "04Dec2001 - sailor, to clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(quelpa 'erc)
(require 'erc)
(add-hook 'erc-mode-hook 'flyspell-mode)

(quelpa 'json-mode)
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))

(quelpa 'handlebars-mode)
(add-to-list 'auto-mode-alist '("\\.hbs$" . handlebars-mode))
(add-to-list 'auto-mode-alist '("\\.handlebards$" . handlebars-mode))

(add-to-list 'auto-mode-alist '("\\.svg$" . image-mode))

(quelpa 'coffee-mode)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))

(defun setup-coffee-mode ()
  (setq coffee-tab-width 2))
(eval-after-load 'coffee-mode '(progn (setup-coffee-mode)))

(add-to-list 'auto-mode-alist '("\\.service" . conf-mode))

(quelpa 'fish-mode)
(add-to-list 'auto-mode-alist '("\\.fish$" . fish-mode))

(add-to-list 'magic-mode-alist '("#!/usr/bin/sh" . sh-mode))
(add-to-list 'magic-mode-alist '("#!/usr/bin/bash" . sh-mode))
(add-to-list 'magic-mode-alist '("#!/usr/bin/zsh" . sh-mode))
(add-to-list 'magic-mode-alist '("#!/usr/bin/env sh" . sh-mode))
(add-to-list 'magic-mode-alist '("#!/usr/bin/env bash" . sh-mode))
(add-to-list 'magic-mode-alist '("#!/usr/bin/env zsh" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.sh" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.bash" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.zsh" . sh-mode))

(quelpa 'clojure-mode)
(quelpa '(queue :url "http://www.dr-qubit.org/download.php?file=predictive/queue.el" :fetcher url :version original))
(quelpa 'cider)
(quelpa 'clj-refactor)
(quelpa 'rainbow-delimiters)

(defun setup-clojure-mode ()
  (defadvice clojure-test-run-tests (before save-first activate)
    (save-buffer))

  (defadvice nrepl-load-current-buffer (before save-first activate)
    (save-buffer))

  ;; Cider, the Clojure IDE
  (require 'cider)

  ;; Define some cider keys
  (bind-key "C-," 'complete-symbol cider-repl-mode-map)
  (bind-key "C-," 'complete-symbol cider-mode-map)
  (bind-key "C-c C-q" 'nrepl-close cider-mode-map)
  (bind-key "C-c C-Q" 'cider-quit cider-mode-map)


  ;; Clojure refactorings
  (require 'clj-refactor)
  (cljr-add-keybindings-with-prefix "C-c C-r")


  (add-hook 'clojure-mode-hook
            (lambda ()
              (smartparens-strict-mode)

              ;; Turn on refactoring
              (clj-refactor-mode 1)


              ;; Set up some cider defaults
              ;; Hide nrepl buffers when switching buffers (switch to by prefixing with space)
              (setq nrepl-hide-special-buffers t)

              ;; Enable error buffer popping also in the REPL:
              (setq cider-repl-popup-stacktraces t)

              ;; Specify history file
              (setq cider-history-file "~/.emacs.d/nrepl-history")

              ;; auto-select the error buffer when it's displayed
              (setq cider-auto-select-error-buffer t)

              ;; Prevent the auto-display of the REPL buffer in a separate window after connection is established
              (setq cider-repl-pop-to-buffer-on-connect nil)

              ;; Enable eldoc in Clojure buffers
              (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

              ;; Use clojure font in repl
              (setq cider-repl-use-clojure-font-lock t)

              ;; Rainbows
              (require 'rainbow-delimiters)
              (rainbow-delimiters-mode))))
(eval-after-load 'org '(progn (setup-clojure-mode)))

(quelpa 'twittering-mode)
(require 'twittering-mode)

(setq twittering-cert-file "/etc/ssl/certs/ca-certificates.crt")
(setq twittering-use-master-password t)
(setq twittering-auth-method 'basic)

(quelpa 'go-mode)
(require 'go-mode)

(quelpa 'go-eldoc)
(quelpa 'go-direx)
(quelpa 'golint)
(quelpa 'go-projectile)
(quelpa 'company-go)
(quelpa 'go-errcheck)
(quelpa 'helm-go-package)

(defun setup-go-mode ()

  ;; Set default GOPATH
  (unless (getenv "GOPATH")
    (setenv "GOPATH" (concat (getenv "HOME") "/gocode")))

  ;; go-direx
  (require 'go-direx)
  (bind-key "C-c d" 'go-direx-pop-to-buffer go-mode-map)

  ;; golint
  (require 'golint)

  ;; go-projectile
  (require 'go-projectile)

  ;; completion
  (require 'company-go)

  ;; go-errcheck
  (require 'go-errcheck)

  ;; helm-go-package
  (require 'helm-go-package)
  (substitute-key-definition 'go-import-add 'helm-go-package go-mode-map)

  ;; oracle (source analyzer)
  (load (concat (getenv "GOPATH") "/src/code.google.com/p/go.tools/cmd/oracle/oracle.el"))

  ;; Local settings
  (add-hook 'go-mode-hook
            (lambda ()
              ;; Set go-eldoc
              (require 'go-eldoc-setup)
              (go-eldoc-setup)

              ;; go-direx popwin options
              (require 'popwin)
              (setq display-buffer-function 'popwin:display-buffer)
              (push '("^\*go-direx:" :regexp t :position left :width 0.4 :dedicated t :stick t)
                    popwin:special-display-config)

              ;; Add company-go to the completion list
              (set (make-local-variable 'company-backends) '(company-go))

              ;; oracle
              (go-oracle-mode))))
(eval-after-load 'org '(progn (setup-go-mode)))

>>>>>>> d562c2a73e2c9a125bf0285a7187d5dcc7e0afa3
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))
(global-set-key (kbd "C-x x e") 'eval-and-replace)

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single
line instead."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-beginning-position 2)))))

(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single
line instead."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position) (line-beginning-position 2)))))

;; See http://bzg.fr/emacs-hide-mode-line.html
(defvar-local hidden-mode-line-mode nil)
(defvar-local hide-mode-line nil)

(define-minor-mode hidden-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global nil
  :variable hidden-mode-line-mode
  :group 'editing-basics
  (if hidden-mode-line-mode
      (setq hide-mode-line mode-line-format
            mode-line-format nil)
    (setq mode-line-format hide-mode-line
          hide-mode-line nil))
  (force-mode-line-update)
  ;; Apparently force-mode-line-update is not always enough to
  ;; redisplay the mode-line
  (redraw-display)
  (when (and (called-interactively-p 'interactive)
             hidden-mode-line-mode)
    (run-with-idle-timer
     0 nil 'message
     (concat "Hidden Mode Line Mode enabled.  "
             "Use M-x hidden-mode-line-mode to make the mode-line appear."))))
  ;; If you want to hide the mode-line in all new buffers
  ;; (add-hook 'after-change-major-mode-hook 'hidden-mode-line-mode))

(defun unmark-flyspell-in-buffer ()
       (interactive)
       (flyspell-delete-all-overlays))

(defun create-scratch-buffer nil
  "create a new scratch buffer to work in. (could be *scratch* - *scratchX*)"
  (interactive)
  (let ((n 0)
        bufname)
    (while (progn
             (setq bufname (concat "*scratch"
                                   (if (= n 0) "" (int-to-string n))
                                   "*"))
             (setq n (1+ n))
             (get-buffer bufname)))
    (switch-to-buffer (get-buffer-create bufname))
    (lisp-interaction-mode)))

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))
(global-set-key (kbd "C-c @ u") 'untabify-buffer)

(defun tabify-buffer ()
  (interactive)
  (tabify (point-min) (point-max)))
(global-set-key (kbd "C-c @ t") 'tabify-buffer)

(defun indnet-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))
(global-set-key (kbd "C-c @ i") 'indent-buffer)

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (untabify-buffer)
  (delete-trailing-whitespace)
  (indent-buffer))

(defun cleanup-buffer-boring ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (delete-trailing-whitespace))

(defun auto-buffer-cleanup ()
  "Turn on buffer cleanup"
  (interactive)
  (stop-auto-buffer-cleanup)
  (add-hook 'before-save-hook 'cleanup-buffer))
(defun auto-buffer-cleanup-boring ()
  "Turn on buffer cleanup"
  (interactive)
  (stop-auto-buffer-cleanup)
  (add-hook 'before-save-hook 'cleanup-buffer-boring))
(defun stop-auto-buffer-cleanup ()
  "Turn on buffer cleanup"
  (interactive)
  (remove-hook 'before-save-hook 'cleanup-buffer)
  (remove-hook 'before-save-hook 'cleanup-buffer-boring))
(global-set-key (kbd "C-c @ Y") 'auto-buffer-cleanup)
(global-set-key (kbd "C-c @ y") 'auto-buffer-cleanup-boring)
(global-set-key (kbd "C-c @ n") 'stop-auto-buffer-claenup)
(auto-buffer-cleanup-boring)

;; Mimic vim's "w" command
(defun forward-word-to-beginning (&optional n)
  "Move point forward n words and place cursor at the beginning."
  (interactive "p")
  (let (myword)
    (setq myword
      (if (and transient-mark-mode mark-active)
        (buffer-substring-no-properties (region-beginning) (region-end))
        (thing-at-point 'symbol)))
    (if (not (eq myword nil))
      (forward-word n))
    (forward-word n)
    (backward-word n)))
(global-set-key (kbd "M-f") 'forward-word-to-beginning)
;; Remap old forward word
(global-set-key (kbd "M-F") 'forward-word)

;; Hide DOS line endings
(defun remove-dos-eol ()
        (interactive)
        (setq buffer-display-table (make-display-table))
        (aset buffer-display-table ?\^M []))

(defun other-window-backwards (count)
  (itneractive "p")
  (otehr-window (- 0 count)))
(global-set-key (kbd "C-x p") 'other-window-backwards)

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))
(global-set-key (kbd "C-c @ r") 'rename-current-buffer-file)

(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))
(global-set-key (kbd "C-c @ d") 'rename-current-buffer-file)

(defun insert-date ()
  "Insert current date yyyy-mm-dd H:M:S."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %T")))

(require 'server)
(unless (server-running-p)
  (server-start))

(quelpa 'edit-server)
(when (daemonp)
  (edit-server-start))
