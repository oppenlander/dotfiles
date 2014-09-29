;; Clean up buffer after bullshit
(defun cleanup-buffer-boring ()
	(interactive)
	(delete-trailing-whitespace)
	(set-buffer-file-coding-system 'utf-8))
(defun cleanup-buffer-safe ()
	"Perform a safe buffer cleanup"
	(interactive)
	(untabify (point-min) (point-max))
	(cleanup-buffer-boring))
(defun cleanup-buffer ()
	"Unsafe buffer cleanup"
	(interactive)
	(cleanup-buffer-safe)
	(indent-region (point-min) (point-max)))
(add-hook 'before-save-hook 'cleanup-buffer-boring)
(defun start-buffer-cleanup ()
  "Turn on buffer cleanup"
  (interactive)
  (add-hook 'before-save-hook 'cleanup-buffer-boring))
(defun stop-buffer-cleanup ()
  "Turn on buffer cleanup"
  (interactive)
  (remove-hook 'before-save-hook 'cleanup-buffer-boring))
(define-key global-map (kbd "C-c @ y") 'start-buffer-cleanup)
(define-key global-map (kbd "C-c @ n") 'stop-buffer-cleanup)
(start-buffer-cleanup)

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
(global-set-key (kbd "M-F") 'forward-word)
(global-set-key (kbd "M-f") 'forward-word-to-beginning)

;; Don't add new line at the end of every file
(setq mode-require-final-newline nil)
(setq require-final-newline nil)

;; Global line numbers
(global-linum-mode 1)

;; Allow pasting selection outside Emacs
(setq x-select-enable-clipboard t)

;; Move deleted fiels to trash
(setq delete-by-moving-to-trash t)

;; No shift marking/selecting
(setq shift-select-mode nil)

;; Transparently open compressed files
(auto-compression-mode t)

;; Answer yes/no with y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;; UTF-8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Remove test in active region if inserting text
(require 'delsel)
(delete-selection-mode 1)

;; Line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; Undo/redo window configurations
(winner-mode 1)

;; Represent undo-history as an actual tree (visualize with C-x u)
(setq undo-tree-mode-lighter "")
(require 'undo-tree)
(global-undo-tree-mode)

;; Browse kill ring
;; errors and won't quit
																				;(require 'browse-kill-ring)
																				;(browse-kill-ring-default-keybindings)

;; Add parts of each file's directory to the buffer name if not unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)


;; Diminish modeline
(require 'diminish)

;; Auto-complete
(require 'auto-complete)
(define-globalized-minor-mode real-global-auto-complete-mode
	auto-complete-mode (lambda ()
											 (if (not (minibufferp (current-buffer)))
													 (auto-complete-mode 1))
											 ))
(real-global-auto-complete-mode t)
(diminish 'auto-complete-mode)

;; Hide DOS line endings
(defun remove-dos-eol ()
	(interactive)
	(setq buffer-display-table (make-display-table))
	(aset buffer-display-table ?\^M []))

;; Projectile
(require 'projectile)
(projectile-global-mode)

;; Set global tab width
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default c-basic-offset 2)
(setq-default octave-block-offset 2)

;; Recent files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; Goto last change
(global-set-key "\C-x\C-\\" 'goto-last-change)

;; Show matchin parentheses
(show-paren-mode 1)

;; Cursor Settings
(blink-cursor-mode 1)
(setq default-frame-alist '((cursor-color . "white")))

;; Autocomplete
(define-key ac-mode-map (kbd "C-TAB") 'auto-complete)

;; Same-frame Speedbar
(require 'sr-speedbar)
(global-set-key (kbd "s-s") 'sr-speedbar-toggle)
(setq speedbar-show-unknown-files t)
(setq speedbar-smart-directory-expand-flag t)
(setq speedbar-use-images nil)

;; Rainbows
(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode)

;; Ack
(require 'ack-and-a-half)
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

;; Move backwards through windows
(global-set-key "\C-xp" (lambda ()
													(interactive)
													(other-window -1)))

;; Popwin
(require 'popwin)
(popwin-mode 1)

;; Direx
(require 'direx)
(push '(direx:direx-mode :position left :width 25 :dedicated t)
			popwin:special-display-config)
(global-set-key (kbd "C-x C-j") 'direx:jump-to-directory-other-window)

;; Multi-term
(require 'multi-term)
(setq multi-term-program "/usr/bin/fish")

;; Make tramp work nicely with sudo
(set-default 'tramp-default-proxies-alist (quote ((".*" "\\`root\\'" "/ssh:%h:"))))

;; Smartparens
(require 'smartparens-config)
(smartparens-global-mode 1)
(show-smartparens-global-mode 1)
(diminish 'smartparens-mode)
(global-set-key (kbd "C-M-k") 'sp-kill-sexp)

;; Cleaning modeline
(defmacro rename-modeline (package-name mode new-name)
	`(eval-after-load ,package-name
		 '(defadvice ,mode (after rename-modeline activate)
				(setq mode-name ,new-name))))

;; eww it up
(require 'eww)
(require 'w3m)
(setq browse-url-browser-function 'w3m-browse-url)

;; my magit
(require 'magit)
(global-set-key (kbd "C-c g") 'magit-status)

;; multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; guide key
(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x" "C-c"))
(setq guide-key/recursive-key-sequence-flag t)
(guide-key-mode 1)
(diminish 'guide-key-mode)

;; visual regexp (with steroids!)
(require 'visual-regexp-steroids)
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)
(define-key global-map (kbd "C-c m") 'vr/mc-mark)
(define-key esc-map (kbd "C-r") 'vr/isearch-backward)
(define-key esc-map (kbd "C-s") 'vr/isearch-forward)

;; Flycheck
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(diminish 'flycheck-mode)

;; Evil!
(require 'evil)
(global-set-key [f5] 'evil-mode)

;; New Scratch Buffer
(defun new-empty-buffer ()
  (interactive)
  (let ((n 0)
        new-buf)
    (while (progn
             (setq new-buf (concat "-untitled-"
                                       (if (= n 0) "0" (int-to-string n))
                                       "-"))
             (incf n)
             (get-buffer new-buf)))
    (switch-to-buffer (get-buffer-create new-buf))
    (text-mode)))
(define-key global-map (kbd "C-c C-n") 'new-empty-buffer)

;; ACE!
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; Reverse join
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))

;; Pass(word-store)
(require 'password-store)

;; Comment
(require 'comment-dwim-2)
(global-set-key (kbd "M-;") 'comment-dwim-2)

;; sudo-edit
(require 'sudo-edit)

;; math-at-point
(require 'math-at-point)

;; move-line C-s-UP / C-s-DOWN
(require 'move-line)

;; Workgroups2 C-c w
(require 'workgroups2)
(setq wg-prefix-key (kbd "C-c w"))
(setq wg-session-file "~/.emacs.d/.emacs_workgroups")
(workgroups-mode 1)

;; Powerline (advanced mode-line)
(require 'powerline)
(powerline-default-theme)

(provide 'my-misc)
