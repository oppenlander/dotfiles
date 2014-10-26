
(require 'cask "/usr/share/cask/cask.el")
(cask-initialize)

(require 'req-package)

(add-to-list 'load-path (concat user-emacs-directory "packages/"))

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Start off with some sanity.
(req-package better-defaults)

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
(req-package saveplace)
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
(bind-key "C-x r q" 'save-buffers-kill-terminal)
(bind-key "C-x C-c" 'delete-frame)

;; Home and End Keys:
(bind-key "<home>" 'move-beginning-of-line)
(bind-key "<end>" 'move-end-of-line)

;; Joins Lines into one
(bind-key  "M-j" '(lambda () (interactive) (join-line -1)))

;; Kill line from the left
(bind-key "<s-backspace>" '(lambda () (interactive) (kill-line 0)))

;; Quickly jump to last change
(bind-key "C-x C-\\" 'goto-last-change)

;; Clean up whitespace
(bind-key "C-c @ c" 'delete-trailing-whitespace)

;; Replace string
(bind-key "C-c r" 'replace-string)

(dolist (keys '("<M-up>" "<M-down>" "<s-left>" "<s-right>"
                "s-c" "s-v" "s-x" "s-v" "s-q" "s-s" "s-w"
                "s-a" "s-o" "s-n" "s-p" "s-k" "s-u" "s-m"
                "s-f" "s-z" "s-g" "s-d" "s-," "s-:" "s-e"
                "s-t" "C-z"))
  (global-unset-key (kbd keys)))

(set-face-attribute 'default nil :family "DejaVu Sans Mono" :height 98)

(setq custom-theme-directory (concat user-emacs-directory "themes/"))

(load-theme 'smyx-custom t)

(req-package pretty-mode
  :config
  (global-pretty-mode 1))

(setq display-time-day-and-date t
                display-time-format "%a %b %d %R"
                display-time-interval 60
                display-time-default-load-average nil)
             (display-time)

(req-package powerline
             :config
             (powerline-default-theme))

(defun add-operator-hl ()
  (font-lock-add-keywords
   nil
   '(("\s[-]\s\\|\s[/]\s\\|[%]\\|[+]\\|[*]\\|[!=]\\|[/=]\\|[<=]\\|[>=]" . font-lock-keyword-face))))
;; prog-mode applies to all programming modes
(add-hook 'prog-mode-hook 'add-operator-hl)

(setq redisplay-dont-pause t)

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))
(bind-key "C-x x e" 'eval-and-replace)

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
(bind-key "C-c @ u" 'untabify-buffer)

(defun tabify-buffer ()
  (interactive)
  (tabify (point-min) (point-max)))
(bind-key "C-c @ t" 'tabify-buffer)

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))
(bind-key "C-c @ i" indent-buffer)

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
  (stop-buffer-cleanup)
  (add-hook 'before-save-hook 'cleanup-buffer))
(defun auto-buffer-cleanup-boring ()
  "Turn on buffer cleanup"
  (interactive)
  (stop-buffer-cleanup)
  (add-hook 'before-save-hook 'cleanup-buffer-boring))
(defun stop-auto-buffer-cleanup ()
  "Turn on buffer cleanup"
  (interactive)
  (remove-hook 'before-save-hook 'cleanup-buffer)
  (remove-hook 'before-save-hook 'cleanup-buffer-boring))
(bind-key "C-c @ Y" auto-buffer-cleanup)
(bind-key "C-c @ y" auto-buffer-cleanup-boring)
(bind-key "C-c @ n" stop-auto-buffer-claenup)
(start-buffer-cleanup-boring)

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
(bind-key "M-f" 'forward-word-to-beginning)
;; Remap old forward word
(bind-key "M-F" 'forward-word)

;; Hide DOS line endings
(defun remove-dos-eol ()
        (interactive)
        (setq buffer-display-table (make-display-table))
        (aset buffer-display-table ?\^M []))

(defun other-window-backwards (count)
  (itneractive "p")
  (otehr-window (- 0 count)))
(bind-key "C-x p" 'other-window-backwards)

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
(bind-key "C-c @ r" 'rename-current-buffer-file)

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
(bind-key "C-c @ d" 'rename-current-buffer-file)

(defun insert-date ()
  "Insert current date yyyy-mm-dd H:M:S."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %T")))

(defun js-method-p ()
  (save-excursion
    (word-search-backward "function")
    (looking-back ": ")))

(defun js-function-declaration-p ()
  (save-excursion
    (word-search-backward "function")
    (looking-back "^\\s *")))

(defun snippet--function-punctuation ()
  (if (js-method-p)
      (when (not (looking-at "[ \n\t\r]*[},]"))
        (insert ","))
    (unless (js-function-declaration-p)
      (if (looking-at "$") (insert ";")))))

(defun snippet--function-name ()
  (if (js-function-declaration-p) "name" ""))

;;; clojure
(defun snippet--clojure-namespace-from-buffer-file-name ()
  (replace-regexp-in-string "_" "-"
   (replace-regexp-in-string "/" "."
    (chop-prefix "test/"
    (chop-prefix "src/"
    (chop-suffix ".clj"
     (substring (buffer-file-name) (length eproject-root))))))))

(defun snippet--clojure-namespace-under-test ()
  (replace-regexp-in-string "-test" "" (snippet--clojure-namespace-from-buffer-file-name)))

;; snippet-helper-helpers
(defun chop-suffix (suffix s)
  "Remove string 'suffix' if it is at end of string 's'"
  (let ((pos (- (length suffix))))
    (if (and (>= (length s) (length suffix))
             (string= suffix (substring s pos)))
        (substring s 0 pos)
      s)))

(defun chop-prefix (prefix s)
  "Remove string 'prefix' if it is at start of string 's'"
  (let ((pos (length prefix)))
    (if (and (>= (length s) (length prefix))
             (string= prefix (substring s 0 pos)))
        (substring s pos)
      s)))

(req-package server
             :config
             (unless (server-running-p)
               (server-start)))

(req-package edit-server
             :config
             (when (daemonp)
               (edit-server-start)))

(req-package-finish)
