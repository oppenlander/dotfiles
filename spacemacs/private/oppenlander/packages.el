(defvar oppenlander-packages
  '(
    ;; package usrs go here
    css-mode
    grunt
    jade-mode
    js2-mode
    ;; password-store
    pretty-mode
    rainbow-delimiters
    restclient
    vlf
    ws-butler
    vimrc-mode
    org
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar oppenlander-excluded-packages '()
  "List of packages to exclude.")


(defun oppenlander/init-jade-mode ()
  "Jade Templating language"
  (use-package jade-mode :defer t))

(defun oppenlander/init-org ()
  (use-package org
    :defer t
    :init
    (progn
      (setq org-todo-keywords
            '((sequence "UNASSIGNED(u)" "DREW(D)" "HAZLETT(H)" "ROSS(R)" "YOUYOU(Y)" "MICHAEL(M)" "CHRIS(C)" "ERIN(E)" "INTARN(I)" "ROB(R)" "ANDY(O)" "KEVIN(K)" "MO(M)" "SHERMAN(S)" "PATRICK(P)" "JAMES(J)" "BONNIE(B)" "DYLAN(N)" "OTHER(?@)" "|" "DONE(d)")
              (sequence "NEW(n)" "WAIT(w)" "TODO(t@/@)" "PROGRESS(p@)" "REVIEW(r!)" "QA(q!)" "|" "COMPLETE(c!)" "CANCEL(d@)" "ROADBLOCK(b@)")))
      )
    ))

(defun oppenlander/init-grunt ()
  "Grunt Task Runner"
  (use-package grunt
    :defer t
    :init (evil-leader/set-key "aG" 'grunt-exec)))

(defun oppenlander/init-vlf ()
  "Very Large Files"
  (use-package vlf :config (require 'vlf-setup)))

(defun oppenlander/init-pretty-mode ()
  (use-package pretty-mode
    :defer t
    :init
    (progn
      (defun oppenlander/toggle-pretty-mode ()
        "Lazy load/toggle pretty mode"
        (interactive)
        (if (bound-and-true-p pretty-mode)
            (global-pretty-mode -1)
          (global-pretty-mode 1)))
      (evil-leader/set-key "tp" 'oppenlander/toggle-pretty-mode))))

(defun oppenlander/post-init-js2-mode ()
  (use-package js2-mode
    :defer t
    :config
    (progn
      ;; Set default js2 settings
      (setq-default js2-enter-indents-newline nil)
      (setq-default js2-bounce-indent-p t)
      (setq-default js2-global-externs '("module" "require" "jQuery" "$" "_" "buster" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON" "process" "setImmediate" "exports" "enum" "it" "describe"))

      ;; Let Flycheck handle errors until js2 mode supports ES6
      (setq-default js2-show-parse-errors nil)
      (setq-default js2-strict-missing-semi-warning nil)
      (setq-default js2-strict-trailing-comma-warning t)

      (setq-default js-indent-level 2)
      (setq-default js2-strict-inconsistent-return-warning nil)
      (setq-default js2-include-node-externs t)
      (setq-default js2-include-jslint-globals t)
      (setq-default js2-basic-offset 2)
      (setq-default js2-indent-switch-body t)

      (defun oppenlander/js2-mode-hook ()
        ;; Electric indnets hate bouncies
        (electric-indent-mode -1))
      (add-hook 'js2-mode-hook 'oppenlander/js2-mode-hook)
      )))

(defun oppenlander/post-init-css-mode ()
  (use-package css-mode
    :defer t
    :config (setq-default css-indent-offset 2)))

(defun oppenlander/init-rainbow-delimiters ()
  (use-package rainbow-delimiters
    :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))

(defun oppenlander/init-flycheck ()
  (use-package flycheck
    :config (global-flycheck-mode)))

(defun oppenladner/init-ws-butler ()
  (use-package ws-butler
    :init (add-hook 'prog-mode-hook 'ws-butler-mode)))

(defun oppenlander/init-restclient ()
  (use-package restclient
    :defer t
    :config
    (progn
      (evil-leader/set-key-for-mode 'restclient-mode "c" 'restclient-http-send-current)
      (evil-leader/set-key-for-mode 'restclient-mode "r" 'restclient-http-send-current-raw)
      (evil-leader/set-key-for-mode 'restclient-mode "v" 'restclient-http-send-current-stay-in-window)
      (evil-leader/set-key-for-mode 'restclient-mode "j" 'restclient-jump-next)
      (evil-leader/set-key-for-mode 'restclient-mode "k" 'restclient-jump-previous)
      (evil-leader/set-key-for-mode 'restclient-mode "." 'restclient-mark-current)
      (evil-leader/set-key-for-mode 'restclient-mode "y" 'restclient-copy-curl-command))))
