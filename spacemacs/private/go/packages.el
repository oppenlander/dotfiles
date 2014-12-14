(defvar go-packages
  '(
    go-mode
    go-eldoc
    go-direx
    golint
    go-projectile
    go-errcheck
    helm-go-package
    go-autocomplete
    go-stacktracer
    gotest
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar go-excluded-packages '()
  "List of packages to exclude.")

(defun go/init-go-mode ()
  (use-package go-mode
    :defer t
    :init
    (progn
      (unless (getenv "GOPATH")
        (setenv "GOPATH" (concat (getenv "HOME") "/gocode")))
      )
    :config
    (progn
      
      ;; Try to load teh Oracle
      (let (oracle-path (concat (getenv "GOPATH") "/src/code.google.com/p/go.tools/cmd/oracle/oracle.el"))
        (if (file-exists-p oracle-path) (load oracle-path)))    
      )
    ))

(defun go/init-go-eldoc ()
  (use-package go-eldoc
    :defer t
    :init (add-hook 'go-mode-hook (lambda () (require 'go-eldoc-setup) (go-eldoc-setup)))))

(defun go/init-go-direx ()
  (use-package go-direx 
    :defer t
    :init (eval-after-load 'go-mode 
            '(progn
               (require 'go-direx)
               (evil-loader/set-key-for-mode 'go-mode "md")))))

(defun go/init-golint ()
  (use-package golint
    :defer t
    :init (eval-after-load 'go-mode
            '(require 'golint))))

(defun go/init-go-projectile ()
  (use-package go-projectile
    :defer t
    :init (eval-after-load 'go-mode
            '(require 'go-projectile))))

(defun go/init-go-errcheck ()
  (use-package go-errcheck
    :defer t
    :init (eval-after-load 'go-mode
            '(require 'go-errcheck))))

(defun go/init-helm-go-package ()
  (use-package helm-go-package
    :defer t
    :init (eval-after-load 'go-mode
            '(progn
               (require 'helm-go-package)
               (substitute-key-definition 'go-import-add 'helm-go-package go-mode-map)))))

(defun go/init-go-autocomplete ()
  (use-package go-autocomplete
    :defer t
    :init (eval-after-load 'go-mode
            '(require 'go-autocomplete))))

(defun go/init-go-stacktracer ()
  (use-package go-stacktracer
    :defer t
    :init (eval-after-load 'go-mode
            '(require 'go-stacktracer))))

(defun go/init-gotest ()
  (use-package gotest
    :defer t
    :init (eval-after-load 'go-mode
            '(require go-test))))
