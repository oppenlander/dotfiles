(defvar go-packages
  '(
    go-mode
    go-eldoc
    go-direx
    golint
    go-projectile
    go-errcheck
    helm-go-package
    go-stacktracer
    company-go
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

      ;; gofmt on save
      (add-hook 'go-mode-hook '(add-hook 'before-save-hook #'gofmt-before-save))
      
      ;; Try to load the Oracle
      (let ((oracle-path (concat (getenv "GOPATH") "/src/code.google.com/p/go.tools/cmd/oracle/oracle.el")))
        (if (file-exists-p oracle-path) (load oracle-path)))    
      )
    ))

(defun go/init-go-eldoc ()
  (use-package go-eldoc
    :defer t
     :init (eval-after-load 'go-mode 
            '(progn
               (require 'go-eldoc)
               (go-eldoc-setup)))))

(defun go/init-go-direx ()
  (use-package go-direx 
    :defer t
    :init (eval-after-load 'go-mode 
            '(progn
               (require 'go-direx)
               (evil-leader/set-key-for-mode 'go-mode "md" 'go-direx-pop-to-buffer)))))

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

(defun go/init-company-go ()
  (use-package company-go
    :defer t
    :init (eval-after-load 'go-mode
            '(progn
               (require 'company)
               (require 'company-go)

               (add-hook 'go-mode-hook (lambda ()
                                         (set (make-local-variable 'company-backends) '(company-go))
                                         (company-mode)))))))

(defun go/init-go-stacktracer ()
  (use-package go-stacktracer
    :defer t
    :init (eval-after-load 'go-mode
            '(require 'go-stacktracer))))
