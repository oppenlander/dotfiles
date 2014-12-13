(defvar idris-packages
  '(
    ;; package idriss go here
    idris-mode
    helm-idris
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defun idris/init-idris-mode ()
  (use-package idris-mode
    :defer t
    :init
    (set-default 'idris-mode-hook
                 '(
                   turn-on-idris-simple-indent
                   idris-enable-clickable-imports
                   turn-on-eldoc-mode
                   idris-define-loading-keys
                   idris-define-docs-keys
                   idris-define-editing-keys
                   idris-define-general-keys
                   idris-define-ipkg-keys
                   idris-define-ipkg-opening-keys
                   ))
    :config
    (progn
      (spacemacs/declare-prefix-for-mode 'idris-mdoe "m" "idris mode")

      (evil-leader/set-key-for-mode 'idris-mode
        "mr" 'idris-load-file
        "mt" 'idris-type-at-point
        "ma" 'idris-add-clause
        "mc" 'idris-case-split
        "mw" 'idris-make-with-block
        "mm" 'idris-add-missing
        "mp" 'idris-proof-search
        "md" 'idris-docs-at-point))))

(defun idris/init-helm-idris ()
  (use-package helm-idris
    :defer t
    :config (evil-leader/set-key-for-mode 'idris-mode "mh" 'helm-idris)))

