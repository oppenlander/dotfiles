(defvar oppenlander-packages
  '(
    ;; package usrs go here
    jade-mode
    password-store
    grunt
    vlf
    edit-server
    edit-server-htmlize
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar oppenlander-excluded-packages '()
  "List of packages to exclude.")


(defun oppenlander/init-jade-mode ()
  "Jade Templating language"
  (use-package jade-mode :defer t))

(defun oppenlander/init-password-store ()
  "Password Store that uses pass"
  (use-package password-store
    :defer t
    :init
    (evil-leader/set-key
      "opc" 'password-store-edit
      "opy" 'password-store-copy
      "opi" 'password-store-insert
      "opD" 'password-store-clear
      "opI" 'password-store-init
      "opg" 'password-store-generate
      "opd" 'password-store-remove
      "opC" 'password-store-rename
      "opj" 'password-store-url)))

(defun oppenlander/init-grunt ()
  "Grunt Task Runner"
  (use-package grunt :defer t :init (evil-leader/set-key "aG" 'grunt-exec)))

(defun oppenlander/init-vlf ()
  "Very Large Files"
  (use-package vlf :config (require 'vlf-integrate)))

;; (defun oppenlander/init-edit-server ()
;;   "Edit Server used with the 'Edit With Emacs' plugin"
;;   (use-package edit-server
;;     :config
;;     (progn
;;       (require 'server)
;;       (when (server-running-p)
;;         (edit-server-start)))))

;; (defun oppenladner/init-edit-server-htmlize ()
;;   "Will try to htmlize certain plain text blocks that come in, like from GMail"
;;   (use-package edit-server-htmlize
;;     :init
;;     (progn
;;       (add-hook 'edit-server-start-hook 'edit-server-maybe-dehtmlize-buffer)
;;       (add-hook 'edit-server-done-hook 'edit-server-maybe-htmlize-buffer))))

;; For each package, define a function usr/init-<package-usr>
;;
;; (defun usr/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
