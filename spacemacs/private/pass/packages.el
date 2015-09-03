;;; packages.el --- pass Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Andrew Oppenlander <andrew.oppenlander@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq pass-packages '(password-store))

(setq pass-excluded-packages '())

(defun pass/init-password-store ()
  (use-package password-store
    :defer t
    :init
    (evil-leader/set-key
      "Psy" 'password-store-copy
      "Psg" 'password-store-generate
      "Psi" 'password-store-insert
      "Psc" 'password-store-edit
      "Psr" 'password-store-rename
      "Psd" 'password-store-remove
      "PsD" 'password-store-clear
      "PsI" 'password-store-init
      "Psw" 'password-store-url)))
