;;; rename-modeline.el --- provides a macro to rename a mode in the modeline
;;; -*- coding: utf-8 -*-
;; Filename rename-modeline.el
;; Description: Provides a macro to rename a mode in the modeline
;; Author: Mangers

;;; Commentary:

;;; Code:
(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

(provide 'rename-modeline)
;;; rename-modeline ends here