;; Emacs 24 init.el

;; NOTE: The name of the Org files is important!  When a file gets tangled,
;; it gets the same base name as the Org file.  Thus, tangling Emacs Lisp from
;; a file `init.org` would generate `init.el`, obliterating this file in the
;; process. So your config org file should not be named "init.org".

(require 'ob-tangle)
(setq debug-on-error t)
(org-babel-load-file
 (expand-file-name "emacs-init.org" user-emacs-directory))
