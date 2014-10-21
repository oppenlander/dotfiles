(require 'setup-lisp-mode)
(require 'clojure-mode)
(require 'clojure-test-mode)
(require 'cider)

(defadvice clojure-test-run-tests (before save-first activate)
  (save-buffer))

;; Enable eldoc
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

;; Hide *nrepl-connection* and *nrepl-server* buffers
(setq nrepl-hide-special-buffers t)



(provide 'setup-clojure-mode)
