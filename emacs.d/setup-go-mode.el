
;; flymake
(require 'flymake-go)

;; Autocomplete (requires github.com/nsf/gocode)
(require 'go-autocomplete)

;; go-direx
(require 'go-direx)
(define-key go-mode-map (kbd "C-c C-j") 'go-direx-pop-to-buffer)
(defvar popwin:special-display-config-backup popwin:special-display-config)
(setq display-buffer-function 'popwin:display-buffer)
(push '("^\*go-direx:" :regexp t :position left :width 0.4 :dedicated t :stick t)
			popwin:special-display-config)

(provide 'setup-go-mode)
