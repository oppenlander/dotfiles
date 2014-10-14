(require 'company)


(setq company-idle-delay 0.3)
(setq company-tooltip-limit 20)
(setq company-minimum-prefix-length 2)

(global-company-mode 1)

;; (add-to-list 'company-backends 'company-dabbrev t)

(require 'company-tern)
(add-to-list 'company-backends 'company-tern t)
; (add-to-list 'company-backends 'company-ispell t)
;; (add-to-list 'company-backends 'company-files t)


(provide 'setup-company-mode)