;;; package --- Summary

;;; Commentary:

;;; Code:
(require 'helm-config)

(require 'helm-dash)
(setq helm-dash-browser-func 'w3m)

(global-set-key (kbd "C-c h") 'helm-mini)

(require 'flycheck)
(eval-after-load 'flycheck
  '(define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck))

;; Google
(global-set-key (kbd "C-c C-h g") 'helm-google)

;; Projectile
;(require 'helm-projectile)
;(helm-projectile-on)

;; Spotify
(global-set-key (kbd "C-c C-h m") 'helm-spotify)


(provide 'setup-helm-mode)
;;;
