;; Interactively Do Things

(require 'ido)
(ido-mode t)

(require 'flx-ido)
(flx-ido-mode 1)
(setq ido-use-faces nil)

(require 'ido-vertical-mode)
(ido-vertical-mode)

;; Always rescan buffer for imenu
(set-default 'imenu-auto-rescan t)

(add-to-list 'ido-ignore-directories "target")
(add-to-list 'ido-ignore-directories "node_modules")

(require 'ido-at-point)
(ido-at-point-mode)

;; Use ido everywhere
(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

(provide 'setup-ido)
