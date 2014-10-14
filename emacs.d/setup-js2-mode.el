(setq-default js2-enter-indents-newline t)
(setq-default js2-bounce-indent-p t)
(setq-default js2-global-externs '("module" "require" "jQuery" "$" "_" "buster" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON" "process" "setImmediate" "exports" "enum"))

;; Let Flycheck handle errors until js2 mode supports ES6
(setq-default js2-show-parse-errors nil)
(setq-default js2-strict-missing-semi-warning nil)
(setq-default js2-strict-trailing-comma-warning t)

(setq-default js2-basic-offset 2)
(setq-default js-indent-level 2)
(setq-default js2-strict-inconsistent-return-warning nil)
(setq-default js2-include-node-externs t)
(setq-default js2-indent-ignore-first-tab t)

;; Jade/Stylus
(require 'sws-mode)
(require 'jade-mode)

;; Handlebars
(require 'handlebars-mode)

;; Less
(require 'less-css-mode)
(add-hook 'less-css-mode-hook '(lambda () (setq indent-tabs-mode 1)))

;; Autocomplete js2
(require 'auto-complete)
(add-hook 'js2-mode-hook 'ac-js2-mode)
(setq-default ac-js2-evaluate-calls t)

;; Grunt
(require 'grunt)
(global-set-key (kbd "C-M-g") 'grunt-exec)

;; js2-refactor
(require 'js2-refactor)
(js2r-add-keybindings-with-prefix "C-c C-r")

;; Clean modeline
(rename-modeline "js2-mode" js2-mode "JS2")

(require 'js-comint)
(setq inferior-js-program-command "node")
(setq inferior-js-mode-hook
			(lambda ()
				(ansi-color-for-comint-mode-on)
				(add-to-list
				 'comint-preoutput-filter-functions
				 (lambda (output)
					 (replace-regexp-in-string "\033\\[[0-9]+[A-Z]" "" output)))))
(add-hook
 'js2-mode-hook
 '(lambda ()
		(local-set-key "\C-cje" 'js-send-last-sexp)
		(local-set-key "\C-cjx" 'js-send-last-sexp-and-go)
		(local-set-key "\C-cjb" 'js-send-buffer)
		(local-set-key "\C-cj\C-b" 'js-send-buffer-and-go)
		(local-set-key "\C-cjn" 'js-send-region)
		(local-set-key "\C-cj\C-n" 'js-send-region-and-go)
		(local-set-key "\C-cjl" 'js-load-file-and-go)))

(require 'json-mode)

;; Use lambda for anonymous functions
;; (font-lock-add-keywords
;;  'js2-mode `(("\\(function\\) *("
;; 							(0 (progn (compose-region (match-beginning 1)
;; 																				(match-end 1) "\u03BB")
;; 												nil)))))

(add-hook 'js2-mode-hook '(lambda () (setq indent-tabs-mode 1)))

(require 'rainbow-delimiters)
(add-hook 'js2-mode-hook 'rainbow-delimiters-mode)

(provide 'setup-js2-mode)
;;; setup-js2-mode ends here
