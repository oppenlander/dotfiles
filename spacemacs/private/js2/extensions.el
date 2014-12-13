(defvar js2-pre-extensions
  '(
    ;; pre extension js2s go here
    )
  "List of all extensions to load before the packages.")

(defvar js2-post-extensions
  '(
    ;; post extension js2s go here
    )
  "List of all extensions to load after the packages.")

;; For each extension, define a function js2/init-<extension-js2>
;;
;; (defun js2/init-my-extension ()
;;   "Initialize my extension"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
