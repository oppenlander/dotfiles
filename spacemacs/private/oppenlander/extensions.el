(defvar oppenlander-pre-extensions
  '(
    ;; pre extension usrs go here
    )
  "List of all extensions to load before the packages.")

(defvar oppenlander-post-extensions
  '(
    ;; post extension usrs go here
    )
  "List of all extensions to load after the packages.")

;; For each extension, define a function usr/init-<extension-usr>
;;
;; (defun usr/init-my-extension ()
;;   "Initialize my extension"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
