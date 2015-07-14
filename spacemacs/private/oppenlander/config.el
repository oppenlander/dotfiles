;; Configurations that need to happen

(spacemacs/declare-prefix "o" "oppenlander")

;; Make Multiple-Cursors not die while using evil
(setq mc/cmds-to-run-for-all
      '(
        evil-append-line
        evil-backward-WORD-begin
        evil-backward-word-begin
        evil-delete-char
        evil-delete-line
        evil-digit-argument-or-evil-beginning-of-line
        evil-emacs-state
        evil-end-of-line
        evil-force-normal-state
        evil-forward-WORD-begin
        evil-forward-WORD-end
        evil-forward-word-begin
        evil-forward-word-end
        evil-insert
        evil-next-line
        evil-normal-state
        evil-previous-line
        evil-forward-char
        evil-backward-char
        evil-change
        ))

;; Add ability to close frame
(spacemacs/declare-prefix "F" "frame")
(evil-leader/set-key "Fc" 'delete-frame)
(evil-leader/set-key "Ff" 'find-file-other-frame)
(evil-leader/set-key "Fb" 'display-buffer-other-frame)
(evil-leader/set-key "FC" 'delete-other-frames)
(evil-leader/set-key "Fs" 'switch-to-buffer-other-frame)
(evil-leader/set-key "Fd" 'dired-other-frame)
(evil-leader/set-key "Fo" 'other-frame)

;; Fix zenburn highlight persist isssue
;; (custom-set-faces
;;  '(evil-search-highlight-persist-highlight-face
;;    ((t (:background "selectedMenuItemColor")))))

(spacemacs/declare-prefix "ox" "text")
(spacemacs/declare-prefix "oxu" "uniquify")
(defun uniquify-region-lines (beg end)
  "Remove duplicate adjacent lines in region."
  (interactive "*r")
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "^\\(.*\n\\)\\1+" end t)
      (replace-match "\\1"))))
(evil-leader/set-key "oxur" 'uniquify-region-lines)
(defun uniquify-buffer-lines ()
  "Remove duplicate adjacent lines in the current buffer."
  (interactive)
  (uniquify-region-lines (point-min) (point-max)))
(evil-leader/set-key "oxub" 'uniquify-region-lines)

(spacemacs/declare-prefix "oi" "irc")
(defun oppenlander/irc-join-zipscene ()
  (interactive)
  (erc
   :server "irc.zipscene.com"
   :port 6667
   :nick "oppenlander"
   :password (password-store-get "IRC/zipscene")))
(evil-leader/set-key "oiz" 'oppenlander/irc-join-zipscene)


