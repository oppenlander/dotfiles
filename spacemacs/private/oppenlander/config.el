;; Configurations that need to happen

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
(evil-leader/set-key "Fc" 'delete-frame)
(evil-leader/set-key "Ff" 'find-file-other-frame)
(evil-leader/set-key "Fb" 'display-buffer-other-frame)
(evil-leader/set-key "FC" 'delete-other-frames)
(evil-leader/set-key "Fs" 'switch-to-buffer-other-frame)
(evil-leader/set-key "Fd" 'dired-other-frame)
(evil-leader/set-key "Fo" 'other-frame)
