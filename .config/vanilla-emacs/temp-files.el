;; Store backup files in a central location
(setq backup-directory-alist
      `(("." . ,(expand-file-name "backup" user-emacs-directory))))
(setq backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5)   ; and how many of the old

;; Store auto-save files in a central location
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-save/" user-emacs-directory) t)))

;; Create backup and auto-save directories if they don't exist
(unless (file-exists-p (expand-file-name "backup" user-emacs-directory))
  (make-directory (expand-file-name "backup" user-emacs-directory)))
(unless (file-exists-p (expand-file-name "auto-save" user-emacs-directory))
  (make-directory (expand-file-name "auto-save" user-emacs-directory)))
