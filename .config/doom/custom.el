(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("ddffe74bc4bf2c332c2c3f67f1b8141ee1de8fd6b7be103ade50abb97fe70f0c" default))
 '(ein:jupyter-server-use-subcommand "server" t)
 '(elfeed-feeds '("https://mirshams.com/index.xml") t)
 '(ignored-local-variable-values
   '((projectile-project-compilation-cmd . "go test ./... -v -count=1")))
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
 '(org-agenda-files
   '("/home/erfan/org/habits.org" "/home/erfan/org/birthdays.org" "/home/erfan/org/todo.org" "/home/erfan/org/roam/20240306165225-divar_messenger_project.org" "/home/erfan/org/roam/20230826101714-asiawatt_website.org" "/home/erfan/org/roam/20240608153223-divar_email_service.org"))
 '(package-selected-packages '(kubernetes-evil kubernetes))
 '(safe-local-variable-values
   '((lsp-clangd-format-fallback-style . "llvm")
     (lsp-clangd-format-style . "file")
     (projectile-project-compilation-cmd . "make e2e-test")))
 '(warning-suppress-types '((yasnippet zombie) (defvaralias))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ts-fold-replacement-face ((t (:foreground unspecified :box nil :inherit font-lock-comment-face :weight light)))))
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'projectile-ag 'disabled nil)
