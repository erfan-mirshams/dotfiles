(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
(setq rc-file (expand-file-name "rc.el" user-emacs-directory))
(load rc-file)
(setq inhibit-x-resources t)
(setq frame-inhibit-implied-resize t)

(rc/require 'all-the-icons)
(rc/require 'all-the-icons-dired)

;; Enable all-the-icons-dired mode when Dired mode is activated
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

;; Optional: Ensure icons are colored
(add-hook 'dired-mode-hook
          (lambda ()
            (setq-local all-the-icons-dired-monochrome t)))

(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-i") 'consult-imenu)

(rc/require 'vertico)
(vertico-mode t)

(rc/require 'marginalia)
(marginalia-mode t)

(rc/require 'orderless)
;; Use orderless completion style
(setq completion-styles '(orderless)
      completion-category-overrides '((file (styles . (partial-completion)))))

(rc/require 'consult)
;; Example consult keybindings
;;(global-set-key (kbd "C-s") 'consult-line)       ;; Enhanced search within the buffer
(global-set-key (kbd "C-x b") 'consult-buffer)   ;; Enhanced buffer switch
(recentf-mode)
(global-set-key (kbd "C-x C-r") 'consult-recent-file) ;; Enhanced recent file search
(global-set-key (kbd "C-x /") 'consult-ripgrep)

(rc/require 'embark)
;; Use Embark Act for performing actions on current minibuffer selection
(global-set-key (kbd "C-.") 'embark-act)  ;; Often set to a key like C-.
(global-set-key (kbd "C-;") 'embark-dwim) ;; Execute default action, similar to embark-act

;; Optionally add a keybinding for embark which-key integration
(setq embark-prompter 'embark-keymap-prompter)

;; create temp files in backup dir
(setq temp-files-config (expand-file-name "temp-files.el" user-emacs-directory))
(load temp-files-config)

(setq pdf-tools-config (expand-file-name "pdf-tools.el" user-emacs-directory))
(load pdf-tools-config)

(rc/require 'whole-line-or-region)
(whole-line-or-region-global-mode t)

(global-set-key [remap dabbrev-expand] 'hippie-expand)

;; change default behavior of zap to char to zap up to char
(global-set-key (kbd "M-z") 'zap-up-to-char)

(add-hook 'tex-mode-hook
          #'(lambda () (setq ispell-parser 'tex)))

(require 'dired-x)

(global-set-key (kbd "C-x j") #'duplicate-dwim)

(rc/require 'magit)
(add-hook 'git-commit-post-finish-hook 'magit)

;; Install ultra-scroll from GitHub if not already installed
(unless (package-installed-p 'ultra-scroll)
  (package-vc-install '(ultra-scroll :vc-backend Git 
                                    :url "https://github.com/jdtsmith/ultra-scroll")))

;; Configure ultra-scroll
(setq scroll-conservatively 101  ; important!
      scroll-margin 0)

;; Enable ultra-scroll mode
(require 'ultra-scroll)
(ultra-scroll-mode 1)

(setq treesit-file (expand-file-name "treesit.el" user-emacs-directory))
(when (file-exists-p treesit-file)
  (load treesit-file))

(setq prog-setup-file (expand-file-name "prog-setup.el" user-emacs-directory))
(when (file-exists-p prog-setup-file)
  (load prog-setup-file))

(global-set-key (kbd "C-x c") #'compile)

;; Install YASnippet
(rc/require 'yasnippet)

;; Initialize YASnippet
(yas-global-mode 1)

;; Set up a snippet directory dynamically
(setq yas-snippet-dirs (list (expand-file-name "snippets" user-emacs-directory)))

(rc/require 'dockerfile-mode)

(rc/require 'envrc)
(envrc-global-mode t)

(rc/require 'zoxide)
(global-set-key (kbd "C-c z") #'zoxide-find-file)

;; org config
(setq org-setup-file (expand-file-name "org.el" user-emacs-directory))
(when (file-exists-p org-setup-file)
  (load org-setup-file))

(rc/require 'ox-hugo)

(rc/require 'keycast)

;; Configure dashboard
(rc/require 'dashboard)
(setq dashboard-banner-logo-title "Welcome to Emacs")
(setq dashboard-startup-banner 'logo)
(setq dashboard-center-content t)
(setq dashboard-items '((recents  . 5)
                       (bookmarks . 5)
                       (projects . 5)))
(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)
(dashboard-setup-startup-hook)

;; Show dashboard instead of scratch buffer for emacsclient
(defun show-dashboard ()
  (interactive)
  (dashboard-refresh-buffer)
  (switch-to-buffer dashboard-buffer-name))

(add-hook 'server-after-make-frame-hook 'show-dashboard)

;; Load dape configuration
;; (setq dape-config-file (expand-file-name "dape-config.el" user-emacs-directory))
;; (when (file-exists-p dape-config-file)
;;   (load dape-config-file))

(rc/require-vterm)
(setq vterm-shell "/usr/bin/zsh")

(defun rc/popup-term ()
  "Open a terminal in a popup window."
  (interactive)
  (let ((buf (get-buffer-create "*terminal*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'vterm-mode)
        (vterm-mode)))
    (display-buffer-in-side-window
     buf
     '((side . bottom)
       (window-height . 0.3)
       (window-parameters . ((no-delete-other-windows . t)))))
    (select-window (get-buffer-window buf))))

(global-set-key (kbd "C-c t") 'rc/popup-term)

(dolist (mode '(vterm-mode
                term-mode
                shell-mode
                eshell-mode))
  (add-hook (intern (format "%s-hook" mode))
            (lambda () (display-line-numbers-mode -1))))

;; multiple cursors
(rc/require 'multiple-cursors)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->")         'mc/mark-next-like-this)
(global-set-key (kbd "C-<")         'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<")     'mc/mark-all-like-this)
(global-set-key (kbd "C-\"")        'mc/skip-to-next-like-this)
(global-set-key (kbd "C-:")         'mc/skip-to-previous-like-this)
