;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 4)
 '(c-ts-mode-indent-offset 4)
 '(custom-enabled-themes '(modus-vivendi))
 '(delete-selection-mode t)
 '(display-line-numbers-type 'relative)
 '(electric-pair-mode t)
 '(electric-pair-pairs '((34 . 34) (8216 . 8217) (8220 . 8221) (123 . 125)))
 '(enable-recursive-minibuffers t)
 '(gdb-many-windows t)
 '(gdb-show-main t)
 '(gdb-use-colon-colon-notation t)
 '(global-auto-revert-mode t)
 '(global-display-line-numbers-mode t)
 '(go-ts-mode-indent-offset 4)
 '(gud-gdb-command-name "gdb -q -i=mi")
 '(ido-enable-flex-matching nil)
 '(ido-everywhere nil)
 '(ido-mode nil nil (ido))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(magit-bury-buffer-function 'magit-mode-quit-window)
 '(magit-display-buffer-function 'magit-display-buffer-traditional)
 '(menu-bar-mode nil)
 '(org-hide-leading-stars t)
 '(org-startup-folded 'showeverything)
 '(package-selected-packages nil)
 '(package-vc-selected-packages
   '((ultra-scroll :vc-backend Git :url
                   "https://github.com/jdtsmith/ultra-scroll")))
 '(safe-local-variable-values '((projectile-project-compilation-cmd . "make e2e-test")))
 '(scroll-bar-mode nil)
 '(tab-always-indent 'complete)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(use-short-answers t)
 '(warning-minimum-level :error)
 '(whole-line-or-region-global-mode t)
 '(windmove-default-keybindings '(nil shift))
 '(windmove-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#000000" :foreground "#ffffff" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 110 :width normal :foundry "GOOG" :family "Inconsolata Nerd Font Mono"))))
 '(org-level-1 ((t (:foreground "#51afef" :weight bold))))
 '(org-level-2 ((t (:foreground "#c678dd" :weight bold))))
 '(org-level-3 ((t (:foreground "#98be65" :weight bold))))
 '(org-level-4 ((t (:foreground "#da8548" :weight bold))))
 '(org-level-5 ((t (:foreground "#5699af" :weight bold))))
 '(org-level-6 ((t (:foreground "#a9a1e1" :weight bold))))
 '(org-level-7 ((t (:foreground "#46d9ff" :weight bold))))
 '(org-level-8 ((t (:foreground "#ff6c6b" :weight bold)))))
