;; Install and load pdf-tools
(rc/require 'pdf-tools)

;; Initialize pdf-tools
(pdf-tools-install t)

;; Use PDF Tools instead of DocView for PDF files
(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))

;; Quality and display settings
(setq-default pdf-view-display-size 'fit-page)
(setq pdf-view-use-scaling t
      pdf-view-use-imagemagick nil)

;; For better quality (at the cost of performance, adjust as needed)
(setq pdf-view-resize-factor 1.1)

;; Configure midnight colors if you enable midnight mode
(setq pdf-view-midnight-colors '("#eceff4" . "#2e3440"))

;; For better navigation
(setq pdf-view-continuous t
      pdf-view-midnight-invert nil)

;; Customizing key bindings
(with-eval-after-load 'pdf-view
  (define-key pdf-view-mode-map (kbd "h") 'pdf-annot-add-highlight-markup-annotation)
  (define-key pdf-view-mode-map (kbd "t") 'pdf-annot-add-text-annotation)
  (define-key pdf-view-mode-map (kbd "D") 'pdf-annot-delete))

;; Enable useful minor modes
(add-hook 'pdf-view-mode-hook 'pdf-isearch-minor-mode)
(add-hook 'pdf-view-mode-hook 'pdf-links-minor-mode)
(add-hook 'pdf-view-mode-hook 'pdf-annot-minor-mode)

;; avoid jumping suddenly to next page
(setq pdf-view-continuous t)

