;; -*- lexical-binding: t; -*-

(setq user-full-name "Erfan Mirshams"
      user-mail-address "erfanmirshams@protonmail.com")

(setq doom-font (font-spec :family "Inconsolata Nerd Font Mono" :size 28)
      doom-variable-pitch-font (font-spec :family "Samim" :size 30)
      doom-big-font (font-spec :family "Inconsolata Nerd Font Mono" :size 42))

(setq doom-theme 'modus-vivendi)

(set-frame-parameter (selected-frame) 'alpha '(95 95))
(add-to-list 'default-frame-alist '(alpha 95 95))

(setq display-line-numbers-type 'relative)

(require 'org-faces)

;; Configure fill width
(setq visual-fill-column-width 50
      visual-fill-column-center-text t)


(defun my/org-present-prepare-slide (buffer-name heading)
  ;; Show only top-level headlines
  (org-overview)

  ;; Unfold the current entry
  (org-show-entry)

  ;; Show only direct subheadings of the slide but don't expand them
  (org-show-children))

(defun my/org-present-start ()
  ;; Tweak font sizes
  (doom-big-font-mode 1)
  ;; Remove Line Numbers
  (display-line-numbers-mode 0)
  ;; Set a blank header line string to create blank space at the top
  (setq header-line-format " ")

  ;; Display inline images automatically
  (org-display-inline-images)

  ;; Center the presentation and wrap lines
  (visual-fill-column-mode 1)
  (visual-line-mode 1))

(defun my/org-present-end ()
  ;; Reset font customizations
  (doom-big-font-mode 0)
  ;; Add Line Numbers
  (display-line-numbers-mode 1)
  ;; Clear the header line string so that it isn't displayed
  (setq header-line-format nil)

  ;; Stop displaying inline images
  (org-remove-inline-images)

  ;; Stop centering the document
  (visual-fill-column-mode 0)
  (visual-line-mode 0))

;; Register hooks with org-present
(add-hook 'org-present-mode-hook 'my/org-present-start)
(add-hook 'org-present-mode-quit-hook 'my/org-present-end)
(add-hook 'org-present-after-navigate-functions 'my/org-present-prepare-slide)

(map! :leader
      (:prefix ("d" . "dired")
       :desc "Open dired" "d" #'dired
       :desc "Dired jump to current" "j" #'dired-jump)
      (:after dired
              (:map dired-mode-map
               :desc "Peep-dired image previews" "d p" #'peep-dired
               :desc "Dired view file"           "d v" #'dired-view-file)))

(evil-define-key 'normal dired-mode-map
  (kbd "M-RET") 'dired-display-file
  (kbd "h") 'dired-up-directory
  (kbd "l") 'dired-open-file ; use dired-find-file instead of dired-open.
  (kbd "m") 'dired-mark
  (kbd "t") 'dired-toggle-marks
  (kbd "u") 'dired-unmark
  (kbd "C") 'dired-do-copy
  (kbd "D") 'dired-do-delete
  (kbd "J") 'dired-goto-file
  (kbd "M") 'dired-do-chmod
  (kbd "O") 'dired-do-chown
  (kbd "P") 'dired-do-print
  (kbd "R") 'dired-do-rename
  (kbd "T") 'dired-do-touch
  (kbd "Y") 'dired-copy-filenamecopy-filename-as-kill ; copies filename to kill ring.
  (kbd "Z") 'dired-do-compress
  (kbd "+") 'dired-create-directory
  (kbd "-") 'dired-do-kill-lines
  (kbd "% l") 'dired-downcase
  (kbd "% m") 'dired-mark-files-regexp
  (kbd "% u") 'dired-upcase
  (kbd "* %") 'dired-mark-files-regexp
  (kbd "* .") 'dired-mark-extension
  (kbd "* /") 'dired-mark-directories
  (kbd "; d") 'epa-dired-do-decrypt
  (kbd "; e") 'epa-dired-do-encrypt)

;; With dired-open plugin, you can launch external programs for certain extensions.
(setq dired-open-extensions '(("gif" . "sxiv")
                              ("jpg" . "sxiv")
                              ("png" . "sxiv")
                              ("mkv" . "mpv")
                              ("mp4" . "mpv")))

(evil-define-key 'normal peep-dired-mode-map
  (kbd "j") 'peep-dired-next-file
  (kbd "k") 'peep-dired-prev-file)
(add-hook 'peep-dired-hook 'evil-normalize-keymaps)

(setq bookmark-default-file "~/.config/doom/bookmarks")

(map! :leader
      (:prefix ("b". "buffer")
       :desc "List bookmarks"                          "L" #'list-bookmarks
       :desc "Set bookmark"                            "m" #'bookmark-set
       :desc "Delete bookmark"                         "M" #'bookmark-set
       :desc "Save current bookmarks to bookmark file" "w" #'bookmark-save))

(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

(evil-define-key 'normal ibuffer-mode-map
  (kbd "f c") 'ibuffer-filter-by-content
  (kbd "f d") 'ibuffer-filter-by-directory
  (kbd "f f") 'ibuffer-filter-by-filename
  (kbd "f m") 'ibuffer-filter-by-mode
  (kbd "f n") 'ibuffer-filter-by-name
  (kbd "f x") 'ibuffer-filter-disable
  (kbd "g h") 'ibuffer-do-kill-lines
  (kbd "g H") 'ibuffer-update)

(setq elfeed-goodies/entry-pane-size 0.5)

(evil-define-key 'normal elfeed-show-mode-map
  (kbd "J") 'elfeed-goodies/split-show-next
  (kbd "K") 'elfeed-goodies/split-show-prev)
(evil-define-key 'normal elfeed-search-mode-map
  (kbd "J") 'elfeed-goodies/split-show-next
  (kbd "K") 'elfeed-goodies/split-show-prev)
(setq elfeed-feeds (quote
                    (("https://www.reddit.com/r/linux.rss" reddit linux)
                     ("https://www.reddit.com/r/commandline.rss" reddit commandline)
                     ("https://www.reddit.com/r/distrotube.rss" reddit distrotube)
                     ("https://www.reddit.com/r/emacs.rss" reddit emacs)
                     ("https://www.gamingonlinux.com/article_rss.php" gaming linux)
                     ("https://hackaday.com/blog/feed/" hackaday linux)
                     ("https://opensource.com/feed" opensource linux)
                     ("https://linux.softpedia.com/backend.xml" softpedia linux)
                     ("https://itsfoss.com/feed/" itsfoss linux)
                     ("https://www.zdnet.com/topic/linux/rss.xml" zdnet linux)
                     ("https://www.phoronix.com/rss.php" phoronix linux)
                     ("http://feeds.feedburner.com/d0od" omgubuntu linux)
                     ("https://www.computerworld.com/index.rss" computerworld linux)
                     ("https://www.networkworld.com/category/linux/index.rss" networkworld linux)
                     ("https://www.techrepublic.com/rssfeeds/topic/open-source/" techrepublic linux)
                     ("https://betanews.com/feed" betanews linux)
                     ("http://lxer.com/module/newswire/headlines.rss" lxer linux))))

(after! cider
  (set-popup-rule! "^\\*cider" :ignore t))

(map! :leader
      (:prefix ("e". "evaluate/ERC/EWW")
       :desc "Evaluate elisp in buffer"  "b" #'eval-buffer
       :desc "Evaluate defun"            "d" #'eval-defun
       :desc "Evaluate elisp expression" "e" #'eval-expression
       :desc "Evaluate last sexpression" "l" #'eval-last-sexp
       :desc "Evaluate elisp in region"  "r" #'eval-region))

(setq browse-url-browser-function 'browse-url-firefox)

(map! :leader
      (:prefix ("t" . "toggle")
       :desc "Toggle line numbers"            "l" #'doom/toggle-line-numbers
       :desc "Toggle line highlight in frame" "h" #'hl-line-mode
       :desc "Toggle line highlight globally" "H" #'global-hl-line-mode
       :desc "Toggle truncate lines"          "t" #'toggle-truncate-lines))

(xterm-mouse-mode 1)

(map! :leader
      :desc "Org babel tangle" "m B" #'org-babel-tangle
      :desc "Org babe execute source block code" "b e" #'org-babel-execute-src-block-maybe)
(use-package! org
  :config
  (setq org-directory "~/org/")
  (setq org-agenda-files
        '("~/org/habits.org"
          "~/org/birthdays.org"
          "~/org/todo.org"))
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-refile-targets
        '(("archive.org" :maxlevel . 1)
          ("todo.org" :maxlevel . 1)))
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  (advice-add 'org-agenda-quit :after 'org-save-all-org-buffers))
(defun set-bidi-env ()
  "interactive"
  (setq bidi-paragraph-direction 'nil))
(add-hook 'org-mode-hook 'set-bidi-env)

(use-package! org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-default t))

(use-package! org-roam
  :custom
  (org-roam-directory "~/org/roam")
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n#+DATE: %U\n")
      :unnarrowed t)
     ("p" "project" entry (file "~/org/roam/templates/project-capture.org")
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Project")
      :unnarrowed t)))
  :config
  (org-roam-db-autosync-mode))

(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (push arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

(defun my/org-roam-filter-by-tag (tag-name)
  (lambda (node)
    (member tag-name (org-roam-node-tags node))))

(defun my/org-roam-list-notes-by-tag (tag-name)
  (mapcar #'org-roam-node-file
          (seq-filter
           (my/org-roam-filter-by-tag tag-name)
           (org-roam-node-list))))

(defun my/org-roam-refresh-agenda-list ()
  (interactive)
  (let* ((current-files org-agenda-files)
         (new-files (my/org-roam-list-notes-by-tag "Project"))
         (all-files (delete-dups (append current-files new-files))))
    (setq org-agenda-files all-files)))
;; Build the agenda list the first time for the session
(my/org-roam-refresh-agenda-list)

(defun my/org-roam-project-finalize-hook ()
  "Adds the captured project file to `org-agenda-files' if the
capture was not aborted."
  ;; Remove the hook since it was added temporarily
  (remove-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

  ;; Add project file to the agenda list if the capture was confirmed
  (unless org-note-abort
    (with-current-buffer (org-capture-get :buffer)
      (add-to-list 'org-agenda-files (buffer-file-name)))))

(defun my/org-roam-find-project ()
  (interactive)
  ;; Add the project file to the agenda after capture is finished
  (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

  ;; Select a project file to open, creating it if necessary
  (org-roam-node-find
   nil
   nil
   (my/org-roam-filter-by-tag "Project")
   nil
   :templates
   '(("p" "project" entry (file "~/org/roam/templates/project-capture.org")
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Project")
      :unnarrowed t)
     )))
(defun my/org-roam-capture-task ()
  (interactive)
  ;; Add the project file to the agenda after capture is finished
  (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

  ;; Capture the new task, creating the project file if necessary
  (org-roam-capture- :node (org-roam-node-read
                            nil
                            (my/org-roam-filter-by-tag "Project"))
                     :templates '(("p" "project" plain "** TODO %?"
                                   :if-new (file+head+olp "%<%Y%m%d%H%M%S>-${slug}.org"
                                                          "#+title: ${title}\n#+category: ${title}\n#+filetags: Project"
                                                          ("Tasks"))))))

(map! :leader
      (:prefix "n r"
       :desc "insert node immediate" "I" #'org-roam-node-insert-immediate
       :desc "find Project node" "p" #'my/org-roam-find-project
       :desc "capture Task to Project" "t" #'my/org-roam-capture-task))

(map! :leader
      (:prefix ("w" . "window")
       :desc "Winner redo" "<right>" #'winner-redo
       :desc "Winner undo" "<left>"  #'winner-undo))

(after! clojure-mode
  (add-hook 'clojure-mode-hook #'enable-paredit-mode))

;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook
  (prog-mode . copilot-mode)
  (copilot-mode . (lambda ()
                    (setq-local copilot--indent-warning-printed-p t))))

;; Set the keybinding for accepting Copilot completions to C-y
(map! :map copilot-completion-map
      "C-y" #'copilot-accept-completion
      "C-S-y" #'copilot-accept-completion-by-word )

(after! lsp-mode
  ;; https://github.com/emacs-lsp/lsp-mode/issues/3577#issuecomment-1709232622
  (delete 'lsp-terraform lsp-client-packages))

(use-package! kubernetes
  :ensure t
  :commands (kubernetes-overview)
  :config
  (setq kubernetes-poll-frequency 3600
        kubernetes-redraw-frequency 3600))
(use-package! kubernetes-evil
  :defer
  :after kubernetes)
(map! :leader
      (:prefix "o"
        :desc "Kubernetes" "k" 'kubernetes-overview))

(use-package! telega
  :defer t
  :custom
  (telega-directory (expand-file-name "telega" doom-cache-dir))
  (telega-video-player-command '(concat "mpv"
                                        (when telega-ffplay-media-timestamp
                                          (format " --start=%f" telega-ffplay-media-timestamp))))
  (telega-completing-read-function completing-read-function)
  (telega-use-docker t))
(map! (:leader
       :desc "Telegram" :mv "o m" #'erfan/=telega)
      (:after telega
       :map telega-root-mode-map
       :n "q" #'erfan/=telega-kill))
(after! evil-snipe
  (add-to-list 'evil-snipe-disabled-modes 'telega-root-mode))
(after! telega
  (set-company-backend! 'telega-chat-mode
    '(company-ispell
      company-dabbrev
      telega-company-telegram-emoji
      telega-company-username
      telega-company-botcmd
      telega-company-hashtag)))
(add-hook! telega-load
           ;; core
           #'telega-mode-line-mode
           #'global-telega-squash-message-mode
           #'telega-notifications-mode
           #'telega-autoplay-mode

           ;; contrib
           #'global-telega-url-shorten-mode
           #'global-telega-mnz-mode
           #'telega-alert-mode
           #'telega-transient-mode
           #'telega-status-history-mode)
(add-hook! '(telega-root-mode-hook telega-chat-mode-hook)
           #'hl-line-mode)
(add-hook! kill-emacs
  (when (and (boundp 'telega-root-buffer-name)
             (get-buffer telega-root-buffer-name))
    (telega-kill t)))
(after! telega
  (set-popup-rule! "\\`\\*Telega.+"
    :ignore t))
;;; $DOOMDIR/autoload/telega.el -*- lexical-binding: t; -*-
(defvar +telega-workspace-name "Telega")

;;;###autoload
(defun erfan/=telega (&optional arg)
  "Like `telega', but opens the buffer in it's own workspace."
  (interactive "P")
  (+workspace-switch +telega-workspace-name t)
  (unless (memq (buffer-local-value 'major-mode (window-buffer (selected-window)))
                '(telega-root-mode telega-chat-mode telega-image-mode))
    (doom/switch-to-scratch-buffer)
    (telega arg)
    (+workspace/display)))
;;;###autoload
(defun erfan/=telega-kill (&optional args)
  "Like `telega-kill', but deletes the workspace associated with `+telega-workspace-name' if it exists."
  (interactive "P")
  (telega-kill args)
  (+workspace-delete +telega-workspace-name))

(use-package python-mode
  :ensure t
  :hook (python-mode . lsp-deferred)
  :custom
  (dap-python-debugger 'debugpy)
  :config
  (require 'dap-python))

(require 'dap-dlv-go)
