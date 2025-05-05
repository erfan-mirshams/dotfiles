;; First, try to load eglot but don't fail if it's not available
(require 'eglot nil t)

;; programming modes
(rc/require 'go-mode)
(rc/require 'rust-mode)
(rc/require 'yaml-mode)
(rc/require 'markdown-mode)
(rc/require 'markdown-ts-mode)

;; Python virtual environment management
(rc/require 'pyvenv)
(setq pyvenv-mode-line-indicator '(pyvenv-virtual-env-name (" [venv:" pyvenv-virtual-env-name "] ")))
(pyvenv-mode 1)

;; Define languages we want to use with Eglot
(defvar rc/eglot-auto-enable-modes
  '(
    ;; Tree-sitter modes
    python-ts-mode
    js-ts-mode
    typescript-ts-mode
    css-ts-mode
    c-ts-mode
    c++-ts-mode
    java-ts-mode
    rust-ts-mode
    go-ts-mode
    yaml-ts-mode
    json-ts-mode
    bash-ts-mode
    
    ;; Traditional modes (as fallback)
    python-mode
    js-mode
    typescript-mode
    css-mode
    c-mode
    c++-mode
    java-mode
    rust-mode
    go-mode
    yaml-mode
    json-mode
    sh-mode
    )
  "List of major modes where Eglot should be automatically enabled.")

;; A function to check if a server is likely available before starting Eglot
(defun rc/eglot-try-enable ()
  "Try to enable Eglot for the current buffer if a server is likely available."
  (when (and (featurep 'eglot)             ;; Make sure eglot is loaded
             (member major-mode rc/eglot-auto-enable-modes)
             (buffer-file-name))
    ;; Only start Eglot if a server is configured for this major mode
    (let ((servers eglot-server-programs)  ;; Use the variable, not calling as a function
          (found nil))
      (dolist (server servers)
        (when (or (and (symbolp (car server)) 
                       (eq (car server) major-mode))
                  (and (listp (car server))
                       (member major-mode (car server))))
          (setq found t)))
      
      (when found
        (eglot-ensure)))))

;; Only set up hooks if eglot is available
(when (featurep 'eglot)
  ;; Automatically generate hooks for all specified modes
  (dolist (mode rc/eglot-auto-enable-modes)
    (let ((hook-name (intern (format "%s-hook" mode))))
      (add-hook hook-name #'rc/eglot-try-enable)))

  ;; Global hook for programming modes as a fallback
  (add-hook 'prog-mode-hook
            (lambda ()
              ;; Only try if it's not already in our specific list
              (unless (member major-mode rc/eglot-auto-enable-modes)
                (rc/eglot-try-enable)))))
