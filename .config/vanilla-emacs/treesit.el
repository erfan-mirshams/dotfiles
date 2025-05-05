;; Define treesit-language-source-alist if it doesn't exist
(defvar treesit-language-source-alist
  '((c "https://github.com/tree-sitter/tree-sitter-c")
    (cpp "https://github.com/tree-sitter/tree-sitter-cpp") 
    (go "https://github.com/tree-sitter/tree-sitter-go")
    (java "https://github.com/tree-sitter/tree-sitter-java")
    (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
    (html "https://github.com/tree-sitter/tree-sitter-html")
    (json "https://github.com/tree-sitter/tree-sitter-json")
    (verilog "https://github.com/tree-sitter/tree-sitter-verilog")
    (bash "https://github.com/tree-sitter/tree-sitter-bash")
    (python "https://github.com/tree-sitter/tree-sitter-python")
    (yaml "https://github.com/ikatyang/tree-sitter-yaml")
    (elisp "https://github.com/Wilfred/tree-sitter-elisp")
    (css "https://github.com/tree-sitter/tree-sitter-css")
    (make "https://github.com/alemuller/tree-sitter-make")
    (markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown")
    (markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown"))
  "Alist mapping languages to their TreeSitter grammar repository URLs.")

(defun rc/treesit-language-install (lang-name &optional lang-source)
  "Ensure TreeSitter grammar for LANG-NAME is installed.
If LANG-SOURCE is provided, use it as the source URL, otherwise use the default."
  (unless (treesit-language-available-p lang-name)
    (let* ((source (or lang-source
                        (alist-get lang-name
                                   (mapcar (lambda (item)
                                             (cons (car item)
                                                   (alist-get 'url (cdr item))))
                                           treesit-language-source-alist)
                                   nil nil #'equal)))
           (message-log-max nil)) ; Suppress message log output
      (message "Installing TreeSitter grammar for %s..." lang-name)
      (condition-case err
          (progn
            (treesit-install-language-grammar lang-name source)
            (message "TreeSitter grammar for %s installed successfully!" lang-name))
        (error
         (message "Failed to install TreeSitter grammar for %s: %s" 
                  lang-name (error-message-string err)))))))

;; install additional ts-modes
(rc/require 'verilog-ts-mode)

;; Define a mapping of special cases where the mode name doesn't follow the standard pattern
(defvar rc/treesit-mode-name-exceptions
  '((javascript . js)
    (elisp . emacs-lisp)
    (cpp . c++))
  "Alist of language names that don't follow the standard naming convention.")

;; Function to build major-mode-remap-alist from treesit-language-source-alist
(defun rc/build-major-mode-remap-alist ()
  "Build major-mode-remap-alist based on available tree-sitter languages."
  (let (remappings)
    (dolist (lang-entry treesit-language-source-alist)
      (let* ((lang-name (car lang-entry))
             (base-name (or (alist-get lang-name rc/treesit-mode-name-exceptions)
                           lang-name))
             (regular-mode-symbol (intern (format "%s-mode" base-name)))
             (ts-mode-symbol (intern (format "%s-ts-mode" base-name))))
        ;; Only add remapping if the treesitter mode exists
        (when (fboundp ts-mode-symbol)
          (push (cons regular-mode-symbol ts-mode-symbol) remappings))))
    remappings))

;; Only run setup if TreeSitter is available
(when (fboundp 'treesit-available-p)
  ;; Install all available language grammars from treesit-language-alist
  (dolist (lang-entry treesit-language-source-alist)
    (rc/treesit-language-install (car lang-entry)))
  ;; Build and set the remapping
  (setq major-mode-remap-alist (rc/build-major-mode-remap-alist)))
