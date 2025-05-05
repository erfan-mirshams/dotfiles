(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Initialize the package system if it's not yet initialized
(unless (bound-and-true-p package--initialized) 
  (package-initialize))

;; Refresh package contents
(unless package-archive-contents
  (package-refresh-contents))

(defvar rc/package-contents-refreshed nil)

(defun rc/package-refresh-contents-once ()
  (when (not rc/package-contents-refreshed)
    (setq rc/package-contents-refreshed t)
    (package-refresh-contents)))

(defun rc/require-one-package (package)
  (when (not (package-installed-p package))

    (rc/package-refresh-contents-once)
    (package-install package)))

(defun rc/require (&rest packages)
  (dolist (package packages)
    (rc/require-one-package package)))

(defun rc/require-theme (theme)
  (let ((theme-package (->> theme
                            (symbol-name)
                            (funcall (-flip #'concat) "-theme")
                            (intern))))
    (rc/require theme-package)
    (load-theme theme t)))

(defun rc/require-vterm ()
  "Ensure vterm is installed and its module is compiled."
  (rc/require 'vterm)
  (let ((vterm-always-compile-module t))  ; Force automatic compilation
    (unless (require 'vterm nil t)
      (message "Compiling vterm module...")
      (vterm-module-compile))))
