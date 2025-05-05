;; Install dape
(rc/require 'dape)

;; Basic configuration
(setq dape-buffer-window-arrangement 'right) ;; Show debug windows on the right side

;; Key bindings organized by function groups
(global-set-key (kbd "C-c d d") 'dape)                     ;; Start debugging
(global-set-key (kbd "C-c d q") 'dape-quit)                ;; Quit debugging session
(global-set-key (kbd "C-c d r") 'dape-restart)             ;; Restart debugging

;; Execution control
(global-set-key (kbd "C-c d c") 'dape-continue)            ;; Continue execution
(global-set-key (kbd "C-c d n") 'dape-next)                ;; Step over
(global-set-key (kbd "C-c d i") 'dape-step-in)             ;; Step in
(global-set-key (kbd "C-c d o") 'dape-step-out)            ;; Step out

;; Breakpoints
(global-set-key (kbd "C-c d b") 'dape-breakpoint-toggle)   ;; Toggle breakpoint
(global-set-key (kbd "C-c d l") 'dape-breakpoint-log)      ;; Add log breakpoint
(global-set-key (kbd "C-c d x") 'dape-breakpoint-remove-all) ;; Remove all breakpoints

;; Inspection
(global-set-key (kbd "C-c d e") 'dape-eval)                ;; Evaluate expression
(global-set-key (kbd "C-c d w") 'dape-watch-dwim)          ;; Watch expression
(global-set-key (kbd "C-c d t") 'dape-select-thread)       ;; Select thread
(global-set-key (kbd "C-c d s") 'dape-select-stack)        ;; Select stack frame

(with-eval-after-load 'dape
  (add-to-list 'dape-configs
    '(cp-file-stdin
      modes (c-mode c++-mode c-ts-mode c++-ts-mode)
      :type "gdb"
      :command "gdb"
      :command-args ("--silent" "-i=dap")
      :program "a.out"      
      :args ("<" "input.txt" ">" "output.txt")
      :cwd "${fileDirname}"
     )))


