;; [[file:../readme.org::#h:28CB5546-FBE4-481D-B620-623006DC0FDA][lsp-mode:1]]
(use-package lsp-mode
  :commands
  (lsp lsp-deferred)
  :hook
  ((lsp-mode . (lambda () (setq-local evil-lookup-func #'lsp-describe-thing-at-point)))
   (lsp-mode . lsp-enable-which-key-integration))
  :general
  (lc/local-leader-keys
    :states 'normal
    :keymaps 'lsp-mode-map
    "i" '(:ignore t :which-key "import")
    "i o" '(lsp-organize-imports :wk "optimize")
    "l" '(:keymap lsp-command-map :wk "lsp")
    "a" '(lsp-execute-code-action :wk "code action")	
    "r" '(lsp-rename :wk "rename"))
  ;; (lsp-mode-map
  ;;  :states 'normal
  ;;  "gD" 'lsp-find-references)
  :init
  (setq lsp-restart 'ignore)
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-enable-file-watchers nil)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-keep-workspace-alive nil)
  (setq lsp-auto-execute-action nil)
  (setq lsp-before-save-edits nil)
	(setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-diagnostics-provider :none)
  )
;; lsp-mode:1 ends here

;; [[file:../readme.org::#h:B69B077F-B96E-4A86-859D-A3D29547D39C][lsp-ui:1]]
(use-package lsp-ui
  :hook
  ((lsp-mode . lsp-ui-mode)
   ;; (lsp-mode . (lambda () (setq-local evil-goto-definition-functions '(lambda (&rest args) (lsp-ui-peek-find-definitions)))))
   )
  ;; :bind
  ;; (:map lsp-ui-mode-map
  ;;       ([remap lsp-find-references] . lsp-ui-peek-find-references))
  :general
  ;; (lc/local-leader-keys
  ;;   "h" 'lsp-ui-doc-show
  ;;   "H" 'lsp-ui-doc-hide)
  (lsp-ui-peek-mode-map
   :states 'normal
   "C-j" 'lsp-ui-peek--select-next
   "C-k" 'lsp-ui-peek--select-prev)
  (outline-mode-map
   :states 'normal
   "C-j" 'nil
   "C-k" 'nil)
  :init
  (setq lsp-ui-doc-show-with-cursor nil)
  (setq lsp-ui-doc-show-with-mouse nil)
  (setq lsp-ui-peek-always-show t)
  (setq lsp-ui-peek-fontify 'always)
  )
;; lsp-ui:1 ends here

;; [[file:../readme.org::#h:85405E87-7858-4F5C-A229-B72E70F68597][dap-mode:1]]
(use-package dap-mode
  :hook
  ((dap-mode . corfu-mode)
   (dap-terminated . lc/hide-debug-windows)
   (dap-session-created . (lambda (_arg) (projectile-save-project-buffers)))
   (dap-ui-repl-mode . (lambda () (setq-local truncate-lines t))))
  :general
  (lc/local-leader-keys
    :states '(normal)
    :keymaps '(python-mode-map dap-ui-repl-mode-map)
    "d d" '(dap-debug :wk "debug")
    "d b" '(dap-breakpoint-toggle :wk "breakpoint toggle")
    "d B" '(dap-ui-breakpoints-list :wk "breakpoint list")
    "d c" '(dap-continue :wk "continue")
    "d n" '(dap-next :wk "next")
    "d e" '(dap-eval-thing-at-point :wk "eval")
    "d i" '(dap-step-in :wk "step in")
    "d l" '(dap-debug-last :wk "step in")
    "d q" '(dap-disconnect :wk "quit")
    "d r" '(dap-ui-repl :wk "repl")
    "d h" '(dap-hydra :wk "hydra")
    "d i" '(lc/dap-inspect-df :wk "view df")
    ;; "d t" '(lc/dap-dtale-df :wk "dtale df")
    )
  (dap-ui-repl-mode-map
   :states '(insert)
   "<up>" 'comint-previous-input
   )
  (:keymaps 'dap-ui-repl-mode-map
            "<backtab>" 'dabbrev-completion
            "TAB" 'lc/py-indent-or-complete)
  :init
  ;; (defun lc/dap-dtale-df (dataframe)
  ;;   "Show df in tale in default browser"
  ;;   (interactive (list (read-from-minibuffer "DataFrame: " (evil-find-symbol nil))))
  ;;   (dap-eval (concat "import dtale; dtale.show(" dataframe ", open_browser=True)")))
  (setq lc/dap-temp-dataframe-buffer  "*inspect-df*")
  (setq lc/dap-temp-dataframe-path "~/tmp-inspect-df.csv")
  (defun lc/dap-inspect-df (dataframe)
    "Save the df to csv and open the file with csv-mode"
    (interactive (list (read-from-minibuffer "DataFrame: " (evil-find-symbol nil))))
    (dap-eval (format  "%s.to_csv('%s', index=False)" dataframe lc/dap-temp-dataframe-path))
    (sleep-for 1)
    (find-file-other-window lc/dap-temp-dataframe-path)
    )
  ;; prevent minibuffer prompt about reloading from disk
  (setq revert-without-query '("~/tmp-inspect-df.csv"))
  ;; (setq dap-auto-configure-features '(locals repl))
  (setq dap-auto-configure-features '(sessions repl))
  (setq dap-python-debugger 'debugpy)
  ;; show stdout
  (setq dap-auto-show-output t)
  (setq dap-output-window-min-height 10)
  (setq dap-output-window-max-height 200)
  (setq dap-overlays-use-overlays nil)
  ;; hide stdout window  when done
  (defun lc/hide-debug-windows (session)
    "Hide debug windows when all debug sessions are dead."
    (unless (-filter 'dap--session-running (dap--get-sessions))
      ;; delete output buffer
      ;; (when-let (window (display-buffer-in-side-window
      ;;         (dap--debug-session-output-buffer (dap--cur-session-or-die))
      ;;         `((side . bottom) (slot . 5) (window-width . 0.20))))
      ;;   (delete-window window))
      (lc/kill-output-buffer)
      ;; delete dataframe inspector window
      ;; (when-let
      ;;     (win (get-buffer-window (get-file-buffer lc/dap-temp-dataframe-path)))
      ;;   (delete-window win))
      )
    )
  (defun lc/dap-python--executable-find (orig-fun &rest args)
    (executable-find "python"))
  (defun lc/kill-output-buffer ()
    "Go to output buffer."
    (interactive)
    (let ((win (display-buffer-in-side-window
                (dap--debug-session-output-buffer (dap--cur-session-or-die))
                `((side . bottom) (slot . 5) (window-width . 0.20)))))
      (delete-window win)))
  (defun lc/window-resize-to-percentage (percentage)
    (interactive)
    (window-resize nil (- (truncate (* percentage (frame-height))) (window-height))))
  (defun lc/reset-dap-windows ()
    (interactive)
    ;; display sessions and repl
    (seq-doseq (feature-start-stop dap-auto-configure-features)
      (when-let
          (start-stop (alist-get feature-start-stop
                                 ;; <
                                 dap-features->windows
                                 ))
        (funcall (car start-stop))))
    ;; display output buffer
    (save-excursion (dap-go-to-output-buffer t))
    ;; resize window
    (save-window-excursion
      ;; switch to main window
      (winum-select-window-1)
      (lc/window-resize-to-percentage 0.66)
      )
    )
  
  :config
  ;; configure windows
  (require 'dap-ui)
  (setq dap-ui-buffer-configurations
        '(("*dap-ui-sessions*"
           (side . bottom)
           (slot . 1)
           (window-height . 0.33))
          ("*debug-window*"
           (side . bottom)
           (slot . 2)
           (window-height . 0.33))
          ("*dap-ui-repl*"
           (side . bottom)
           (slot . 3)
           (window-height . 0.33))))
  (dap-ui-mode 1)
  ;; python virtualenv
  (require 'dap-python)
  (advice-add 'dap-python--pyenv-executable-find :around #'lc/dap-python--executable-find)
  ;; debug templates
  (defvar dap-script-args (list :type "python"
                                :args []
                                :cwd "${workspaceFolder}"
                                :justMyCode :json-false
                                :request "launch"
                                :debugger 'debugpy
                                :name "dap-debug-script"))
  (defvar dap-test-args (list :type "python-test-at-point"
                              :args ""
                              :justMyCode :json-false
                              ;; :cwd "${workspaceFolder}"
                              :request "launch"
                              :module "pytest"
                              :debugger 'debugpy
                              :name "dap-debug-test-at-point"))
  (defvar eco-cold-start (list
                          :name "mill"
                          :type "python"
                          :request "launch"
                          :program (expand-file-name "~/git/ran_optimization/scripts_smart_sleep_orchestration/find_cold_start_smart_sleep_thresholds.py")
                          ;; :env '(("NO_JSON_LOG" . "true"))
                          ;; :args ["-m" "mill" "--config" "user_luca"]
                          ))

  (dap-register-debug-template "dap-debug-script" dap-script-args)
  (dap-register-debug-template "dap-debug-test-at-point" dap-test-args)
  (dap-register-debug-template "eco-cold-start" eco-cold-start)
  ;; bind the templates
  (lc/local-leader-keys
    :keymaps 'python-mode-map
    "d t" '((lambda () (interactive) (dap-debug dap-test-args)) :wk "test")
    "d s" '((lambda () (interactive) (dap-debug dap-script-args)) :wk "script")
    )
  )
;; dap-mode:1 ends here

;; [[file:../readme.org::#h:E80DEB4B-6AC9-415D-AF36-0044479D1B5A][init-prog-lsp:1]]
(provide 'init-prog-lsp)
;;; init-prog-lsp.el ends here
;; init-prog-lsp:1 ends here
