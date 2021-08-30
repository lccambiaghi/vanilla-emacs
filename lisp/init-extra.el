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
  (setq lsp-diagnostics-provider :flymake)
  )

(use-package lsp-ui
  :hook
  ((lsp-mode . lsp-ui-mode)
   ;; (lsp-mode . (lambda () (setq-local evil-goto-definition-functions '(lambda (&rest args) (lsp-ui-peek-find-definitions)))))
   )
  ;; :bind
  ;; (:map lsp-ui-mode-map
  ;;       ([remap lsp-find-references] . lsp-ui-peek-find-references))
  :general
  (lc/local-leader-keys
    "h" 'lsp-ui-doc-show
    "H" 'lsp-ui-doc-hide)
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

(use-package dap-mode
  :hook
  ((dap-terminated . lc/hide-debug-windows)
	 (dap-ui-repl-mode . (lambda () (setq-local truncate-lines t))))
  :general
  (lc/local-leader-keys
    :keymaps 'python-mode-map
    "d d" '(dap-debug :wk "debug")
    "d b" '(dap-breakpoint-toggle :wk "breakpoint")
    "d c" '(dap-continue :wk "continue")
    "d n" '(dap-next :wk "next")
    "d e" '(dap-eval-thing-at-point :wk "eval")
    "d i" '(dap-step-in :wk "step in")
    "d q" '(dap-disconnect :wk "quit")
    "d r" '(dap-ui-repl :wk "repl")
    "d h" '(dap-hydra :wk "hydra"))
	(:keymaps 'dap-ui-repl-mode-map
	 "TAB" 'lc/py-indent-or-complete)
  :init
  ;; (setq dap-auto-configure-features '(locals repl))
  (setq dap-auto-configure-features '(repl))
  (setq dap-python-debugger 'debugpy)
  ;; show stdout
  (setq dap-auto-show-output t)
  (setq dap-output-window-max-height 20)
  (setq dap-output-window-min-height 10)
  (setq dap-overlays-use-overlays nil)
  ;; hide stdout window  when done
  (defun lc/hide-debug-windows (session)
    "Hide debug windows when all debug sessions are dead."
    (unless (-filter 'dap--session-running (dap--get-sessions))
      (kill-buffer (dap--debug-session-output-buffer (dap--cur-session-or-die)))))
  (defun lc/dap-python--executable-find (orig-fun &rest args)
    (executable-find "python"))
  :config
  ;; configure windows
  (require 'dap-ui)
  (setq dap-ui-buffer-configurations
        `(;; (,dap-ui--locals-buffer . ((side . right) (slot . 1) (window-width . 0.50)))
          ;; (,dap-ui--breakpoints-buffer . ((side . left) (slot . 1) (window-width . ,treemacs-width)))
          ;; (,dap-ui--sessions-buffer . ((side . left) (slot . 2) (window-width . ,treemacs-width)))
          (,dap-ui--repl-buffer . ((side . right) (slot . 2) (window-width . 0.50)))))
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
  (defvar empties-forecast (list
                            :name "empties forecast"
                            :type "python"
                            :request "launch"
                            :program "./src/empties/forecasting/predict.py"
                            :env '(("NO_JSON_LOG" . "true"))
                            :args ["--source01" "./data/empties-history-sample.parquet"
                                   "--source02" "./data/model_selection.files"
                                   "--source03" "./data/booking-feature-sample.parquet"
                                   "--source04" "./data/holiday-2019-05-24-1558683595"
                                   "--output-data" "./data/predictions.parquet"
                                   "--output-metrics" "./data/metrics.json"]
                            ))
  (dap-register-debug-template "dap-debug-script" dap-script-args)
  (dap-register-debug-template "dap-debug-test-at-point" dap-test-args)
  ;; bind the templates
  (lc/local-leader-keys
    :keymaps 'python-mode-map
    "d t" '((lambda () (interactive) (dap-debug dap-test-args)) :wk "test")
    "d s" '((lambda () (interactive) (dap-debug dap-script-args)) :wk "script")
    )
  )

(use-package python-mode
  :hook ((envrc-mode . (lambda ()
                         (when (executable-find "ipython")
                           (setq python-shell-interpreter (executable-find "ipython"))))))
  :general
  (python-mode-map
   :states 'normal
   "gz" nil
   "C-j" nil)
  (python-mode-map
   :states 'insert
   "TAB" 'lc/py-indent-or-complete
   )
  :init
  (setq python-indent-offset 0)
  (defun lc/py-indent-or-complete ()
    (interactive "*")
    (window-configuration-to-register py--windows-config-register)
    (cond ((use-region-p)
           (py-indent-region (region-beginning) (region-end)))
          ((or (bolp)   
               (member (char-before) (list 9 10 12 13 32 ?:  ;; ([{
																					 ?\) ?\] ?\}))
               ;; (not (looking-at "[ \t]*$"))
							 )
           (py-indent-line))
          ((comint-check-proc (current-buffer))
           (ignore-errors (completion-at-point)))
          (t
           (completion-at-point))))
  :config
  (setq python-shell-interpreter (executable-find "ipython")     ;; FIXME
        python-shell-interpreter-args "-i --simple-prompt --no-color-info"
        python-shell-prompt-regexp "In \\[[0-9]+\\]: "
        python-shell-prompt-block-regexp "\\.\\.\\.\\.: "
        python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
        python-shell-completion-setup-code
        "from IPython.core.completerlib import module_completion"
        python-shell-completion-string-code
        "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"))

(use-package lsp-pyright
  :init
  (setq lsp-pyright-typechecking-mode "basic") ;; too much noise in "real" projects
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))

(use-package python-pytest
  :general
  (lc/local-leader-keys
    :keymaps 'python-mode-map
		"t" '(:ignore t :wk "test")
    "t d" '(python-pytest-dispatch :wk "dispatch")
    "t f" '(python-pytest-file :wk "file")
    "t t" '(python-pytest-function :wk "function"))
  :init
  (setq python-pytest-arguments '("--color" "--failed-first"))
  (defun lc/pytest-use-venv (orig-fun &rest args)
    (if-let ((python-pytest-executable (executable-find "pytest")))
        (apply orig-fun args)
      (apply orig-fun args)))
  :config
  (advice-add 'python-pytest--run :around #'lc/pytest-use-venv)
  )

(use-package flymake
  :straight (:type built-in)
  :hook (emacs-lisp-mode . flymake-mode)
  :init
  (setq python-flymake-command (executable-find "flake8"))
  (setq flymake-fringe-indicator-position 'right-fringe)
  :general
  (general-nmap "] !" 'flymake-goto-next-error)
  (general-nmap "[ !" 'flymake-goto-prev-error)
  )

(use-package jupyter
  :straight (:no-native-compile t :no-byte-compile t) ;; otherwise we get jupyter-channel void
  :general
  (lc/local-leader-keys
    :keymaps 'python-mode-map
    "e" '(:ignore true :wk "eval")
    "e e" '(jupyter-eval-line-or-region :wk "line")
    "e d" '(jupyter-eval-defun :wk "defun")
    "e b" '((lambda () (interactive) (lc/jupyter-eval-buffer)) :wk "buffer")
    "J" '(lc/jupyter-repl :wk "jupyter REPL")
    "k" '(:ignore true :wk "kernel"))
  (lc/local-leader-keys
    :keymaps 'python-mode-map
    :states 'visual
    "e" '(jupyter-eval-region :wk "eval"))
	(lc/local-leader-keys
    :keymaps 'jupyter-org-interaction-mode-map
    :states 'normal
    "k i" '(jupyter-org-interrupt-kernel :wk "interrupt")
    "k r" '(jupyter-repl-restart-kernel :wk "restart"))
  :init
  (setq jupyter-repl-prompt-margin-width 4)
  (defun jupyter-command-venv (&rest args)
    "This overrides jupyter-command to use the virtualenv's jupyter"
    (let ((jupyter-executable (executable-find "jupyter")))
      (with-temp-buffer
        (when (zerop (apply #'process-file jupyter-executable nil t nil args))
          (string-trim-right (buffer-string))))))
  (defun lc/jupyter-eval-buffer ()
    "Send the contents of BUFFER using `jupyter-current-client'."
    (interactive)
    (jupyter-eval-string (jupyter-load-file-code (buffer-file-name))))
  (defun lc/jupyter-repl ()
    "If a buffer is already associated with a jupyter buffer, then pop to it. Otherwise start a jupyter kernel."
    (interactive)
    (if (bound-and-true-p jupyter-current-client)
        (jupyter-repl-pop-to-buffer)
      (call-interactively 'jupyter-repl-associate-buffer)))
  (advice-add 'jupyter-command :override #'jupyter-command-venv))

(use-package jupyter
  :straight (:no-native-compile t :no-byte-compile t) ;; otherwise we get jupyter-channel void
  :general
  (lc/local-leader-keys
    :keymaps 'org-mode-map
    "=" '((lambda () (interactive) (jupyter-org-insert-src-block t nil)) :wk "block below")
    "m" '(jupyter-org-merge-blocks :wk "merge")
    "+" '(jupyter-org-insert-src-block :wk "block above")
    "?" '(jupyter-inspect-at-point :wk "inspect")
    "x" '(jupyter-org-kill-block-and-results :wk "kill block"))
  :hook ((jupyter-org-interaction-mode . (lambda () (lc/add-local-electric-pairs '((?' . ?')))))
         (jupyter-repl-persistent-mode . (lambda ()  ;; we activate org-interaction-mode ourselves
                                           (when (derived-mode-p 'org-mode)
                                             ;; (setq-local company-backends '((company-capf)))
																						 (setq-local evil-lookup-func #'jupyter-inspect-at-point)
                                             (jupyter-org-interaction-mode))))
				 (envrc-mode . lc/load-ob-jupyter))
  :init
  (setq org-babel-default-header-args:jupyter-python '((:async . "yes")
                                                       (:pandoc t)
                                                       (:kernel . "python3")))
  (setq org-babel-default-header-args:jupyter-R '((:pandoc t)
                                                  (:async . "yes")
                                                  (:kernel . "ir")))
	(defun lc/org-load-jupyter ()
    (org-babel-do-load-languages 'org-babel-load-languages
                                 (append org-babel-load-languages
                                         '((jupyter . t)))))
  (defun lc/load-ob-jupyter ()
    ;; only try to load in org-mode
    (when (derived-mode-p 'org-mode)
      ;; skip if already loaded
      (unless (member '(jupyter . t) org-babel-load-languages)
        ;; only load if jupyter is available
        (when (executable-find "jupyter")
					(lc/org-load-jupyter)))))
  :config
	(cl-defmethod jupyter-org--insert-result (_req context result)
    (let ((str
           (org-element-interpret-data
            (jupyter-org--wrap-result-maybe
             context (if (jupyter-org--stream-result-p result)
                         (thread-last result
                           jupyter-org-strip-last-newline
                           jupyter-org-scalar)
                       result)))))
      (if (< (length str) 100000)  ;; >
          (insert str)
        (insert (format ": Result was too long! Length was %d" (length str)))))
    (when (/= (point) (line-beginning-position))
      ;; Org objects such as file links do not have a newline added when
      ;; converting to their string representation by
      ;; `org-element-interpret-data' so insert one in these cases.
      (insert "\n")))
  ;;Remove text/html since it's not human readable
  ;; (delete :text/html jupyter-org-mime-types)
  ;; (with-eval-after-load 'org-src
  ;;   (add-to-list 'org-src-lang-modes '("jupyter-python" . python))
  ;;   (add-to-list 'org-src-lang-modes '("jupyter-R" . R)))
	)

(use-package pyimport
  :general
  (lc/local-leader-keys
    :keymaps 'python-mode-map
    "i i" '(pyimport-insert-missing :wk "autoimport")))

(use-package blacken
	:general
	(lc/local-leader-keys
      :keymaps 'python-mode-map
      "=" '(blacken-buffer :wk "format"))
	)

(use-package toml-mode
	:mode "\\.toml\\'")

(use-package ess
  :general
  (lc/local-leader-keys
    :keymaps 'ess-r-mode-map
    :states 'normal
    "R" '(R :wk "R")
    "q" '(ess-quit :wk "quit")
    "RET" '(ess-eval-line-visibly-and-step :wk "line and step")
    ;; debug
    "d b" '(ess-bp-set :wk "breakpoint")
    "d n" '(ess-debug-command-next :wk "next")
    "d q" '(ess-debug-command-quit :wk "quit")
    "d c" '(ess-bp-next :wk "continue")
    "d f" '(ess-debug-flag-for-debugging :wk "flag function")
    "d F" '(ess-debug-unflag-for-debugging :wk "unflag function")
    "d p" '(ess-debug-goto-debug-point :wk "go to point")
    ;; "e l" '(ess-eval-line :wk "eval line")
    "e p" '(ess-eval-paragraph :wk "paragraph")
    "e f" '(ess-eval-function :wk "function")
    "h" '(:keymap ess-doc-map :which-key "help")
    ;; "h" '(ess-display-help-on-object :wk "help")
    )
  (lc/local-leader-keys
    :keymaps 'ess-r-mode-map
    :states 'visual
    "RET" '(ess-eval-region-or-line-visibly-and-step :wk "line and step"))
  :init
  (setq ess-eval-visibly 'nowait)
  (setq ess-R-font-lock-keywords '((ess-R-fl-keyword:keywords . t)
                                   (ess-R-fl-keyword:constants . t)
                                   (ess-R-fl-keyword:modifiers . t)
                                   (ess-R-fl-keyword:fun-defs . t)
                                   (ess-R-fl-keyword:assign-ops . t)
                                   (ess-R-fl-keyword:%op% . t)
                                   (ess-fl-keyword:fun-calls . t)
                                   (ess-fl-keyword:numbers . t)
                                   (ess-fl-keyword:operators . t)
                                   (ess-fl-keyword:delimiters . t)
                                   (ess-fl-keyword:= . t)
                                   (ess-R-fl-keyword:F&T . t)))
	;; (setq ess-first-continued-statement-offset 2
  ;;         ess-continued-statement-offset 0
  ;;         ess-expression-offset 2
  ;;         ess-nuke-trailing-whitespace-p t
  ;;         ess-default-style 'DEFAULT)
	;; (setq ess-r-flymake-linters "line_length_linter = 120")
  )

(use-package ess-view-data
  :general
  (lc/local-leader-keys
    :keymaps 'ess-r-mode-map
    :states 'normal
    "hd" 'ess-R-dv-pprint
    "ht" 'ess-R-dv-ctable
    ))

(use-package lsp-mode
  :hook
  (ess-r-mode . lsp-deferred)
	)

(use-package evil-lisp-state
  :after evil
  :demand
  :init
  (setq evil-lisp-state-enter-lisp-state-on-command nil)
  (setq evil-lisp-state-global t)
  ;; (setq evil-lisp-state-major-modes '(org-mode emacs-lisp-mode clojure-mode clojurescript-mode lisp-interaction-mode))
  :config
  (evil-lisp-state-leader "SPC l")
  )

(use-package eros
  :hook ((emacs-lisp-mode org-mode lisp-interaction-mode) . eros-mode)
  :general
  (lc/local-leader-keys
    :keymaps '(org-mode-map emacs-lisp-mode-map lisp-interaction-mode-map)
    :states 'normal
    "e l" '(eros-eval-last-sexp :wk "last sexp")
    ;; "e d" '((lambda () (interactive) (eros-eval-defun t)) :wk "defun")
    "e b" '(eval-buffer :wk "buffer"))
  (lc/local-leader-keys
    :keymaps '(org-mode-map emacs-lisp-mode-map lisp-interaction-mode-map)
    :states 'visual
    "e" '((lambda (start end)
            (interactive (list (region-beginning) (region-end)))
            (eval-region start end t))
          :wk "region")
    ;; "e" '((lambda (start end)
    ;;         (interactive (list (region-beginning) (region-end)))
    ;;         (eros--eval-overlay
    ;;          (eval-region start end t)
    ;;          end))
    ;;       :wk "region")
    )
  )

(use-package nix-mode
:mode "\\.nix\\'")

(use-package clojure-mode
  :mode "\\.clj$"
  :init
  (setq clojure-align-forms-automatically t))

(use-package clojure-mode
  :hook
  ((clojure-mode clojurescript-mode)
   . (lambda ()
       (setq-local lsp-enable-indentation nil ; cider indentation
                   lsp-enable-completion-at-point nil ; cider completion
                   )
       (lsp-deferred)))
  )

(use-package cider
  :hook ((cider-repl-mode . evil-normalize-keymaps)
         (cider-mode . (lambda ()
                           (setq-local evil-lookup-func #'cider-doc)))
         (cider-mode . eldoc-mode))
  :general
  (lc/local-leader-keys
    :keymaps 'clojure-mode-map
    "c" '(cider-connect-clj :wk "connect")
    "C" '(cider-connect-cljs :wk "connect (cljs)")
    "j" '(cider-jack-in :wk "jack in")
    "J" '(cider-jack-in-cljs :wk "jack in (cljs)")
    "d d" 'cider-debug-defun-at-point 
    "e b" 'cider-eval-buffer
    "e l" 'cider-eval-last-sexp
    "e L" 'cider-pprint-eval-last-sexp-to-comment
    "e d" '(cider-eval-defun-at-point :wk "defun")
    "e D" 'cider-pprint-eval-defun-to-comment
		"h" 'cider-clojuredocs-web 
		"K" 'cider-doc
		"q" '(cider-quit :qk "quit")
		)
  (lc/local-leader-keys
    :keymaps 'clojure-mode-map
    :states 'visual
    "e" 'cider-eval-region)
  :init
  (setq nrepl-hide-special-buffers t)
  (setq nrepl-sync-request-timeout nil)
	(setq cider-repl-display-help-banner nil)
  )

(use-package cider
  :init
  (defun mpereira/cider-eval-sexp-at-point (&optional output-to-current-buffer)
    "Evaluate the expression around point.
If invoked with OUTPUT-TO-CURRENT-BUFFER, output the result to current buffer."
    (interactive "P")
    (save-excursion
      (goto-char (- (cadr (cider-sexp-at-point 'bounds))
                    1))
      (cider-eval-last-sexp output-to-current-buffer)))
  )

(use-package org
:config
(require 'ob-clojure)
(setq org-babel-clojure-backend 'cider))

;; keep the file indented
(use-package aggressive-indent
  :hook ((clojure-mode . aggressive-indent-mode)
         (emacs-lisp-mode . aggressive-indent-mode)))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package xwwp
  :straight (xwwp :type git :host github :repo "canatella/xwwp")
  :commands (xwwp)
  :general
  (lc/leader-keys
    "x x" '((lambda () (interactive)
              (let ((current-prefix-arg 4)) ;; emulate C-u universal arg
                (call-interactively 'xwwp)))
            :wk "search or visit")
    "x l" '(xwwp-follow-link :wk "link")
    "x b" '(xwidget-webkit-back :wk "back"))
  :init
  ;; :custom
  ;; (setq xwwp-follow-link-completion-system 'ivy)
  ;; :bind (:map xwidget-webkit-mode-map
  ;;             ("v" . xwwp-follow-link))
  )

(use-package elfeed
  :straight (elfeed :type git :host github :repo "skeeto/elfeed")
  :hook (elfeed-search-mode . elfeed-update)
  :general
  (lc/leader-keys
    "s r" '(elfeed :wk "elfeed"))
  (general-nmap
		:keymaps 'elfeed-search-mode-map
   "x" 'lc/elfeed-xwwp-open)
  :init
  (defun lc/elfeed-xwwp-open (&optional use-generic-p)
    "open with eww"
    (interactive "P")
    (let ((entries (elfeed-search-selected)))
      (cl-loop for entry in entries
               do (elfeed-untag entry 'unread)
               when (elfeed-entry-link entry)
               do (xwwp it))
      (mapc #'elfeed-search-update-entry entries)
      (unless (use-region-p) (forward-line))))
  :config
  (setq elfeed-feeds'(("https://www.reddit.com/r/emacs.rss?sort=new" reddit emacs)
                      ("http://emacsredux.com/atom.xml" emacs)
                      ("http://irreal.org/blog/?tag=emacs&amp;feed=rss2" emacs)
                      ("https://www.reddit.com/search.rss?q=url%3A%28youtu.be+OR+youtube.com%29&sort=top&t=week&include_over_18=1&type=link"
                      reddit youtube popular))))

(provide 'init-extra)
;;; init-extra.el ends here
