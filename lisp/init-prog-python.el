(use-package python-mode
  :hook
	((envrc-mode . (lambda ()
									 (when (executable-find "ipython")
                           (setq python-shell-interpreter (executable-find "ipython"))))))
  :general
  (python-mode-map
   :states 'normal
   "gz" nil
   "C-j" nil)
  (python-mode-map
   :states 'insert
   "TAB" 'lc/py-indent-or-complete)
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
  (setq python-shell-interpreter-args "-i --simple-prompt --no-color-info"
        python-shell-prompt-regexp "In \\[[0-9]+\\]: "
        python-shell-prompt-block-regexp "\\.\\.\\.\\.: "
        python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
        python-shell-completion-setup-code
        "from IPython.core.completerlib import module_completion"
        python-shell-completion-string-code
        "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
	)

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

(use-package flycheck
	:hook ((lsp-mode . flycheck-mode)
				 (envrc-mode . (lambda ()
                        (setq flycheck-python-flake8-executable (executable-find "python"))
												(setq flycheck-checker 'python-flake8)
												)))
	:init
	(setq flycheck-indication-mode 'right-fringe)
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
  ;; (lc/local-leader-keys
  ;;   :keymaps 'python-mode-map
  ;;   :states 'visual
  ;;   "e" '(jupyter-eval-region :wk "eval"))
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

(use-package csv-mode
	:hook (csv-mode . csv-align-visible)
  :general
  (lc/local-leader-keys
    :keymaps 'csv-mode-map
    :states 'normal
    "a" '(csv-align-visible :wk "align visible")
    "s" '(csv-sort-fields :wk "sort fields")
    )
  :init
  (defun csv-align-visible (&optional arg)
    "Align visible fields"
    (interactive "P")
    (csv-align-fields nil (window-start) (window-end))
    )
  )

(provide 'init-prog-python)
;;; init-prog-python.el ends here
