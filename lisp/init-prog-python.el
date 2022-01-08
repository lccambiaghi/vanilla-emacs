(use-package python-mode
  :hook
  ((envrc-mode . (lambda ()
                   (when (executable-find "ipython")
                     (setq python-shell-interpreter (executable-find "ipython"))))))
  :general
  (lc/local-leader-keys
    :keymaps 'python-mode-map
    "'" 'run-python)
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
                        (setq flycheck-flake8rc ".flake8")
												)))
	:init
	(setq flycheck-indication-mode 'right-fringe)
	;; only check on save
	(setq flycheck-check-syntax-automatically '(mode-enabled save))
)

(use-package blacken
	:general
	(lc/local-leader-keys
      :keymaps 'python-mode-map
      "=" '(blacken-buffer :wk "format"))
	)

(use-package csv-mode
  :hook (csv-mode . lc/init-csv-mode)
  :general
  (lc/local-leader-keys
    :keymaps 'csv-mode-map
    :states 'normal
    "a" '(csv-align-fields :wk "align fields")
    "A" '(lc/csv-align-visible :wk "align fields, visible")
    "i"	 '(lc/init-csv-mode :wk "init csv mode")
    "u" '(csv-unalign-fields :wk "unalign fields")
    "s" '(csv-sort-fields :wk "sort fields")
    ";" '(lc/set-csv-semicolon-separator :wk "set semicolon sep")
    "," '(lc/reset-csv-separators :wk "set comma sep"))
  :init
  (defun lc/csv-align-visible (&optional arg)
    "Align visible fields"
    (interactive "P")
    (csv-align-fields nil (window-start) (window-end)))
  (defun lc/set-csv-semicolon-separator ()
    (interactive)
    (customize-set-variable 'csv-separators '(";")))
  (defun lc/reset-csv-separators ()
    (interactive)
    (customize-set-variable 'csv-separators lc/default-csv-separators))
  (defun lc/init-csv-mode ()
		(interactive)
    (lc/set-csv-separators)
    (lc/csv-highlight)
    (call-interactively 'csv-align-fields))
  :config
  (require 'cl)
  (require 'color)
  (defun lc/set-csv-separators ()
    (interactive)
    (let* ((n-commas (count-matches "," (point-at-bol) (point-at-eol)))
           (n-semicolons (count-matches ";" (point-at-bol) (point-at-eol))))
      (if ( ; <
           > n-commas n-semicolons)
          (customize-set-variable 'csv-separators '("," "	"))		
        (customize-set-variable 'csv-separators '(";" "	")))))
  (defun lc/csv-highlight ()
    (interactive)
    (font-lock-mode 1)
    (let* ((separator (string-to-char (car csv-separators)))
           (n (count-matches (string separator) (point-at-bol) (point-at-eol)))
           (colors (loop for i from 0 to 1.0 by (/ 2.0 n)
                         collect (apply #'color-rgb-to-hex 
                                        (color-hsl-to-rgb i 0.3 0.5)))))
      (loop for i from 2 to n by 2 
            for c in colors
            for r = (format "^\\([^%c\n]+%c\\)\\{%d\\}" separator separator i)
            do (font-lock-add-keywords nil `((,r (1 '(face (:foreground ,c)))))))))
  )

(provide 'init-prog-python)
;;; init-prog-python.el ends here
