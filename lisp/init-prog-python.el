;; [[file:../readme.org::#h:95658AC9-ADFE-4134-81C4-DEC01069B716][python mode:1]]
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
;; python mode:1 ends here

;; [[file:../readme.org::#h:373160D0-6591-4D96-9928-33EE610CEACE][lsp-pyright:1]]
(use-package lsp-pyright
  :init
  (setq lsp-pyright-typechecking-mode "basic") ;; too much noise in "real" projects
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))
;; lsp-pyright:1 ends here

;; [[file:../readme.org::#h:8C50BED5-6727-41D9-83BE-E5ADB96D7C88][pytest:1]]
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
;; pytest:1 ends here

;; [[file:../readme.org::#h:A98624AB-1B3D-47A2-873F-E961FDE431D9][flycheck:1]]
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
;; flycheck:1 ends here

;; [[file:../readme.org::#h:2DC8F5D9-BD12-4CEA-9CBD-34B4CAC53495][blacken:1]]
(use-package blacken
	:general
	(lc/local-leader-keys
      :keymaps 'python-mode-map
      "=" '(blacken-buffer :wk "format"))
	)
;; blacken:1 ends here

;; [[file:../readme.org::#h:A0F0925F-11FB-43A8-8E26-A068FD70D7D0][csv mode:1]]
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
;; csv mode:1 ends here

;; [[file:../readme.org::#h:72656E4E-C0B1-49E4-92AB-961F08655435][code-cells:1]]
(use-package code-cells
  :hook (python-mode . code-cells-mode)
	:config
	(let ((map code-cells-mode-map))
    (define-key map [remap evil-search-next] (code-cells-speed-key 'code-cells-forward-cell)) ;; n
    (define-key map [remap evil-paste-after] (code-cells-speed-key 'code-cells-backward-cell)) ;; p
    (define-key map [remap evil-backward-word-begin] (code-cells-speed-key 'code-cells-eval-above)) ;; b
    (define-key map [remap evil-forward-word-end] (code-cells-speed-key 'code-cells-eval)) ;; e
    (define-key map [remap evil-jump-forward] (code-cells-speed-key 'outline-cycle))) ;; TAB
	)
;; code-cells:1 ends here

;; [[file:../readme.org::#h:E80DEB4B-6AC9-415D-AF36-0044479D1B5A][init-prog-python:1]]
(provide 'init-prog-python)
;;; init-prog-python.el ends here
;; init-prog-python:1 ends here
