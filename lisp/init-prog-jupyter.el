(use-package emacs
  :hook
  ((org-jupyter-mode . (lambda () (visual-line-mode -1)
												 (advice-add 'org-cycle :around #'lc/org-cycle-or-py-complete)))
   (org-mode . (lambda () (when (lc/is-jupyter-org-buffer?) (org-jupyter-mode)))))
  :init
  (defun lc/is-jupyter-org-buffer? ()
    (with-current-buffer (buffer-name)
      (goto-char (point-min))
      (re-search-forward "begin_src jupyter-" 10000 t)))
  (defun lc/org-cycle-or-py-complete (orig-fun &rest args)
    "If in a jupyter-python code block, call py-indent-or-complete, otherwise use org-cycle"
    (if (and (org-in-src-block-p)
             (eq (intern (org-element-property :language (org-element-at-point))) 'jupyter-python))
        (lc/py-indent-or-complete)
      (apply orig-fun args)))
  (define-minor-mode org-jupyter-mode
    "Minor mode which is active when an org file has the string begin_src jupyter-python
    in the first few hundred rows"
    ;; :keymap (let ((map (make-sparse-keymap)))
    ;;             (define-key map (kbd "C-c f") 'insert-foo)
    ;;             map)
    )
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

#+HTML_HEAD: <link rel="stylesheet" type="text/css" href="https://gongzhitaao.org/orgcss/org.css"/>

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

(provide 'init-prog-jupyter)
;;; init-prog-jupyter.el ends here
