;; [[file:../readme.org::#h:DE534B1B-9DF0-4966-A21C-B65B76B6A64E][org-jupyter-mode:1]]
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
;; org-jupyter-mode:1 ends here

;; [[file:../readme.org::#h:388AA3FF-BC18-4628-93DC-9292B568AE9F][jupyter:1]]
(use-package jupyter
  :straight (:no-native-compile t :no-byte-compile t) ;; otherwise we get jupyter-channel void
  :general
  (lc/local-leader-keys
    :keymaps '(jupyter-org-interaction-mode-map jupyter-repl-interaction-mode-map)
    :states 'normal
    "e" '(:ignore true :wk "eval")
    "e e" '(jupyter-eval-line-or-region :wk "line")
    "e d" '(jupyter-eval-defun :wk "defun")
    "e b" '((lambda () (interactive) (lc/jupyter-eval-buffer)) :wk "buffer")
    "e r" '(jupyter-eval-remove-overlays :wk "remove overlays")
    "k" '(:ignore true :wk "kernel")
    "k d" '(lc/kill-repl-kernel :wk "kill")
    "k i" '(jupyter-org-interrupt-kernel :wk "interrupt")
    "k r" '(jupyter-repl-restart-kernel :wk "restart")
    "J" '(lc/jupyter-repl :wk "jupyter REPL")
    )
  (lc/local-leader-keys
    :keymaps '(jupyter-org-interaction-mode-map jupyter-repl-interaction-mode-map)
    :states 'visual
    "e" '(jupyter-eval-line-or-region :wk "region")
    )
  :init
  (setq jupyter-repl-prompt-margin-width 4)
  (setq jupyter-eval-use-overlays t)
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
  (defun lc/kill-repl-kernel ()
    "Kill repl buffer associated with current jupyter kernel"
    (interactive)
    (if jupyter-current-client
        (jupyter-with-repl-buffer jupyter-current-client
          (kill-buffer (current-buffer)))
      (error "Buffer not associated with a REPL, see `jupyter-repl-associate-buffer'"))
    )
  (advice-add 'jupyter-command :override #'jupyter-command-venv)
  ;; TODO refactor to avoid duplication of dap code
  (setq lc/jupyter-temp-dataframe-buffer  "*inspect-df*")
  (setq lc/jupyter-temp-dataframe-path "~/tmp-inspect-df.csv")
  (defun lc/jupyter-inspect-df (dataframe)
    "Save the df to csv and open the file with csv-mode"
    (interactive (list (read-from-minibuffer "DataFrame: " (evil-find-symbol nil))))
    (jupyter-eval (format  "%s.to_csv('%s', index=False)" dataframe lc/jupyter-temp-dataframe-path))
    (find-file-other-window lc/jupyter-temp-dataframe-path)
    )
  )
;; jupyter:1 ends here

;; [[file:../readme.org::#h:600809B3-9B92-43F9-9A99-3D007CEE044D][ob-jupyter:2]]
(use-package jupyter
  :straight (:no-native-compile t :no-byte-compile t) ;; otherwise we get jupyter-channel void
  :general
  (lc/local-leader-keys
    :keymaps 'org-mode-map
    "=" '((lambda () (interactive) (jupyter-org-insert-src-block t nil)) :wk "block below")
    "," 'org-ctrl-c-ctrl-c
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
;; ob-jupyter:2 ends here

;; [[file:../readme.org::#h:FD3CCF1B-8F23-4863-8592-0AF46542AB21][init-prog-jupyter:1]]
(provide 'init-prog-jupyter)
;;; init-prog-jupyter.el ends here
;; init-prog-jupyter:1 ends here
