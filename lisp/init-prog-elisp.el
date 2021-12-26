(use-package emacs
  :straight (:type built-in)
	:general
	(general-nmap
		:keymaps 'emacs-lisp-mode-map
    :states 'normal
   "gr" nil) ;; interferes with eval-operator
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
    ;; "e" '((lambda (start end)
    ;;         (interactive (list (region-beginning) (region-end)))
    ;;         (eval-region start end t))
    ;;       :wk "region")
    ;; "e" '((lambda (start end)
    ;;         (interactive (list (region-beginning) (region-end)))
    ;;         (eros--eval-overlay
    ;;          (eval-region start end)
    ;;          end))
    ;;       :wk "region")
		"e" '(eros-eval-region :wk "region")
    )
	:init
  (defun eros-eval-region (start end)
    (interactive "r")
    (eros--eval-overlay
     (string-trim
      (with-output-to-string
        (eval-region start end standard-output)))
     (max (point) (mark))))
  )

(provide 'init-prog-elisp)
;;; init-org-prog-elisp.el ends here
