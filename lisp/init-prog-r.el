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

(provide 'init-prog-r)
;;; init-org-prog-r.el ends here
