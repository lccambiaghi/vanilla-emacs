;; [[file:../readme.org::#h:06BE4AA2-688D-4748-BF6D-9656EF6ED767][ess:1]]
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
;; ess:1 ends here

;; [[file:../readme.org::#h:9838BF54-50A8-4E07-8639-184253159FC3][ESS data viewer:1]]
(use-package ess-view-data
  :general
  (lc/local-leader-keys
    :keymaps 'ess-r-mode-map
    :states 'normal
    "hd" 'ess-R-dv-pprint
    "ht" 'ess-R-dv-ctable
    ))
;; ESS data viewer:1 ends here

;; [[file:../readme.org::#h:4492B9C6-4E27-4A4D-B927-13D3DDD44706][enable lsp-mode:1]]
(use-package lsp-mode
  :hook
  (ess-r-mode . lsp-deferred)
	)
;; enable lsp-mode:1 ends here

;; [[file:../readme.org::#h:A480FCCD-E2E4-4BBA-ACA1-DEF5EFDC1D03][init-prog-r:1]]
(provide 'init-prog-r)
;;; init-org-prog-r.el ends here
;; init-prog-r:1 ends here
