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
