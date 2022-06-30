;; [[file:../readme.org::#h:C757468F-281B-4A9D-84AF-26B9B656F01C][Clojure mode:1]]
(use-package clojure-mode
  :mode "\\.clj$"
  :init
  (setq clojure-align-forms-automatically t)
	)
;; Clojure mode:1 ends here

;; [[file:../readme.org::#h:1613B1A4-02B4-4EB0-AC4B-53FA0BF3CF60][clojure-lsp:1]]
(use-package clojure-mode
  :hook
  ((clojure-mode clojurescript-mode)
   . (lambda ()
       (setq-local lsp-enable-indentation nil ; cider indentation
                   lsp-enable-completion-at-point nil ; cider completion
                   )
       (lsp-deferred)))
  )
;; clojure-lsp:1 ends here

;; [[file:../readme.org::#h:3D8B9BAA-05E9-4F0B-8988-4B56882BB607][Cider:1]]
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
;; Cider:1 ends here

;; [[file:../readme.org::#h:3B21567B-6B7B-4747-B44B-AA2480FA49E5][ob-clojure:1]]
(use-package org
  :config
  (require 'ob-clojure)
  (setq org-babel-clojure-backend 'cider)
  )
;; ob-clojure:1 ends here

;; [[file:../readme.org::#h:804F32D8-081E-427C-8949-90D3ECF5879F][aggressive-indent:1]]
;; keep the file indented
(use-package aggressive-indent
  :hook ((clojure-mode . aggressive-indent-mode)
         (emacs-lisp-mode . aggressive-indent-mode)))
;; aggressive-indent:1 ends here

;; [[file:../readme.org::#h:39D468C7-8D6B-4F16-A3FE-8675917B3DBD][init-prog-clojure:1]]
(provide 'init-prog-clojure)
;;; init-org-prog-clojure.el ends here
;; init-prog-clojure:1 ends here
