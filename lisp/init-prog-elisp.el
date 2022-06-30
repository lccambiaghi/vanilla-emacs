;; [[file:../readme.org::#h:96EF892B-7F9F-4F69-B72C-C01F011A054D][emacs-lisp-mode:1]]
(use-package emacs
  :straight (:type built-in)
	:general
	(general-nmap
		:keymaps 'emacs-lisp-mode-map
    :states 'normal
   "gr" nil) ;; interferes with eval-operator
	)
;; emacs-lisp-mode:1 ends here

;; [[file:../readme.org::#h:C59E740E-F00B-4D2F-B5A3-36C3B99BCD5E][evil-lisp state:1]]
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
;; evil-lisp state:1 ends here

;; [[file:../readme.org::#h:309D0DC9-29FF-46E8-A98A-F2CD6CCEB12A][eros: results in overlays:1]]
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
;; eros: results in overlays:1 ends here

;; [[file:../readme.org::#h:6F1A571B-D934-4CF8-BE66-3363F695B7B5][init-prog-elisp:1]]
(provide 'init-prog-elisp)
;;; init-org-prog-elisp.el ends here
;; init-prog-elisp:1 ends here
