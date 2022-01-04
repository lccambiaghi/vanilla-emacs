(use-package olivetti
  :general
  (lc/leader-keys
    "t o" '(olivetti-mode :wk "olivetti"))
  :init
  (setq olivetti-body-width 100)
  (setq olivetti-recall-visual-line-mode-entry-state t))

(use-package darkroom
  :init
  ;; Don't scale the text, so ugly man!
  (setq darkroom-text-scale-increase 1)
  :general
  (lc/leader-keys
    "tf" '(darkroom-tentative-mode :wk "focus")))

(provide 'init-extra-focus)
;;; init-org-extra-focus.el ends here
