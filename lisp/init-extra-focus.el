;; [[file:../readme.org::#h:78EE4DFB-9A75-4BB0-BD4D-CFF94ACE3CF6][olivetti mode:1]]
(use-package olivetti
  :general
  (lc/leader-keys
    "t o" '(olivetti-mode :wk "olivetti"))
  :init
  (setq olivetti-body-width 100)
  (setq olivetti-recall-visual-line-mode-entry-state t))
;; olivetti mode:1 ends here

;; [[file:../readme.org::#h:EC24FC48-E3A5-4C16-9F52-3A6E14DBA564][darkroom:1]]
(use-package darkroom
  :init
  ;; Don't scale the text, so ugly man!
  (setq darkroom-text-scale-increase 3)
  :general
  (lc/leader-keys
    "t F" '(darkroom-tentative-mode :wk "focus")))
;; darkroom:1 ends here

;; [[file:../readme.org::#h:E8D67694-4B23-47E2-800B-10136B4D8F2A][init-extra-focus:1]]
(provide 'init-extra-focus)
;;; init-org-extra-focus.el ends here
;; init-extra-focus:1 ends here
