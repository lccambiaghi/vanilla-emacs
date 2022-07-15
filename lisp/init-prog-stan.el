;; [[file:../readme.org::#h:33FE46F3-DB69-4F9F-B40B-9E1C970040D5][stan mode:1]]
(use-package stan-mode
  :mode ("\\.stan\\'" . stan-mode)
  :hook (stan-mode . stan-mode-setup)
  ;;
  :config
  ;; The officially recommended offset is 2.
  (setq stan-indentation-offset 2))
;; stan mode:1 ends here

;; [[file:../readme.org::#h:B315364E-ECF0-4ABC-9ACB-7546F6BB734C][init-prog-stan:1]]
(provide 'init-prog-stan)
;;; init-prog-stan.el ends here
;; init-prog-stan:1 ends here
