;; [[file:../readme.org::#h:6B214C3A-22E3-47D0-9DD7-F379E07CF960][web browser:1]]
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
  ;; :custom
  ;; (setq xwwp-follow-link-completion-system 'ivy)
  ;; :bind (:map xwidget-webkit-mode-map
  ;;             ("v" . xwwp-follow-link))
  )
;; web browser:1 ends here

;; [[file:../readme.org::#h:11871421-6FA2-4450-AA35-B779A525FE69][init-extra-web:1]]
(provide 'init-extra-web)
;;; init-org-extra-web.el ends here
;; init-extra-web:1 ends here
