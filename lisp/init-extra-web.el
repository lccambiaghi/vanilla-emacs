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

(provide 'init-extra-web)
;;; init-org-extra-web.el ends here
