(use-package vterm
	:if (not lc/is-ipad)
  :general
  (general-imap
    :keymaps 'vterm-mode-map
    "M-l" 'vterm-send-right
    "M-h" 'vterm-send-left)
  :config
  (setq vterm-shell (executable-find "fish")
        vterm-max-scrollback 10000))

(use-package vterm-toggle
  :if (not lc/is-ipad)
  :general
  (lc/leader-keys
    "'" 'vterm-toggle)
  :init
  (setq vterm-toggle-scope 'project)
  )

(provide 'init-prog-vterm)
;;; init-prog-vterm.el ends here
