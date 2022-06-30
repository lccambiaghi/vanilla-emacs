;; [[file:../readme.org::#h:2dc32a09-6e88-4d10-9e30-f138f4345143][vterm:1]]
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
;; vterm:1 ends here

;; [[file:../readme.org::#h:1162BDDB-9DC6-4750-8EF8-9C5D2AF51892][vterm toggle:1]]
(use-package vterm-toggle
  :if (not lc/is-ipad)
  :general
  (lc/leader-keys
    "'" 'vterm-toggle)
  :init
  (setq vterm-toggle-scope 'project)
  )
;; vterm toggle:1 ends here

;; [[file:../readme.org::#h:EE92BA80-1F88-4D69-A45C-69268595FA71][init-prog-vterm:1]]
(provide 'init-prog-vterm)
;;; init-prog-vterm.el ends here
;; init-prog-vterm:1 ends here
