(use-package centered-cursor-mode
  :general
	(lc/leader-keys
		"t =" '((lambda () (interactive) (centered-cursor-mode 'toggle)) :wk "center cursor")
		)
	)

(use-package hide-mode-line
  :commands (hide-mode-line-mode))

(use-package winum
:general
(lc/leader-keys
"1" '(winum-select-window-1 :wk "win 1")
"2" '(winum-select-window-2 :wk "win 2")
"3" '(winum-select-window-3 :wk "win 3"))
:config
(winum-mode))

(use-package transpose-frame
  :general
  (lc/leader-keys
    "w t" '(transpose-frame :wk "transpose")
    "w f" '(rotate-frame :wk "flip")))

(provide 'init-ui-extra)
;;; init-ui-extra.el ends here
