;; [[file:../readme.org::#h:26FC27A5-5E20-4440-BA86-48FAA8707D00][centered cursor mode:1]]
(use-package centered-cursor-mode
  :general
	(lc/leader-keys
		"t =" '((lambda () (interactive) (centered-cursor-mode 'toggle)) :wk "center cursor")
		)
	)
;; centered cursor mode:1 ends here

;; [[file:../readme.org::#h:89BB7025-084D-4465-897D-C731EB02161A][hide mode line:1]]
(use-package hide-mode-line
  :commands (hide-mode-line-mode))
;; hide mode line:1 ends here

;; [[file:../readme.org::#h:991499E8-6FF8-41DB-B533-6D627602DFD4][winum:1]]
(use-package winum
  :general
  (lc/leader-keys
    "1" '(winum-select-window-1 :wk "win 1")
    "2" '(winum-select-window-2 :wk "win 2")
    "3" '(winum-select-window-3 :wk "win 3")
    "4" '(winum-select-window-4 :wk "win 4")
    "5" '(winum-select-window-5 :wk "win 5")
    "6" '(winum-select-window-6 :wk "win 6")
    )
  :config
  (winum-mode))
;; winum:1 ends here

;; [[file:../readme.org::#h:9E3C0942-1D8C-403F-9D44-5BEDA2966C58][transpose frame:1]]
(use-package transpose-frame
  :general
  (lc/leader-keys
    "w t" '(transpose-frame :wk "transpose")
    "w f" '(rotate-frame :wk "flip")))
;; transpose frame:1 ends here

;; [[file:../readme.org::#h:E80DEB4B-6AC9-415D-AF36-0044479D1B5A][init-ui-extras:1]]
(provide 'init-ui-extra)
;;; init-ui-extra.el ends here
;; init-ui-extras:1 ends here
