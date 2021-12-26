(use-package org-roam
  :after org
  :init
  (setq org-roam-directory (file-truename "~/roam"))
  (setq org-roam-v2-ack t)
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?" :target
           (file+head "personal/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n") :unnarrowed t)
          ("w" "work" plain "%?" :target
           (file+head "work/%<%Y%m%d%H%M%S>-${slug}.org"  "#+title: ${title}\n") :unnarrowed t)))
  :general
  (general-nmap
    "SPC n b" 'org-roam-buffer-toggle
    "SPC n f" 'org-roam-node-find
    "SPC n g" 'org-roam-graph
    "SPC n i" 'org-roam-node-insert
    "SPC n c" 'org-roam-capture
    "SPC n t" 'org-roam-tag-add
    "SPC n r" 'org-roam-ref-add
    "SPC n a" 'org-roam-alias-add
    ;; Dailies
    "SPC n j" 'org-roam-dailies-capture-today
    "SPC n J" 'org-roam-dailies-goto-today
    )
  :config
  (org-roam-setup)
  ;; If using org-roam-protocol
  ;; (require 'org-roam-protocol)
  (add-to-list 'display-buffer-alist
               '(("*org-roam*"
                  (display-buffer-in-direction)
                  (direction . right)
                  (window-width . 0.33)
                  (window-height . fit-window-to-buffer))))
  
  )

(provide 'init-org-roam)
;;; init-org-roam.el ends here
