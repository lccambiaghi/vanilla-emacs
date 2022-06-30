;; [[file:../readme.org::#h:175B1F5E-17E0-4D19-86CE-9577FBC6609B][org-roam:1]]
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
  (lc/leader-keys
    "TAB n" '((lambda () (interactive) (persp-switch "notes")) :wk "notes")
    "n n" '((lambda () (interactive)
              (persp-switch "notes")
              (org-roam-node-find))
            :wk "notes workspace")
    "n b" 'org-roam-buffer-toggle
    "n f" 'org-roam-node-find
    "n g" 'org-roam-graph
    "n i" 'org-roam-node-insert
    "n c" 'org-roam-capture
    "n t" 'org-roam-tag-add
    "n r" 'org-roam-ref-add
    "n a" 'org-roam-alias-add
    ;; Dailies
    "n j" 'org-roam-dailies-capture-today
    "n J" 'org-roam-dailies-goto-today
    ;; todos
    "o t" '((lambda () (interactive)
              (persp-switch "notes")
              (find-file (concat org-roam-directory "/work/todo.org")))
            :wk "work todos")
    "o n" '((lambda () (interactive)
              (persp-switch "notes")
              (org-roam-node-find))
            :wk "notes")
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
;; org-roam:1 ends here

;; [[file:../readme.org::#h:8C169B84-C560-42FB-8A12-F052E4685ECB][init-org-roam:1]]
(provide 'init-org-roam)
;;; init-org-roam.el ends here
;; init-org-roam:1 ends here
