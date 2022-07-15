;; [[file:../readme.org::#h:89A2FA66-9647-4BF8-A45C-EA61D5D95FDC][org-html-themify:1]]
(use-package org-html-themify
  :after modus-themes
  :straight
  (org-html-themify
   :type git
   :host github
   :repo "DogLooksGood/org-html-themify"
   :files ("*.el" "*.js" "*.css"))
  :hook (org-mode . org-html-themify-mode)
  :init
  (setq org-html-themify-themes
        '((light . modus-operandi)
          (dark . modus-operandi)))
  :config
  ;; otherwise it complains about invalid face
  (require 'hl-line)
  
  )

(use-package htmlize
  :after org-html-themify)
;; org-html-themify:1 ends here

;; [[file:../readme.org::#h:021F6686-3272-4900-BB50-CB98C5C5C2BA][ox-gfm:1]]
(use-package ox-gfm
	:commands (org-gfm-export-as-markdown org-gfm-export-to-markdown)
	:after org
	)
;; ox-gfm:1 ends here

;; [[file:../readme.org::#h:F97C3D33-1B2E-4D80-BD51-E4052A5535DE][ox-ipynb:1]]
(use-package ox-ipynb
  :straight (ox-ipynb :type git :host github :repo "jkitchin/ox-ipynb")
	:commands (ox-ipynb-export-org-file-to-ipynb-file))
;; ox-ipynb:1 ends here

;; [[file:../readme.org::#h:6181675A-C397-4449-A8D9-AFDAECD6D4EE][ox-reveal:1]]
(use-package org-re-reveal
  :after org
  :init
  ;; (setq org-re-reveal-root (expand-file-name "../../" (locate-library "dist/reveal.js" t))
  ;;       org-re-reveal-revealjs-version "4")
  (setq org-re-reveal-root "./reveal.js"
        org-re-reveal-revealjs-version "3.8"
        org-re-reveal-external-plugins  '((progress . "{ src: '%s/plugin/toc-progress/toc-progress.js', async: true, callback: function() { toc_progress.initialize(); toc_progress.create();} }"))
        ))
;; ox-reveal:1 ends here

;; [[file:../readme.org::#h:8739EFFA-1436-4DF9-AD0D-AD0C94144A40][weblorg:1]]
(use-package weblorg)

(use-package templatel)

(use-package htmlize)
;; weblorg:1 ends here

;; [[file:../readme.org::#h:9AB653A8-CACD-45B4-A330-7EB2421370E6][ox-cv:1]]
(use-package ox-altacv
  :straight (ox-altacv :type git :host github :repo "lccambiaghi/org-cv")
  :config (require 'ox-altacv))
;; ox-cv:1 ends here

;; [[file:../readme.org::#h:A646A19B-12BC-4B95-A592-BCB640E10659][init-org-export:1]]
(provide 'init-org-export)
;;; init-org-export.el ends here
;; init-org-export:1 ends here
