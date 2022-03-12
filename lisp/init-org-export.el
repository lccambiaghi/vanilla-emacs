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

(use-package ox-gfm
	:commands (org-gfm-export-as-markdown org-gfm-export-to-markdown)
	:after org
	)

(use-package ox-ipynb
  :straight (ox-ipynb :type git :host github :repo "jkitchin/ox-ipynb")
	:commands (ox-ipynb-export-org-file-to-ipynb-file))

(use-package org-re-reveal
  :after org
  :init
  ;; (setq org-re-reveal-root (expand-file-name "../../" (locate-library "dist/reveal.js" t))
  ;;       org-re-reveal-revealjs-version "4")
  (setq org-re-reveal-root "./reveal.js"
        org-re-reveal-revealjs-version "3.8"
        org-re-reveal-external-plugins  '((progress . "{ src: '%s/plugin/toc-progress/toc-progress.js', async: true, callback: function() { toc_progress.initialize(); toc_progress.create();} }"))
        ))

(use-package weblorg)

(use-package templatel)

(use-package htmlize)

(use-package ox-altacv
  :straight (ox-altacv :type git :host github :repo "lccambiaghi/org-cv")
  :config (require 'ox-altacv))

(provide 'init-org-export)
;;; init-org-export.el ends here
