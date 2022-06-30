;; [[file:../readme.org::#h:1FD9CDF3-4C5D-450E-99DE-034077C3D8BC][emacs tree-sitter:1]]
(use-package tree-sitter
  ;; :straight (tree-sitter :host github :repo "ubolonton/emacs-tree-sitter" :depth full)
  :hook (python-mode . (lambda ()
                         (require 'tree-sitter)
                         (require 'tree-sitter-langs)
                         (require 'tree-sitter-hl)
                         (tree-sitter-hl-mode)
                         )))

(use-package tree-sitter-langs)
;; emacs tree-sitter:1 ends here

;; [[file:../readme.org::#h:A646A19B-12BC-4B95-A592-BCB640E10659][init-prog-tree-sitter:1]]
(provide 'init-prog-tree-sitter)
;;; init-prog-tree-sitter.el ends here
;; init-prog-tree-sitter:1 ends here
