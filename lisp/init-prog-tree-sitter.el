(use-package tree-sitter
  ;; :straight (tree-sitter :host github :repo "ubolonton/emacs-tree-sitter" :depth full)
  :hook (python-mode . (lambda ()
                         (require 'tree-sitter)
                         (require 'tree-sitter-langs)
                         (require 'tree-sitter-hl)
                         (tree-sitter-hl-mode)
                         )))

(use-package tree-sitter-langs)

(provide 'init-prog-tree-sitter)
;;; init-prog-tree-sitter.el ends here
