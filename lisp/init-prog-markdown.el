(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package yaml-mode
  :mode ((rx ".yml" eos) . yaml-mode))

(use-package toml-mode
	:mode "\\.toml\\'")

(provide 'init-prog-markdown)
;;; init-prog-markdown.el ends here
