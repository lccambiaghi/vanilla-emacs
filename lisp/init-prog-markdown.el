;; [[file:../readme.org::#h:C643DC36-8F5E-40E4-B8BB-126F341491FD][markdown:1]]
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
	(setq markdown-command "multimarkdown")
	(setq markdown-fontify-code-blocks-natively t)
	)
;; markdown:1 ends here

;; [[file:../readme.org::#h:351ADC7D-38F2-47FF-9D9B-1C3F2F7B61D3][yaml mode:1]]
(use-package yaml-mode
  :mode ((rx ".yml" eos) . yaml-mode))
;; yaml mode:1 ends here

;; [[file:../readme.org::#h:4474E2AE-F934-48CD-ACB8-5729B7C26F0A][toml mode:1]]
(use-package toml-mode
	:mode "\\.toml\\'")
;; toml mode:1 ends here

;; [[file:../readme.org::#h:B315364E-ECF0-4ABC-9ACB-7546F6BB734C][init-prog-markdown:1]]
(provide 'init-prog-markdown)
;;; init-prog-markdown.el ends here
;; init-prog-markdown:1 ends here
