;; [[file:../readme.org::#h:B0653EBC-9E48-4E3D-86A3-34E7450B4F03][elfeed:1]]
(use-package elfeed
  :straight (elfeed :type git :host github :repo "skeeto/elfeed")
  :hook (elfeed-search-mode . elfeed-update)
  :general
  (lc/leader-keys
    "s r" '(elfeed :wk "elfeed"))
  (general-nmap
		:keymaps 'elfeed-search-mode-map
   "x" 'lc/elfeed-xwwp-open)
  :init
  (defun lc/elfeed-xwwp-open (&optional use-generic-p)
    "open with eww"
    (interactive "P")
    (let ((entries (elfeed-search-selected)))
      (cl-loop for entry in entries
               do (elfeed-untag entry 'unread)
               when (elfeed-entry-link entry)
               do (xwwp it))
      (mapc #'elfeed-search-update-entry entries)
      (unless (use-region-p) (forward-line))))
  :config
  (setq elfeed-feeds'(("https://www.reddit.com/r/emacs.rss?sort=new" reddit emacs)
                      ("http://emacsredux.com/atom.xml" emacs)
                      ("http://irreal.org/blog/?tag=emacs&amp;feed=rss2" emacs)
                      ("https://www.reddit.com/search.rss?q=url%3A%28youtu.be+OR+youtube.com%29&sort=top&t=week&include_over_18=1&type=link"
                      reddit youtube popular))))
;; elfeed:1 ends here

;; [[file:../readme.org::#h:A8F1EDE6-D506-49E6-A459-3AF50486BC89][init-extra-rss:1]]
(provide 'init-extra-rss)
;;; init-org-extra-rss.el ends here
;; init-extra-rss:1 ends here
