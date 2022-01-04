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

(provide 'init-extra-rss)
;;; init-org-extra-rss.el ends here
