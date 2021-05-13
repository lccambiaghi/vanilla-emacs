(deftheme 8colors
  "Theme using only 8 colors")

;; (custom-theme-set-variables
;;   '8colors
;;   '(overline-margin 0)
;; )

(custom-theme-set-faces
 '8colors
 '(centaur-tabs-unselected ((t (:foreground "white" :background "black"))) t)
 '(centaur-tabs-unselected-modified ((t (:foreground "white" :background "black"))) t)
 '(tool-bar ((t (:background "black"))) t)
 '(selectrum-current-candidate ((t (:background "blue"))) t)
 '(org-code ((t (:foreground "magenta"))) t)
 '(org-special-keyword ((t (:foreground "magenta"))) t)
 '(mode-line ((t (:background "black"))) t)
 '(doom-modeline-buffer-file ((t (:background "black"))) t)
 '(tab-line ((t (:background "black"))) t)
 '(magit-diff-removed-highlight ((t (:background "red" :foreground "white"))) t)
 '(magit-diff-added-highlight ((t (:background "green" :foreground "white"))) t)
 '(iedit-occurrence ((t (:background "blue" :foreground "white"))) t)
 )

(provide-theme '8colors)
