;; NOTE: init.el is now generated from readme.org.  Please edit that file instead

;; `file-name-handler-alist' is consulted on every `require', `load' and various
;; path/io functions. You get a minor speed up by nooping this. However, this
;; may cause problems on builds of Emacs where its site lisp files aren't
;; byte-compiled and we're forced to load the *.el.gz files (e.g. on Alpine)
(unless (daemonp)
  (defvar doom--initial-file-name-handler-alist file-name-handler-alist)
  (setq file-name-handler-alist nil)
  ;; Restore `file-name-handler-alist' later, because it is needed for handling
  ;; encrypted or compressed files, among other things.
  (defun doom-reset-file-handler-alist-h ()
    ;; Re-add rather than `setq', because changes to `file-name-handler-alist'
    ;; since startup ought to be preserved.
    (dolist (handler file-name-handler-alist)
      (add-to-list 'doom--initial-file-name-handler-alist handler))
    (setq file-name-handler-alist doom--initial-file-name-handler-alist))
  (add-hook 'emacs-startup-hook #'doom-reset-file-handler-alist-h)
  (add-hook 'after-init-hook '(lambda ()
                                 ;; restore after startup
                                 (setq gc-cons-threshold 16777216
                                       gc-cons-percentage 0.1)))
  )
;; Ensure Doom is running out of this file's directory
(setq user-emacs-directory (file-truename (file-name-directory load-file-name)))

(setq straight-use-package-by-default t)
(setq straight-vc-git-default-clone-depth 1)
(setq straight-recipes-gnu-elpa-use-mirror t)
(setq straight-check-for-modifications '(check-on-save find-when-checking))
(setq use-package-always-defer t)
(defvar bootstrap-version)
(let* ((straight-repo-dir
        (expand-file-name "straight/repos" user-emacs-directory))
       (bootstrap-file
        (concat straight-repo-dir "/straight.el/bootstrap.el"))
       (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (shell-command
     (concat
      "mkdir -p " straight-repo-dir " && "
      "git -C " straight-repo-dir " clone "
      "https://github.com/raxod502/straight.el.git && "
      "git -C " straight-repo-dir " checkout 2d407bc")))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)
;; This is a variable that has been renamed but straight still refers when
;; doing :sraight (:no-native-compile t)
(setq comp-deferred-compilation-black-list nil)

(setq use-package-compute-statistics t)

(use-package emacs
  :init
  (setq inhibit-startup-screen t
        initial-scratch-message nil
        sentence-end-double-space nil
        ring-bell-function 'ignore
        frame-resize-pixelwise t)

  (setq user-full-name "Luca Cambiaghi"
        user-mail-address "luca.cambiaghi@me.com")

  (setq read-process-output-max (* 1024 1024))

  ;; always allow 'y' instead of 'yes'.
  (defalias 'yes-or-no-p 'y-or-n-p)

  ;; default to utf-8 for all the things
  (set-charset-priority 'unicode)
  (setq locale-coding-system 'utf-8
        coding-system-for-read 'utf-8
        coding-system-for-write 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix))

  ;; write over selected text on input... like all modern editors do
  (delete-selection-mode t)

  ;; enable recent files mode.
  (recentf-mode t)
	(setq recentf-exclude `(,(expand-file-name "straight/build/" user-emacs-directory)
                     ,(expand-file-name "eln-cache/" user-emacs-directory)
                     ,(expand-file-name "etc/" user-emacs-directory)
                     ,(expand-file-name "var/" user-emacs-directory)))

  ;; don't want ESC as a modifier
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)

  ;; Don't persist a custom file, this bites me more than it helps
  (setq custom-file (make-temp-file "")) ; use a temp file as a placeholder
  (setq custom-safe-themes t)            ; mark all themes as safe, since we can't persist now
  (setq enable-local-variables :all)     ; fix =defvar= warnings

  ;; stop emacs from littering the file system with backup files
  (setq make-backup-files nil
        auto-save-default nil
        create-lockfiles nil)

  ;; follow symlinks 
  (setq vc-follow-symlinks t)

  ;; don't show any extra window chrome
  (when (window-system)
    (tool-bar-mode -1)
    (toggle-scroll-bar -1))

  ;; enable winner mode globally for undo/redo window layout changes
  (winner-mode t)

  ;; less noise when compiling elisp
  (setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

  ;; clean up the mode line
  (display-time-mode -1)
  (setq column-number-mode t)
	
  ;; use common convention for indentation by default
  (setq-default indent-tabs-mode t)
  (setq-default tab-width 2)

  ;; use a reasonable line length
  (setq-default fill-column  90)
	
  ;; Increase the amount of data read from processes
	(setq read-process-output-max (* 1024 1024)) ; 1mb.
  )

(use-package emacs
	:init
  ;; auto-close parentheses
  (electric-pair-mode +1)
	(setq electric-pair-preserve-balance nil)
  ;; mode-specific local-electric pairs
  (defun set-local-electric-pairs (pairs)
		"Example usage: 
    (add-hook 'jupyter-org-interaction-mode '(lambda () (set-local-electric-pairs '())))
    "
    (setq-local electric-pair-pairs (append electric-pair-pairs pairs))
    (setq-local electric-pair-text-pairs electric-pair-pairs))

  ;; disable auto pairing for <
  (add-function :before-until electric-pair-inhibit-predicate
                (lambda (c) (eq c ?<))))

(use-package emacs
	:init
	(defconst my/default-font-family "Fira Code Retina" )
	(defconst my/variable-pitch-font-family "Cantarell")
	
	;; my/default-font-size is calculated on start according to the primary screen
	;; size. if screen-size is bigger than 16 inch: 9 else 11.
	(defconst my/default-font-size
		(let* (;; (command "xrandr | awk '/primary/{print sqrt( ($(NF-2)/10)^2 + ($NF/10)^2 )/2.54}'")
					 (command "osascript -e 'tell application \"Finder\" to get bounds of window of desktop' | cut -d',' -f3")
					 (screen-width (string-to-number (shell-command-to-string command))))
      (if (> screen-width 2560) 210 180)))

  ;; Main typeface
  (set-face-attribute 'default nil :font "Fira Code Retina" :height my/default-font-size)
  ;; Set the fixed pitch face
  (set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height my/default-font-size)
  ;; Set the variable pitch face
  (set-face-attribute 'variable-pitch nil :font "Cantarell" :height my/default-font-size :weight 'regular)
	)

(defun my/adjust-font-size (height)
  "Adjust font size by given height. If height is '0', reset font
size. This function also handles icons and modeline font sizes."
  (interactive "nHeight ('0' to reset): ")
  (let ((new-height (if (zerop height)
                        my/default-font-size
                      (+ height (face-attribute 'default :height)))))
    (set-face-attribute 'default nil :height new-height)
    (set-face-attribute 'fixed-pitch nil :height new-height)
    (set-face-attribute 'variable-pitch nil :height new-height)
    ;; (set-face-attribute 'mode-line nil :height new-height)
    ;; (set-face-attribute 'mode-line-inactive nil :height new-height)
    (message "Font size: %s" new-height)))

(defun my/increase-font-size ()
  "Increase font size by 0.5 (5 in height)."
  (interactive)
  (my/adjust-font-size 5))

(defun my/decrease-font-size ()
  "Decrease font size by 0.5 (5 in height)."
  (interactive)
  (my/adjust-font-size -5))

(defun my/reset-font-size ()
  "Reset font size according to the `my/default-font-size'."
  (interactive)
  (my/adjust-font-size 0))

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'super)     ; command as super
  (setq mac-option-modifier 'meta)     ; alt as meta
  (setq mac-control-modifier 'control)) ; control as... control

  (use-package gcmh
    :demand
    :config
    (gcmh-mode 1))

  (use-package helpful
    :after evil
    :init
    (setq evil-lookup-func #'helpful-at-point)
    :bind
    ([remap describe-function] . helpful-callable)
    ([remap describe-command] . helpful-command)
    ([remap describe-variable] . helpful-variable)
    ([remap describe-key] . helpful-key))

  (use-package eldoc
    :hook (emacs-lisp-mode cider-mode))

  (use-package exec-path-from-shell
    :if (memq window-system '(mac ns))
    :hook (emacs-startup . (lambda ()
                             (setq exec-path-from-shell-arguments '("-l")) ; removed the -i for faster startup
                             (exec-path-from-shell-initialize)))
    ;; :config
    ;; (exec-path-from-shell-copy-envs
    ;;  '("GOPATH" "GO111MODULE" "GOPROXY"
    ;;    "NPMBIN" "LC_ALL" "LANG" "LC_TYPE"
    ;;    "SSH_AGENT_PID" "SSH_AUTH_SOCK" "SHELL"
    ;;    "JAVA_HOME"))
    )

(use-package no-littering
	:demand
	:config
  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory))
	)

(use-package emacs
	:init
	(unless (and (fboundp 'server-running-p)
               (server-running-p))
    (server-start)))

(use-package emacs
  :init
  (defun is-jupyter-python-org-buffer? ()
    (with-current-buffer (buffer-name)
      (goto-char (point-min))
      (re-search-forward "begin_src jupyter-python" 10000 t)))
  
  (define-minor-mode org-jupyter-python-mode
    "Minor mode which is active when an org file has the string 
begin_src jupyter-python in the first few hundred rows"
		;; :keymap (let ((map (make-sparse-keymap)))
    ;;             (define-key map (kbd "C-c f") 'insert-foo)
		;;             map)
		)

  (add-hook 'org-mode-hook (lambda ()
														 (when (is-jupyter-python-org-buffer?)
															 (org-jupyter-python-mode)))))

(use-package general
  :demand t
  :config
  (general-evil-setup)

  (general-create-definer my/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  (general-create-definer my/local-leader-keys
    :states '(normal visual)
    :keymaps 'override
    :prefix ","
    :global-prefix "SPC m")

  (my/leader-keys
    "SPC" '(execute-extended-command :which-key "execute command")
    "`" '((lambda () (interactive) (switch-to-buffer (other-buffer (current-buffer) 1))) :which-key "prev buffer")
		
    ";" '(eval-expression :which-key "eval sexp")

    "b" '(:ignore t :which-key "buffer")
    "br"  'revert-buffer
    "bd"  'kill-current-buffer

    "c" '(:ignore t :which-key "code")

    "f" '(:ignore t :which-key "file")
    "fD" '((lambda () (interactive) (delete-file (buffer-file-name))) :wk "delete")
    "ff"  'find-file
    "fs" 'save-buffer
    "fr" 'recentf-open-files
    "fR" '((lambda (new-path)
						 (interactive (list (read-file-name "Move file to: ") current-prefix-arg))
						 (rename-file (buffer-file-name) (expand-file-name new-path))) :wk "move/rename")

    "g" '(:ignore t :which-key "git")
		;; keybindings defined in magit

    "h" '(:ignore t :which-key "describe")
    "he" 'view-echo-area-messages
    "hf" 'describe-function
    "hF" 'describe-face
    "hl" 'view-lossage
    "hL" 'find-library
    "hm" 'describe-mode
    "hk" 'describe-key
    "hK" 'describe-keymap
    "hp" 'describe-package
    "hv" 'describe-variable

    "o" '(:ignore t :which-key "org")
		;; keybindings defined in org-mode

    "p" '(:ignore t :which-key "project")
		;; keybindings defined in projectile

    "s" '(:ignore t :which-key "search")
		;; keybindings defined in consult

    "t"  '(:ignore t :which-key "toggle")
    "t d"  '(toggle-debug-on-error :which-key "debug on error")
		"t l" '(display-line-numbers-mode :wk "line numbers")
    "t w" '((lambda () (interactive) (toggle-truncate-lines)) :wk "word wrap")

		"u" '(universal-argument :wk "universal")

    "w" '(:ignore t :which-key "window")
    "wl"  'windmove-right
    "wh"  'windmove-left
    "wk"  'windmove-up
    "wj"  'windmove-down
    "wr" 'winner-redo
    "wd"  'delete-window
		"w=" 'balance-windows-area
    "wD" 'kill-buffer-and-window
    "wu" 'winner-undo
    "wr" 'winner-redo
    "wm"  '(delete-other-windows :wk "maximize"))

  (my/local-leader-keys
    "d" '(:ignore t :which-key "debug")
    "e" '(:ignore t :which-key "eval")
    "t" '(:ignore t :which-key "test")
    )
  )

(use-package evil
  :demand t
  :general
  (my/leader-keys
    "wv" 'evil-window-vsplit
    "ws" 'evil-window-split)
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-want-Y-yank-to-eol t)
  ;; move to window when splitting
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)
  ;; (setq-local evil-scroll-count 0)
  (setq evil-auto-indent nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  ;; don't move cursor after ==
  (defun my/evil-dont-move-cursor (orig-fn &rest args)
    (save-excursion (apply orig-fn args)))
  (advice-add 'evil-indent :around #'my/evil-dont-move-cursor)
  )

(use-package evil-collection
  :after evil
  :demand
	:init
	(setq evil-collection-magit-use-z-for-folds nil)
  :config
  (evil-collection-init))

(use-package evil-goggles
  :after evil
  :demand
  :init
  (setq evil-goggles-duration 0.05)
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

(use-package evil-snipe
	:after evil
	:demand
	:config
	(evil-snipe-mode +1)
  (evil-snipe-override-mode +1))

(use-package evil-nerd-commenter
  :general
  (general-nvmap
    "gc" 'evilnc-comment-operator
    "gC" 'evilnc-copy-and-comment-operator)
  )

(use-package evil-surround
  :general
  (:states 'operator
   "s" 'evil-surround-edit
   "S" 'evil-Surround-edit)
  (:states 'visual
   "S" 'evil-surround-region
   "gS" 'evil-Surround-region))

(use-package evil-indent-plus
	:after evil
	:demand
  :config
  (define-key evil-inner-text-objects-map "i" 'evil-indent-plus-i-indent)
  (define-key evil-outer-text-objects-map "i" 'evil-indent-plus-a-indent)
	(define-key evil-inner-text-objects-map "k" 'evil-indent-plus-i-indent-up)
	(define-key evil-outer-text-objects-map "k" 'evil-indent-plus-a-indent-up)
	(define-key evil-inner-text-objects-map "j" 'evil-indent-plus-i-indent-up-down)
	(define-key evil-outer-text-objects-map "j" 'evil-indent-plus-a-indent-up-down)
	)

(use-package evil-iedit-state
  :general
  (my/leader-keys
		"s e" '(evil-iedit-state/iedit-mode :wk "iedit")
		"s q" '(evil-iedit-state/quit-iedit-mode :wk "iedit quit")))

(use-package evil-mc
	:general
	(general-vmap
    "A" #'evil-mc-make-cursor-in-visual-selection-end
    "I" #'evil-mc-make-cursor-in-visual-selection-beg)
	(general-nmap
		"Q" #'evil-mc-undo-all-cursors)
	:config
	(global-evil-mc-mode 1))

(use-package which-key
  :demand t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  ;; (setq which-key-idle-delay 0.5)
  :config
  (which-key-mode))

(use-package org
  :hook ((org-mode . prettify-symbols-mode)
         (org-mode . visual-line-mode)
         (org-mode . variable-pitch-mode)
         (org-mode . (lambda () (set-local-electric-pairs '((?= . ?=) (?~ . ?~))))))
  :general
  (my/leader-keys
    "o a" '(org-agenda-list :wk "agenda")
    "o A" '(org-agenda :wk "agenda")
    "o C" '(org-capture :wk "capture")
    "o l" '(org-todo-list :wk "todo list")
    "o c" '((lambda () (interactive)
              (find-file (concat user-emacs-directory "readme.org")))
            :wk "open config")
    "o t" '((lambda () (interactive)
              (find-file (concat org-directory "/personal/todo.org")))
            :wk "open todos"))
  (my/local-leader-keys
    :keymaps 'org-mode-map
    "a" '(org-archive-subtree :wk "archive subtree")
    "E" '(org-export-dispatch :wk "export")
		"i" '(org-insert-structure-template :wk "insert src")
    "l" '(:ignore true :wk "link")
    "l l" '(org-insert-link :wk "insert link")
    "l s" '(org-store-link :wk "store link")
    "L" '((lambda () (interactive) (org-latex-preview)) :wk "latex preview")
    ;; "L" '((lambda () (interactive) (org--latex-preview-region (point-min) (point-max))) :wk "latex")
    "r" '(org-refile :wk "refile")
    "n" '(org-toggle-narrow-to-subtree :wk "narrow subtree")
		"p" '(org-priority :wk "priority")
    "s" '(org-sort :wk "sort")
    "t" '(:ignore true :wk "todo")
    "t t" '(org-todo :wk "heading todo")
    "t s" '(org-schedule :wk "schedule")
    "t d" '(org-deadline :wk "deadline"))
  (org-mode-map
   :states 'normal
   "z i" '(org-toggle-inline-images :wk "inline images"))
	;; (org-mode-map
  ;;  :states 'insert
  ;;  "(" '((lambda () (interactive) (skeleton-pair-insert-maybe))))
  :init
  ;; general settings
  (setq org-directory "~/Dropbox/org"
        org-image-actual-width nil
        +org-export-directory "~/Dropbox/org/export"
        org-default-notes-file "~/Dropbox/org/personal/todo.org"
        org-id-locations-file "~/Dropbox/org/.orgids"
        org-agenda-files '("~/dropbox/org/personal/birthdays.org" "~/dropbox/org/personal/todo.org" "~/dropbox/Notes/Test.inbox.org")
        ;; org-export-in-background t
        org-src-preserve-indentation t ;; do not put two spaces on the left
				org-startup-indented t
				;; org-startup-with-inline-images t
				org-hide-emphasis-markers t
        org-catch-invisible-edits 'smart)
	(setq org-indent-indentation-per-level 1)
	(setq org-list-demote-modify-bullet '(("-" . "+") ("+" . "*")))
  ;; disable modules for faster startup
  (setq org-modules
        '(ol-docview
					org-habit))
  (setq org-todo-keywords
        '((sequence "NEXT(n)" "TODO(t)" "|" "PROG(n)" "|" "DONE(d)" "HOLD(h)")))
  (setq org-capture-templates
        `(("b" "Blog" entry
           (file+headline "personal/todo.org" "Blog")
           ,(concat "* WRITE %^{Title} %^g\n"
                    "SCHEDULED: %^t\n"
                    ":PROPERTIES:\n"
                    ":CAPTURED: %U\n:END:\n\n"
                    "%i%?"))
          ("d" "New Diary Entry" entry(file+olp+datetree"~/Dropbox/org/personal/diary.org" "Daily Logs")
           "* %^{thought for the day}
                 :PROPERTIES:
                 :CATEGORY: %^{category}
                 :SUBJECT:  %^{subject}
                 :MOOD:     %^{mood}
                 :END:
                 :RESOURCES:
                 :END:

                 \*What was one good thing you learned today?*:
                 - %^{whatilearnedtoday}

                 \*List one thing you could have done better*:
                 - %^{onethingdobetter}

                 \*Describe in your own words how your day was*:
                 - %?")
          ("i" "Inbox" entry
           (file+headline "personal/todo.org" "Inbox")
           ,(concat "* %^{Title}\n"
                    ":PROPERTIES:\n"
                    ":CAPTURED: %U\n"
                    ":END:\n\n"
                    "%i%l"))
          ("u" "New URL Entry" entry
           (file+function "~/Dropbox/org/personal/dailies.org" org-reverse-datetree-goto-date-in-file)
           "* [[%^{URL}][%^{Description}]] %^g %?")
          ("w" "Work" entry
           (file+headline "personal/todo.org" "Work")
           ,(concat "* TODO [#A] %^{Title} :@work:\n"
                    "SCHEDULED: %^t\n"
                    ":PROPERTIES:\n:CAPTURED: %U\n:END:\n\n"
                    "%i%?"))))
  (setq-default prettify-symbols-alist '(("#+BEGIN_SRC" . "»")
                                         ("#+END_SRC" . "«")
                                         ("#+begin_src" . "»")
                                         ("#+end_src" . "«")
																				 ("lambda"  . "λ")
                                         ("->" . "→")
                                         ("->>" . "↠")))
  (setq prettify-symbols-unprettify-at-point 'right-edge)
  (setq org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda "" ((org-deadline-warning-days 7)))
            (todo "NEXT"
                  ((org-agenda-overriding-header "Next Tasks")))
            (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))
          ("n" "Next Tasks"
           ((todo "NEXT"
                  ((org-agenda-overriding-header "Next Tasks")))))
          ("w" "Work Tasks" tags-todo "+work")))
  (defun org-toc ()
    (interactive)
    (let ((headings (delq nil (cl-loop for f in (f-entries "." (lambda (f) (f-ext? f "org")) t)
																			 append
																			 (with-current-buffer (find-file-noselect f)
																				 (org-map-entries
																					(lambda ()
																						(when (> 2 (car (org-heading-components)))
																							(cons f (nth 4 (org-heading-components)))))))))))
			(switch-to-buffer (get-buffer-create "*toc*"))
			(erase-buffer)
			(org-mode)
			(cl-loop for (file . file-headings) in (seq-group-by #'car headings) 
							 do
							 (insert (format "* %s \n" file))
							 (cl-loop for (file . heading) in file-headings 
												do
												(insert (format "** [[%s::*%s][%s]]\n" file heading heading))))))
	
	:config
  ;; (efs/org-font-setup)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("clj" . "src clojure"))
  (add-to-list 'org-structure-template-alist '("jp" . "src jupyter-python"))
  (add-to-list 'org-structure-template-alist '("jr" . "src jupyter-R"))
  ;; latex
  ;; (setq org-latex-compiler "xelatex")
	;; see https://www.reddit.com/r/emacs/comments/l45528/questions_about_mving_from_standard_latex_to_org/gkp4f96/?utm_source=reddit&utm_medium=web2x&context=3
	(setq org-latex-pdf-process '("tectonic %f"))
  (add-to-list 'org-export-backends 'beamer)
  (setq org-html-htmlize-output-type 'css)
	(plist-put org-format-latex-options :scale 2.0)
  )

(use-package org-reverse-datetree
:after org)

(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  :init
  (setq org-superstar-headline-bullets-list '("✖" "✚" "◆" "▶" "○")
        org-superstar-special-todo-items t
        ;; org-ellipsis "⤵"
        ;; org-ellipsis "▼"
        ;; org-ellipsis "..."
        org-ellipsis " ↴ "
        )
  )

(use-package hl-todo
	:hook ((prog-mode org-mode) . my/hl-todo-init)
	:init
	(defun my/hl-todo-init ()
    (setq-local hl-todo-keyword-faces '(("HOLD" . "#cfdf30")
                                        ("TODO" . "#ff9977")
                                        ("NEXT" . "#b6a0ff")
																				("PROG" . "#00d3d0")
																				("FIXME" . "#ff9977")
																				("DONE" . "#44bc44")
																				("REVIEW" . "#6ae4b9")
																				("DEPRECATED" . "#bfd9ff")))
		(hl-todo-mode))
  )

(use-package org
  :general
  (my/local-leader-keys
    :keymaps 'org-mode-map
    "," '(org-edit-special :wk "edit")
    "-" '(org-babel-demarcate-block :wk "split block")
    "z" '(org-babel-hide-result-toggle :wk "fold result"))
  (my/local-leader-keys
    :keymaps 'org-src-mode-map
    "," '(org-edit-src-exit :wk "exit")) ;;FIXME
  :init
  (setq org-confirm-babel-evaluate nil)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)))
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
  )

;; enable mermaid diagram blocks
;; (use-package ob-mermaid
;;   :custom (ob-mermaid-cli-path "~/.asdf/shims/mmdc"))

  (use-package ob-async
    :hook (org-load . (lambda () (require 'ob-async)))
    :init
    (setq ob-async-no-async-languages-alist '("jupyter-python" "jupyter-R" "jupyter-julia")))

(use-package jupyter
  :straight (:no-native-compile t :no-byte-compile t) ;; otherwise we get jupyter-channel void
  :general
  (my/local-leader-keys
    :keymaps 'org-mode-map
    "=" '((lambda () (interactive) (jupyter-org-insert-src-block t nil)) :wk "block below")
    "m" '(jupyter-org-merge-blocks :wk "merge")
    "+" '(jupyter-org-insert-src-block :wk "block above")
    "?" '(jupyter-inspect-at-point :wk "inspect")
    "x" '(jupyter-org-kill-block-and-results :wk "kill block"))
  :hook ((jupyter-org-interaction-mode . (lambda () (set-local-electric-pairs '((?' . ?')))))
         (jupyter-repl-persistent-mode . (lambda ()  ;; we activate org-interaction-mode ourselves
                                           (when (derived-mode-p 'org-mode)
                                             ;; (setq-local company-backends '((company-capf)))
                                             (jupyter-org-interaction-mode))))
				 (envrc-mode . my/load-ob-jupyter)
				 )
  :init
  (setq org-babel-default-header-args:jupyter-python '((:async . "yes")
                                                       (:pandoc t)
                                                       (:kernel . "python3")))
  (setq org-babel-default-header-args:jupyter-R '((:pandoc t)
                                                  (:async . "yes")
                                                  (:kernel . "ir")))
	(defun my/org-load-jupyter ()
    (org-babel-do-load-languages 'org-babel-load-languages
                                 (append org-babel-load-languages
                                         '((jupyter . t)))))
  (defun my/load-ob-jupyter ()
    ;; only try to load in org-mode
    (when (derived-mode-p 'org-mode)
      ;; skip if already loaded
      (unless (member '(jupyter . t) org-babel-load-languages)
        ;; only load if jupyter is available
        (when (executable-find "jupyter")
					(my/org-load-jupyter)))))
	
  (cl-defmethod jupyter-org--insert-result (_req context result)
    (let ((str
           (org-element-interpret-data
            (jupyter-org--wrap-result-maybe
             context (if (jupyter-org--stream-result-p result)
                         (thread-last result
                           jupyter-org-strip-last-newline
                           jupyter-org-scalar)
                       result)))))
      (if (< (length str) 100000)
          (insert str)
        (insert (format ": Result was too long! Length was %d" (length str)))))
    (when (/= (point) (line-beginning-position))
      ;; Org objects such as file links do not have a newline added when
      ;; converting to their string representation by
      ;; `org-element-interpret-data' so insert one in these cases.
      (insert "\n")))
  :config
  ;;Remove text/html since it's not human readable
  ;; (delete :text/html jupyter-org-mime-types)
  ;; (require 'tramp)
  (with-eval-after-load 'org-src
    (add-to-list 'org-src-lang-modes '("jupyter-python" . python))
    (add-to-list 'org-src-lang-modes '("jupyter-R" . R))))

(use-package org-tree-slide
	:after org
	:hook ((org-tree-slide-play . (lambda () (+remap-faces-at-start-present)))
				 (org-tree-slide-stop . (lambda () (+remap-faces-at-stop-present))))
	:general
	(my/leader-keys
		"t p" '(org-tree-slide-mode :wk "present"))
	(general-nmap
		:keymaps '(org-tree-slide-mode-map org-mode-map)
		"C-j" 'org-tree-slide-move-next-tree
		"C-k" 'org-tree-slide-move-previous-tree)
	:init
	(setq org-tree-slide-activate-message "Presentation mode ON")
	(setq org-tree-slide-deactivate-message "Presentation mode OFF")
	(setq org-tree-slide-indicator nil)
	(setq org-tree-slide-breadcrumbs "    >    ")
	(setq org-tree-slide-heading-emphasis t)
	(setq org-tree-slide-slide-in-waiting 0.025)
	(setq org-tree-slide-content-margin-top 4)
	(defun +remap-faces-at-start-present ()
		(setq-local face-remapping-alist '((default (:height 1.50) variable-pitch)
																			 (fixed-pitch (:height 1.2) fixed-pitch)
																			 ;; (org-verbatim (:height 1.2) org-verbatim)
																			 ;; (org-block (:height 1.2) org-block)
																			 ))
		;; (setq-local olivetti-body-width 95)
		(olivetti-mode 1)
		(display-fill-column-indicator-mode 0)
		(hide-mode-line-mode 1)
		(diff-hl-mode 0)
		(centaur-tabs-mode 0))
	(defun +remap-faces-at-stop-present ()
		(setq-local face-remapping-alist '((default variable-pitch default)))
		;; (setq-local olivetti-body-width 120)
		(olivetti-mode 0)
		(display-fill-column-indicator-mode 1)
		(hide-mode-line-mode 0)
		(doom-modeline-mode 1)
		(diff-hl-mode 1)
		(centaur-tabs-mode 1))
	(setq org-tree-slide-breadcrumbs nil)
	(setq org-tree-slide-header nil)
	(setq org-tree-slide-slide-in-effect nil)
	(setq org-tree-slide-heading-emphasis nil)
	(setq org-tree-slide-cursor-init t)
	(setq org-tree-slide-modeline-display nil)
	(setq org-tree-slide-skip-done nil)
	(setq org-tree-slide-skip-comments t)
	(setq org-tree-slide-fold-subtrees-skipped t)
	(setq org-tree-slide-skip-outline-level 8) ;; or 0?
	(setq org-tree-slide-never-touch-face t)
	;; :config
	;; (org-tree-slide-presentation-profile)
	;; :custom-face
	;; (org-tree-slide-heading-level-1 ((t (:height 1.8 :weight bold))))
	;; (org-tree-slide-heading-level-2 ((t (:height 1.5 :weight bold))))
	;; (org-tree-slide-heading-level-3 ((t (:height 1.5 :weight bold))))
	;; (org-tree-slide-heading-level-4 ((t (:height 1.5 :weight bold))))
	)

(use-package evil-org-mode
  :straight (evil-org-mode :type git :host github :repo "hlissner/evil-org-mode")
  :hook ((org-mode . evil-org-mode)
         (org-mode . (lambda () 
											 (require 'evil-org)
											 (evil-normalize-keymaps)
											 (evil-org-set-key-theme '(textobjects))
                       (require 'evil-org-agenda)
                       (evil-org-agenda-set-keys))))
  :general
	(my/local-leader-keys
    :keymaps 'org-mode-map
    "> l" '(org-indent-item :wk "indent item")
    "< l" '(org-outdent-item-tree :wk "dedent item")
    "> h" '(org-do-demote :wk "indent heading")
    "< h" '(org-do-promote :wk "dedent heading"))
  (general-nmap
    :keymaps 'org-mode-map
		:states 'normal
    "<C-return>"      #'+org/insert-item-below
    "<C-S-return>"    #'+org/insert-item-above
    "RET"   #'+org/dwim-at-point)
  :init
  (defun +org--insert-item (direction)
    (let ((context (org-element-lineage
                    (org-element-context)
                    '(table table-row headline inlinetask item plain-list)
                    t)))
      (pcase (org-element-type context)
        ;; Add a new list item (carrying over checkboxes if necessary)
        ((or `item `plain-list)
         ;; Position determines where org-insert-todo-heading and org-insert-item
         ;; insert the new list item.
         (if (eq direction 'above)
             (org-beginning-of-item)
           (org-end-of-item)
           (backward-char))
         (org-insert-item (org-element-property :checkbox context))
         ;; Handle edge case where current item is empty and bottom of list is
         ;; flush against a new heading.
         (when (and (eq direction 'below)
                    (eq (org-element-property :contents-begin context)
                        (org-element-property :contents-end context)))
           (org-end-of-item)
           (org-end-of-line)))

        ;; Add a new table row
        ((or `table `table-row)
         (pcase direction
           ('below (save-excursion (org-table-insert-row t))
                   (org-table-next-row))
           ('above (save-excursion (org-shiftmetadown))
                   (+org/table-previous-row))))

        ;; Otherwise, add a new heading, carrying over any todo state, if
        ;; necessary.
        (_
         (let ((level (or (org-current-level) 1)))
           ;; I intentionally avoid `org-insert-heading' and the like because they
           ;; impose unpredictable whitespace rules depending on the cursor
           ;; position. It's simpler to express this command's responsibility at a
           ;; lower level than work around all the quirks in org's API.
           (pcase direction
             (`below
              (let (org-insert-heading-respect-content)
                (goto-char (line-end-position))
                (org-end-of-subtree)
                (insert "\n" (make-string level ?*) " ")))
             (`above
              (org-back-to-heading)
              (insert (make-string level ?*) " ")
              (save-excursion (insert "\n"))))
           (when-let* ((todo-keyword (org-element-property :todo-keyword context))
                       (todo-type    (org-element-property :todo-type context)))
             (org-todo
              (cond ((eq todo-type 'done)
                     ;; Doesn't make sense to create more "DONE" headings
                     (car (+org-get-todo-keywords-for todo-keyword)))
                    (todo-keyword)
                    ('todo)))))))

      (when (org-invisible-p)
        (org-show-hidden-entry))
      (when (and (bound-and-true-p evil-local-mode)
                 (not (evil-emacs-state-p)))
        (evil-insert 1))))

  (defun +org/insert-item-below (count)
    "Inserts a new heading, table cell or item below the current one."
    (interactive "p")
    (dotimes (_ count) (+org--insert-item 'below)))

  (defun +org/insert-item-above (count)
    "Inserts a new heading, table cell or item above the current one."
    (interactive "p")
    (dotimes (_ count) (+org--insert-item 'above)))

  (defun +org/dwim-at-point (&optional arg)
    "Do-what-I-mean at point.
      If on a:
      - checkbox list item or todo heading: toggle it.
      - clock: update its time.
      - headline: cycle ARCHIVE subtrees, toggle latex fragments and inline images in
        subtree; update statistics cookies/checkboxes and ToCs.
      - footnote reference: jump to the footnote's definition
      - footnote definition: jump to the first reference of this footnote
      - table-row or a TBLFM: recalculate the table's formulas
      - table-cell: clear it and go into insert mode. If this is a formula cell,
        recaluclate it instead.
      - babel-call: execute the source block
      - statistics-cookie: update it.
      - latex fragment: toggle it.
      - link: follow it
      - otherwise, refresh all inline images in current tree."
    (interactive "P")
    (let* ((context (org-element-context))
           (type (org-element-type context)))
      ;; skip over unimportant contexts
      (while (and context (memq type '(verbatim code bold italic underline strike-through subscript superscript)))
        (setq context (org-element-property :parent context)
              type (org-element-type context)))
      (pcase type
        (`headline
         (cond ((memq (bound-and-true-p org-goto-map)
                      (current-active-maps))
                (org-goto-ret))
               ((and (fboundp 'toc-org-insert-toc)
                     (member "TOC" (org-get-tags)))
                (toc-org-insert-toc)
                (message "Updating table of contents"))
               ((string= "ARCHIVE" (car-safe (org-get-tags)))
                (org-force-cycle-archived))
               ((or (org-element-property :todo-type context)
                    (org-element-property :scheduled context))
                (org-todo
                 (if (eq (org-element-property :todo-type context) 'done)
                     (or (car (+org-get-todo-keywords-for (org-element-property :todo-keyword context)))
                         'todo)
                   'done))))
         ;; Update any metadata or inline previews in this subtree
         (org-update-checkbox-count)
         (org-update-parent-todo-statistics)
         (when (and (fboundp 'toc-org-insert-toc)
                    (member "TOC" (org-get-tags)))
           (toc-org-insert-toc)
           (message "Updating table of contents"))
         (let* ((beg (if (org-before-first-heading-p)
                         (line-beginning-position)
                       (save-excursion (org-back-to-heading) (point))))
                (end (if (org-before-first-heading-p)
                         (line-end-position)
                       (save-excursion (org-end-of-subtree) (point))))
                (overlays (ignore-errors (overlays-in beg end)))
                (latex-overlays
                 (cl-find-if (lambda (o) (eq (overlay-get o 'org-overlay-type) 'org-latex-overlay))
                             overlays))
                (image-overlays
                 (cl-find-if (lambda (o) (overlay-get o 'org-image-overlay))
                             overlays)))
           ;; (+org--toggle-inline-images-in-subtree beg end)
           (if (or image-overlays latex-overlays)
               (org-clear-latex-preview beg end)
             (org--latex-preview-region beg end))))

        (`clock (org-clock-update-time-maybe))

        (`footnote-reference
         (org-footnote-goto-definition (org-element-property :label context)))

        (`footnote-definition
         (org-footnote-goto-previous-reference (org-element-property :label context)))

        ((or `planning `timestamp)
         (org-follow-timestamp-link))

        ((or `table `table-row)
         (if (org-at-TBLFM-p)
             (org-table-calc-current-TBLFM)
           (ignore-errors
             (save-excursion
               (goto-char (org-element-property :contents-begin context))
               (org-call-with-arg 'org-table-recalculate (or arg t))))))

        (`table-cell
         (org-table-blank-field)
         (org-table-recalculate arg)
         (when (and (string-empty-p (string-trim (org-table-get-field)))
                    (bound-and-true-p evil-local-mode))
           (evil-change-state 'insert)))

        (`babel-call
         (org-babel-lob-execute-maybe))

        (`statistics-cookie
         (save-excursion (org-update-statistics-cookies arg)))

        ((or `src-block `inline-src-block)
         (org-babel-execute-src-block arg))

        ((or `latex-fragment `latex-environment)
         (org-latex-preview arg))

        (`link
         (let* ((lineage (org-element-lineage context '(link) t))
                (path (org-element-property :path lineage)))
           (if (or (equal (org-element-property :type lineage) "img")
                   (and path (image-type-from-file-name path)))
               (+org--toggle-inline-images-in-subtree
                (org-element-property :begin lineage)
                (org-element-property :end lineage))
             (org-open-at-point arg))))

        ((guard (org-element-property :checkbox (org-element-lineage context '(item) t)))
         (let ((match (and (org-at-item-checkbox-p) (match-string 1))))
           (org-toggle-checkbox (if (equal match "[ ]") '(16)))))

        (_
         (if (or (org-in-regexp org-ts-regexp-both nil t)
                 (org-in-regexp org-tsr-regexp-both nil  t)
                 (org-in-regexp org-link-any-re nil t))
             (call-interactively #'org-open-at-point)
           (+org--toggle-inline-images-in-subtree
            (org-element-property :begin context)
            (org-element-property :end context))))))))

(use-package org-html-themify
  :straight
  (org-html-themify
   :type git
   :host github
   :repo "DogLooksGood/org-html-themify"
   :files ("*.el" "*.js" "*.css"))
  :hook (org-mode . org-html-themify-mode)
  :init
  (setq org-html-themify-themes
				'((dark . modus-vivendi)
          (light . modus-operandi)))
  :config
	;; otherwise it complains about invalid face
  (require 'hl-line)
	)

(use-package ox-gfm
  :after org)

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

(use-package org-appear
  :straight (org-appear :type git :host github :repo "awth13/org-appear")
	:hook (org-mode . org-appear-mode)
  :init
  (setq org-appear-autoemphasis  t)
  ;; (setq org-appear-autolinks t)
  (setq org-appear-autosubmarkers t)
	)

(use-package weblorg)

(use-package templatel)

(use-package htmlize)

(use-package org-fragtog
	:hook (org-mode . org-fragtog-mode))

  (use-package all-the-icons)

  (use-package doom-modeline
    :demand
    :init
    (setq doom-modeline-buffer-encoding nil)
    (setq doom-modeline-env-enable-python nil)
    (setq doom-modeline-height 15)
    (setq doom-modeline-project-detection 'projectile)
    :config
    (doom-modeline-mode 1)
    (set-face-attribute 'doom-modeline-evil-insert-state nil :foreground "orange")
)

(use-package emacs
	:init
	(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
	(add-to-list 'default-frame-alist '(ns-appearance . dark))
  (setq ns-use-proxy-icon  nil)
  (setq frame-title-format nil)
	)

(use-package modus-themes
	:straight (modus-themes :type git :host gitlab :repo "protesilaos/modus-themes" :branch "main")
	:hook (emacs-startup . my/load-modus-theme)
	:general
	(my/leader-keys
		"t t" '((lambda () (interactive) (modus-themes-toggle)) :wk "toggle theme"))
	:init
	(setq modus-themes-operandi-color-overrides
				'((bg-main . "#fefcf4")
					(bg-dim . "#faf6ef")
					(bg-alt . "#f7efe5")
					(bg-hl-line . "#f4f0e3")
					(bg-active . "#e8dfd1")
					(bg-inactive . "#f6ece5")
					(bg-region . "#c6bab1")
					(bg-header . "#ede3e0")
					(bg-tab-bar . "#dcd3d3")
					(bg-tab-active . "#fdf6eb")
					(bg-tab-inactive . "#c8bab8")
					(fg-unfocused ."#55556f")))
	(setq modus-themes-vivendi-color-overrides
				'((bg-main . "#100b17")
					(bg-dim . "#161129")
					(bg-alt . "#181732")
					(bg-hl-line . "#191628")
					(bg-active . "#282e46")
					(bg-inactive . "#1a1e39")
					(bg-region . "#393a53")
					(bg-header . "#202037")
					(bg-tab-bar . "#262b41")
					(bg-tab-active . "#120f18")
					(bg-tab-inactive . "#3a3a5a")
					(fg-unfocused . "#9a9aab")))
	(setq modus-themes-slanted-constructs t
				modus-themes-bold-constructs t
				modus-themes-fringes 'nil ; {nil,'subtle,'intense}
				modus-themes-mode-line '3d ; {nil,'3d,'moody}
				modus-themes-intense-hl-line nil
				modus-themes-prompts nil ; {nil,'subtle,'intense}
				modus-themes-completions 'moderate ; {nil,'moderate,'opinionated}
				modus-themes-diffs nil ; {nil,'desaturated,'fg-only}
				modus-themes-org-blocks 'greyscale ; {nil,'greyscale,'rainbow}
				modus-themes-headings  ; Read further below in the manual for this one
				'((1 . line)
					(t . rainbow-line-no-bold))
				modus-themes-variable-pitch-headings nil
				modus-themes-scale-headings t
				modus-themes-scale-1 1.1
				modus-themes-scale-2 1.15
				modus-themes-scale-3 1.21
				modus-themes-scale-4 1.27
				modus-themes-scale-5 1.33)
	(defun my/load-modus-theme ()
		;;Light for the day
		(run-at-time "07:00" (* 60 60 24)
								 (lambda ()
									 (modus-themes-load-operandi)
									 (with-eval-after-load 'org
										 (plist-put org-format-latex-options :foreground "black"))
									 ))
		;; Dark for the night
		(run-at-time "00:00" (* 60 60 24)
								 (lambda ()
									 (modus-themes-load-vivendi)
									 (with-eval-after-load 'org
										 (plist-put org-format-latex-options :foreground "whitesmoke"))
									 ))
		(run-at-time "17:00" (* 60 60 24)
								 (lambda ()
									 (modus-themes-load-vivendi)
									 (with-eval-after-load 'org
										 (plist-put org-format-latex-options :foreground "whitesmoke"))
									 ))))

(use-package dashboard
  :after projectile
  ;; :hook
  ;; (dashboard-after-initialize . (lambda () (setq-local cursor-type nil)))
  :demand
  :init
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (setq dashboard-center-content t)
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
	(defun is-after-17-or-weekends? ()
		(or (-> (nth 4 (split-string (current-time-string) " ")) ; time of the day e.g. 18
															 (substring 0 2)
															 (string-to-number)
															 (> 16)
															 )
													 (-> (substring (current-time-string) 0 3) ; day of the week e.g. Fri
															 (member  '("Sat" "Sun")))))
	;; exclude work items after 17 and on weekends
  (run-at-time "00:00" (* 60 60 24)
							 (lambda ()
								 (if (is-after-17-or-weekends?)
									 (setq dashboard-match-agenda-entry "life")
                   (setq dashboard-match-agenda-entry "work|life"))))
  (setq dashboard-items '((recents  . 5)
                          (agenda . 5)
                          ;; (bookmarks . 5)
                          ;; (projects . 5)
                          ))
  ;; (setq dashboard-startup-banner [VALUE])
	;; (setq dashboard-navigator-buttons
  ;;  `((;; Github
  ;;     (,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
  ;;      "Github"
  ;;      "Browse github"
  ;;      (lambda (&rest _) (browse-url "https://github.com/")))
  ;;     ;; Codebase
  ;;     ;; (,(all-the-icons-faicon "briefcase" :height 1.1 :v-adjust -0.1)
  ;;     ;;  "Codebase"
  ;;     ;;  "My assigned tickets"
  ;;     ;;  (lambda (&rest _) (browse-url "https://hipo.codebasehq.com/tickets")))
  ;;     ;; Perspective
  ;;     (,(all-the-icons-octicon "history" :height 1.1 :v-adjust 0.0)
  ;;      "Reload last session"
  ;;      "Reload last session"
  ;;      (lambda (&rest _) (persp-state-load persp-state-default-file))))))
  :config
  (dashboard-setup-startup-hook)
	)

(use-package centaur-tabs
  :hook (emacs-startup . centaur-tabs-mode)
  :general
  (general-nmap "gt" 'centaur-tabs-forward
    "gT" 'centaur-tabs-backward)
	(my/leader-keys
		"b K" '(centaur-tabs-kill-other-buffers-in-current-group :wk "kill other buffers"))
  :init
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-set-modified-marker t
        centaur-tabs-modified-marker "M"
        centaur-tabs-cycle-scope 'tabs)
  (setq centaur-tabs-set-close-button nil)
  :config
  (centaur-tabs-mode t)
  ;; (centaur-tabs-headline-match)
  (centaur-tabs-group-by-projectile-project)
  )

  (use-package centered-cursor-mode
    :general (my/leader-keys "t -" (lambda () (interactive) (centered-cursor-mode 'toggle))))

  (use-package hide-mode-line
    :commands (hide-mode-line-mode))

  (setq display-buffer-alist
        `((,(rx bos (or "*Apropos*" "*Help*" "*helpful" "*info*" "*Summary*") (0+ not-newline))
           (display-buffer-reuse-mode-window display-buffer-below-selected)
           (window-height . 0.33)
           (mode apropos-mode help-mode helpful-mode Info-mode Man-mode))))

(use-package winum
:general
(my/leader-keys
"1" '(winum-select-window-1 :wk "win 1")
"2" '(winum-select-window-2 :wk "win 2")
"3" '(winum-select-window-3 :wk "win 3"))
:config
(winum-mode))

  (use-package transpose-frame
    :general
    (my/leader-keys
      "w t" '(transpose-frame :wk "transpose")
      "w f" '(rotate-frame :wk "flip")))

(use-package persistent-scratch
	:hook
  (org-mode . (lambda ()
                "only set initial-major-mode after loading org"
                (setq initial-major-mode 'org-mode)))
	:general
	(my/leader-keys
    "bs" '((lambda ()
             "Load persistent-scratch if not already loaded"
						 (interactive)
						 (progn 
               (unless (boundp 'persistent-scratch-mode)
                 (require 'persistent-scratch))
               (pop-to-buffer "*scratch*")))
           :wk "scratch"))
  :init
  (setq persistent-scratch-autosave-interval 60)
  :config
	(persistent-scratch-setup-default))

  (use-package olivetti
    :general
    (my/leader-keys
      "t o" '(olivetti-mode :wk "olivetti"))
    :init
    (setq olivetti-body-width 100)
    (setq olivetti-recall-visual-line-mode-entry-state t))

(use-package display-fill-column-indicator
  :straight (:type built-in)
  :hook
  ((prog-mode org-mode) . display-fill-column-indicator-mode))

;; add a visual intent guide
(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :init
  ;; (setq highlight-indent-guides-method 'column)
  (setq highlight-indent-guides-method 'character)
  ;; (setq highlight-indent-guides-character ?|)
  ;; (setq highlight-indent-guides-character ?❚)
  (setq highlight-indent-guides-character ?‖)
  (setq highlight-indent-guides-responsive 'stack)
	;; (setq highlight-indent-guides-auto-enabled nil)
	;; (set-face-background 'highlight-indent-guides-odd-face "darkgray")
  ;; (set-face-background 'highlight-indent-guides-even-face "dimgray")
  ;; (set-face-foreground 'highlight-indent-guides-character-face "dimgray")
  )

(use-package emojify
  :disabled t
  :hook (after-init . global-emojify-mode))

  (use-package selectrum
    :demand
    :general
    (selectrum-minibuffer-map "C-j" 'selectrum-next-candidate
                              "C-k" 'selectrum-previous-candidate)
    :config
    (selectrum-mode t)
    )

  (use-package selectrum-prescient
    :after selectrum
    :demand
    :config
    (prescient-persist-mode t)
    (selectrum-prescient-mode t)
    )

  (use-package company-prescient
    :after company
    :demand
    :config
    (company-prescient-mode t))

  (use-package marginalia
    :after selectrum
    :demand
    :init
    (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
    :config (marginalia-mode t))

(use-package embark
  :general
  (general-nmap "C-l" 'embark-act)
  (selectrum-minibuffer-map "C-l" #'embark-act)
  (embark-file-map "o" 'find-file-other-window)	
  :config
  ;; For Selectrum users:
  (defun current-candidate+category ()
    (when selectrum-active-p
      (cons (selectrum--get-meta 'category)
            (selectrum-get-current-candidate))))

  (add-hook 'embark-target-finders #'current-candidate+category)

  (defun current-candidates+category ()
    (when selectrum-active-p
      (cons (selectrum--get-meta 'category)
            (selectrum-get-current-candidates
             ;; Pass relative file names for dired.
             minibuffer-completing-file-name))))

  (add-hook 'embark-candidate-collectors #'current-candidates+category)

  ;; No unnecessary computation delay after injection.
  (add-hook 'embark-setup-hook 'selectrum-set-selected-candidate)
  )

(use-package embark-consult
  :straight (embark-consult :type git :host github :repo "oantolin/embark" :files ("embark-consult.el"))
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . embark-consult-preview-minor-mode))

(use-package consult
  :general
	(general-nmap
		:states '(normal insert)
    "C-p" 'consult-yank-pop)
  (my/leader-keys
    "s i" '(consult-isearch :wk "isearch")
    "s o" '(consult-outline :which-key "outline")
    "s s" 'consult-line
    "b b" 'consult-buffer
    ;; TODO consult mark
    "f r" 'consult-recent-file
    "s !" '(consult-flymake :wk "flymake"))
	(with-eval-after-load 'projectile
    (my/leader-keys
      "s p" '((lambda () (interactive) (consult-ripgrep (projectile-project-root))) :wk "ripgrep")))
	:config
	(autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root)
  ;; :init
  ;; (setq consult-preview-key "C-l")
  ;; (setq consult-narrow-key ">")
  )

(use-package consult-selectrum
  :after selectrum
  :demand)

(use-package projectile
  :demand
  :general
  (my/leader-keys
    "p" '(:keymap projectile-command-map :which-key "project")
    "p a" '(projectile-add-known-project :wk "add known")
    "p t" '(projectile-run-vterm :wk "term"))
  :init
  (when (file-directory-p "~/git")
    (setq projectile-project-search-path '("~/git")))
  (setq projectile-completion-system 'default)
  (setq projectile-switch-project-action #'projectile-find-file)
  (setq projectile-project-root-files '(".envrc" ".projectile" "project.clj" "deps.edn"))
	(setq projectile-switch-project-action 'projectile-commander)
	;; Do not include straight repos (emacs packages) to project list
	(setq projectile-ignored-project-function
   (lambda (project-root)
     (string-prefix-p (expand-file-name "straight/" user-emacs-directory) project-root)))
  :config
  (defadvice projectile-project-root (around ignore-remote first activate)
    (unless (file-remote-p default-directory) ad-do-it))
  (projectile-mode)
	;; projectile commander methods
	(setq projectile-commander-methods nil)
	(def-projectile-commander-method ?? "Commander help buffer."
		(ignore-errors (kill-buffer projectile-commander-help-buffer))
		(with-current-buffer (get-buffer-create projectile-commander-help-buffer)
			(insert "Projectile Commander Methods:\n\n")
			(dolist (met projectile-commander-methods)
				(insert (format "%c:\t%s\n" (car met) (cadr met))))
			(goto-char (point-min))
      (help-mode)
      (display-buffer (current-buffer) t))
    (projectile-commander))
	(def-projectile-commander-method ?t
    "Open a *shell* buffer for the project."
    (projectile-run-vterm))
	(def-projectile-commander-method ?\C-? ;; backspace
		"Go back to project selection."
		(projectile-switch-project))
	(def-projectile-commander-method ?d
    "Open project root in dired."
    (projectile-dired))
	(def-projectile-commander-method ?f
    "Find file in project."
    (projectile-find-file))
	(def-projectile-commander-method ?s
    "Ripgrep in project."
    (projectile-find-file))
	(def-projectile-commander-method ?g
    "Git status in project."
    (projectile-vc))
  )

  (use-package perspective
    :commands (persp-new persp-switch)
    :general
    (my/leader-keys
      "<tab>" '(:ignore true :wk "tab")
      "<tab> <tab>" 'persp-switch
      "<tab> `" 'persp-switch-last
      "<tab> d" 'persp-kill
      "<tab> x" '((lambda () (interactive) (persp-kill (persp-current-name))) :wk "kill current")
      "<tab> X" '((lambda () (interactive) (persp-kill (persp-names))) :wk "kill all")
      "<tab> n" '(my/new-tab :wk "new"))
    :init
    (defun my/new-tab ()
      "Jump to the dashboard buffer, if doesn't exists create one."
      (interactive)
      ;; (persp-new (concat "tab " (+ 1 (int (length (persp-names))))))
      (persp-new "main")
      (persp-switch "main")
      (switch-to-buffer dashboard-buffer-name)
      (dashboard-mode)
      (dashboard-insert-startupify-lists)
      (dashboard-refresh-buffer))
    :config
    (persp-mode))

(use-package persp-projectile
  :general

  (my/leader-keys
    "p p" 'projectile-persp-switch-project
		;; "<tab> o"	'((lambda () (interactive) (projectile-persp-switch-project "org")) :wk "org")
    ;; "p P" '((lambda ()
    ;;           (setq projectile-switch-project-action 'projectile-commander)
    ;;           (setq projectile-switch-project-action 'projectile-find-file)
		;; 					(interactive)
    ;;           (projectile-persp-switch-project))
    ;;         :wk "project commander" )
    ))

(use-package magit
  :general
  (my/leader-keys
    "g g" 'magit-status
    "g G" 'magit-status-here
    "g l" '(magit-log :wk "log"))
	(general-nmap
		:keymaps '(magit-status-mode-map
     magit-stash-mode-map
     magit-revision-mode-map
     magit-process-mode-map
     magit-diff-mode-map)
		 "<tab>" #'magit-section-toggle)
  :init
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (setq magit-log-arguments '("--graph" "--decorate" "--color"))
	(setq git-commit-fill-column 72)
	:config
	  (evil-define-key* 'normal magit-status-mode-map [escape] nil)

	(evil-define-key* '(normal visual) magit-mode-map
    "zz" #'evil-scroll-line-to-center)
  )

  (use-package git-timemachine
    :hook (git-time-machine-mode . evil-normalize-keymaps)
    :init (setq git-timemachine-show-minibuffer-details t)
    :general
    (general-nmap "SPC g t" 'git-timemachine-toggle)
    (git-timemachine-mode-map
     "C-k" 'git-timemachine-show-previous-revision
     "C-j" 'git-timemachine-show-next-revision
     "q" 'git-timemachine-quit))

(use-package diff-hl
  :demand
	:general
	(my/leader-keys
    "g n" '(diff-hl-next-hunk :wk "next hunk")
    "g p" '(diff-hl-previous-hunk :wk "prev hunk"))
  :hook
  ((magit-pre-refresh . diff-hl-magit-pre-refresh)
   (magit-post-refresh . diff-hl-magit-post-refresh))
  :init
  (setq diff-hl-draw-borders nil)
	;; (setq diff-hl-global-modes '(not org-mode))
  ;; (setq diff-hl-fringe-bmp-function 'diff-hl-fringe-bmp-from-type)
  ;; (setq diff-hl-global-modes (not '(image-mode org-mode)))
  :config
  (global-diff-hl-mode)
  )

  (use-package smerge-mode
    :straight (:type built-in)
		:after hydra
    :general
    (my/leader-keys "g m" 'hydra-smerge/body)
		:hook
		(magit-diff-visit-file . (lambda ()
                             (when smerge-mode
                               (smerge-hydra/body))))
    :init
    (defhydra smerge-hydra (:hint nil
                                  :pre (smerge-mode 1)
                                  ;; Disable `smerge-mode' when quitting hydra if
                                  ;; no merge conflicts remain.
                                  :post (smerge-auto-leave))
      "
                                                    ╭────────┐
  Movement   Keep           Diff              Other │ smerge │
  ╭─────────────────────────────────────────────────┴────────╯
     ^_g_^       [_b_] base       [_<_] upper/base    [_C_] Combine
     ^_C-k_^     [_u_] upper      [_=_] upper/lower   [_r_] resolve
     ^_k_ ↑^     [_l_] lower      [_>_] base/lower    [_R_] remove
     ^_j_ ↓^     [_a_] all        [_H_] hightlight
     ^_C-j_^     [_RET_] current  [_E_] ediff             ╭──────────
     ^_G_^                                            │ [_q_] quit"
      ("g" (progn (goto-char (point-min)) (smerge-next)))
      ("G" (progn (goto-char (point-max)) (smerge-prev)))
      ("C-j" smerge-next)
      ("C-k" smerge-prev)
      ("j" next-line)
      ("k" previous-line)
      ("b" smerge-keep-base)
      ("u" smerge-keep-upper)
      ("l" smerge-keep-lower)
      ("a" smerge-keep-all)
      ("RET" smerge-keep-current)
      ("\C-m" smerge-keep-current)
      ("<" smerge-diff-base-upper)
      ("=" smerge-diff-upper-lower)
      (">" smerge-diff-base-lower)
      ("H" smerge-refine)
      ("E" smerge-ediff)
      ("C" smerge-combine-with-next)
      ("r" smerge-resolve)
      ("R" smerge-kill-current)
      ("q" nil :color blue)))

(use-package hydra
  :demand)

  (use-package rainbow-delimiters
    :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
           (clojure-mode . rainbow-delimiters-mode)))

  (use-package tree-sitter
    :hook (python-mode . (lambda ()
                           (require 'tree-sitter)
                           (require 'tree-sitter-langs)
                           (require 'tree-sitter-hl)
                           (tree-sitter-hl-mode))))

  (use-package tree-sitter-langs
    :after tree-sitter)

(use-package company
  :demand
	;; :hook
	;; (python-mode . (lambda ()
	;; 								(setq-local company-backends '((company-capf :with company-files)))))
  :init
  (setq company-backends '((company-capf company-files)))
  (setq company-minimum-prefix-length 1)
	(setq company-idle-delay 0.0)
  (setq company-tooltip-align-annotations t)
	;; don't autocomplete when single candidate
	(setq company-auto-complete nil)
	(setq company-auto-complete-chars nil)
	(setq company-dabbrev-code-other-buffers nil)
	;; manually configure tng
	;; (setq company-tng-auto-configure nil)
	;; (setq company-frontends '(company-tng-frontend
  ;;                           company-pseudo-tooltip-frontend
  ;;                           company-echo-metadata-frontend))
	;; (setq company-selection-default nil)
  :config
  (global-company-mode)
  (with-eval-after-load 'evil
    (add-hook 'company-mode-hook #'evil-normalize-keymaps))
	;; needed in case we only have one candidate
	(define-key company-active-map (kbd "C-j") 'company-select-next)
	)

  (use-package company-box
    :hook (company-mode . company-box-mode)
    :config
    (setq company-box-show-single-candidate t
          company-box-backends-colors nil
          company-box-max-candidates 50
          company-box-icons-alist 'company-box-icons-all-the-icons
          company-box-icons-all-the-icons
          (let ((all-the-icons-scale-factor 0.8))
            `((Unknown       . ,(all-the-icons-material "find_in_page"             :face 'all-the-icons-purple))
              (Text          . ,(all-the-icons-material "text_fields"              :face 'all-the-icons-green))
              (Method        . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
              (Function      . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
              (Constructor   . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
              (Field         . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
              (Variable      . ,(all-the-icons-material "adjust"                   :face 'all-the-icons-blue))
              (Class         . ,(all-the-icons-material "class"                    :face 'all-the-icons-red))
              (Interface     . ,(all-the-icons-material "settings_input_component" :face 'all-the-icons-red))
              (Module        . ,(all-the-icons-material "view_module"              :face 'all-the-icons-red))
              (Property      . ,(all-the-icons-material "settings"                 :face 'all-the-icons-red))
              (Unit          . ,(all-the-icons-material "straighten"               :face 'all-the-icons-red))
              (Value         . ,(all-the-icons-material "filter_1"                 :face 'all-the-icons-red))
              (Enum          . ,(all-the-icons-material "plus_one"                 :face 'all-the-icons-red))
              (Keyword       . ,(all-the-icons-material "filter_center_focus"      :face 'all-the-icons-red))
              (Snippet       . ,(all-the-icons-material "short_text"               :face 'all-the-icons-red))
              (Color         . ,(all-the-icons-material "color_lens"               :face 'all-the-icons-red))
              (File          . ,(all-the-icons-material "insert_drive_file"        :face 'all-the-icons-red))
              (Reference     . ,(all-the-icons-material "collections_bookmark"     :face 'all-the-icons-red))
              (Folder        . ,(all-the-icons-material "folder"                   :face 'all-the-icons-red))
              (EnumMember    . ,(all-the-icons-material "people"                   :face 'all-the-icons-red))
              (Constant      . ,(all-the-icons-material "pause_circle_filled"      :face 'all-the-icons-red))
              (Struct        . ,(all-the-icons-material "streetview"               :face 'all-the-icons-red))
              (Event         . ,(all-the-icons-material "event"                    :face 'all-the-icons-red))
              (Operator      . ,(all-the-icons-material "control_point"            :face 'all-the-icons-red))
              (TypeParameter . ,(all-the-icons-material "class"                    :face 'all-the-icons-red))
              (Template      . ,(all-the-icons-material "short_text"               :face 'all-the-icons-green))
              (ElispFunction . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
              (ElispVariable . ,(all-the-icons-material "check_circle"             :face 'all-the-icons-blue))
              (ElispFeature  . ,(all-the-icons-material "stars"                    :face 'all-the-icons-orange))
              (ElispFace     . ,(all-the-icons-material "format_paint"             :face 'all-the-icons-pink)))))

    ;; Disable tab-bar in company-box child frames
    (add-to-list 'company-box-frame-parameters '(tab-bar-lines . 0))
    )

(use-package inheritenv
  :straight (inheritenv :type git :host github :repo "purcell/inheritenv"))

(use-package envrc
  :straight (envrc :type git :host github :repo "purcell/envrc")
  :commands (envrc-mode)
  :hook ((python-mode . envrc-mode)
         (org-jupyter-python-mode . envrc-mode))
  )

(use-package yasnippet
	:general
	(yas-minor-mode-map
	 :states 'insert
   "<tab>" 'nil
   "C-<tab>" 'yas-expand)
  :hook
  ((text-mode . yas-minor-mode)
   (prog-mode . yas-minor-mode)
	 (dap-ui-repl-mode . yas-minor-mode)
   (org-mode . yas-minor-mode))
	:init
  ;; (setq yas-prompt-functions '(yas-ido-prompt))
	(defun my-yas-try-expanding-auto-snippets ()
    (when (and (boundp 'yas-minor-mode) yas-minor-mode)
      (let ((yas-buffer-local-condition ''(require-snippet-condition . auto)))
        (yas-expand))))
  :config
  (yas-reload-all)
  (add-hook 'post-command-hook #'my-yas-try-expanding-auto-snippets)
  )

  (use-package undo-fu
    :general
    (:states 'normal
             "u" 'undo-fu-only-undo
             "\C-r" 'undo-fu-only-redo))

  (use-package vterm
    :config
    (setq vterm-shell (executable-find "fish")
          vterm-max-scrollback 10000))

(use-package vterm-toggle
  :general
  (my/leader-keys
    "'" 'vterm-toggle))

(use-package dired
  :straight (:type built-in)
  :hook
	(dired-mode . dired-hide-details-mode)
  :general
  (my/leader-keys
    "f d" 'dired
    "f j" 'dired-jump)
	:init
	(setq dired-dwim-target t))

(use-package dired-single
  :after dired
  :general
  (dired-mode-map
   :states 'normal
   "h" 'dired-single-up-directory
   "l" 'dired-single-buffer
   "q" 'kill-current-buffer))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

  (use-package restart-emacs
    :general
    (my/leader-keys
      "R" '(restart-emacs :wk "restart"))
    )

(use-package toml-mode
	:mode "\\.toml\\'")

(use-package lsp-mode
	:commands (lsp lsp-deferred)
	:hook
	(lsp-mode . (lambda ()
								(setq-local evil-lookup-func #'lsp-describe-thing-at-point)
								(setq-local evil-goto-definition-functions
														'(lambda (&rest args) (lsp-find-definition)))))
	(lsp-mode . lsp-enable-which-key-integration)
	:general
	(my/local-leader-keys
		:states 'normal
		:keymaps 'lsp-mode-map
		"i" '(:ignore t :which-key "import")
		"i o" '(lsp-organize-imports :wk "optimize")
		"l" '(:keymap lsp-command-map :wk "lsp")
		"r" '(lsp-rename :wk "rename"))
	;; (lsp-mode-map
	;;  :states 'normal "gD" 'lsp-find-references)
	:init
	(setq lsp-restart 'ignore)
	(setq lsp-eldoc-enable-hover nil)
	(setq lsp-enable-file-watchers nil)
	(setq lsp-signature-auto-activate nil)
	(setq lsp-modeline-diagnostics-enable nil)
	(setq lsp-keep-workspace-alive nil)
	(setq lsp-auto-execute-action nil)
	(setq lsp-before-save-edits nil)
	)

(use-package lsp-ui
	:hook ((lsp-mode . lsp-ui-mode))
	:general
	(lsp-mode-map
	 :states 'normal "gD" 'lsp-ui-peek-find-references)
	:init
	(setq lsp-ui-doc-show-with-cursor nil)
	(setq lsp-ui-doc-show-with-mouse nil)
	(setq lsp-ui-peek-always-show t)
	(setq lsp-ui-peek-fontify 'always)
	)

  (use-package dap-mode
    :hook
    (dap-terminated . my/hide-debug-windows)
    :general
    (my/local-leader-keys
      :keymaps 'python-mode-map
      "d d" '(dap-debug :wk "debug")
      "d b" '(dap-breakpoint-toggle :wk "breakpoint")
      "d c" '(dap-continue :wk "continue")
      "d n" '(dap-next :wk "next")
      "d e" '(dap-eval-thing-at-point :wk "eval")
      "d i" '(dap-step-in :wk "step in")
      "d q" '(dap-disconnect :wk "quit")
      "d r" '(dap-ui-repl :wk "repl")
      "d h" '(dap-hydra :wk "hydra"))
    :init
    ;; (setq dap-auto-configure-features '(locals repl))
    (setq dap-auto-configure-features '(repl))
    (setq dap-python-debugger 'debugpy)
    ;; show stdout
    (setq dap-auto-show-output t)
    (setq dap-output-window-max-height 50)
    (setq dap-output-window-min-height 50)
    ;; hide stdout window  when done
    (defun my/hide-debug-windows (session)
      "Hide debug windows when all debug sessions are dead."
      (unless (-filter 'dap--session-running (dap--get-sessions))
        (kill-buffer (dap--debug-session-output-buffer (dap--cur-session-or-die)))))
    (defun my/dap-python--executable-find (orig-fun &rest args)
      (executable-find "python"))
    :config
    ;; configure windows
    (require 'dap-ui)
    (setq dap-ui-buffer-configurations
          `(;; (,dap-ui--locals-buffer . ((side . right) (slot . 1) (window-width . 0.50)))
            ;; (,dap-ui--breakpoints-buffer . ((side . left) (slot . 1) (window-width . ,treemacs-width)))
            ;; (,dap-ui--sessions-buffer . ((side . left) (slot . 2) (window-width . ,treemacs-width)))
            (,dap-ui--repl-buffer . ((side . right) (slot . 2) (window-width . 0.50)))))
    (dap-ui-mode 1)
    ;; python virtualenv
    (require 'dap-python)
    (advice-add 'dap-python--pyenv-executable-find :around #'my/dap-python--executable-find)
    ;; debug templates
    (defvar dap-script-args (list :type "python"
                                  :args []
                                  :cwd "${workspaceFolder}"
                                  :justMyCode :json-false
                                  :request "launch"
                                  :debugger 'debugpy
                                  :name "dap-debug-script"))
    (defvar dap-test-args (list :type "python-test-at-point"
                                :args ""
                                :justMyCode :json-false
                                ;; :cwd "${workspaceFolder}"
                                :request "launch"
                                :module "pytest"
                                :debugger 'debugpy
                                :name "dap-debug-test-at-point"))
    (defvar empties-forecast (list
                              :name "empties forecast"
                              :type "python"
                              :request "launch"
                              :program "./src/empties/forecasting/predict.py"
                              :env '(("NO_JSON_LOG" . "true"))
                              :args ["--source01" "./data/empties-history-sample.parquet"
                                     "--source02" "./data/model_selection.files"
                                     "--source03" "./data/booking-feature-sample.parquet"
                                     "--source04" "./data/holiday-2019-05-24-1558683595"
                                     "--output-data" "./data/predictions.parquet"
                                     "--output-metrics" "./data/metrics.json"]
                              ))
    (dap-register-debug-template "dap-debug-script" dap-script-args)
    (dap-register-debug-template "dap-debug-test-at-point" dap-test-args)
    ;; bind the templates
    (my/local-leader-keys
      :keymaps 'python-mode-map
      "d t" '((lambda () (interactive) (dap-debug dap-test-args)) :wk "test")
      "d s" '((lambda () (interactive) (dap-debug dap-script-args)) :wk "script")
      )
    )

(use-package python-mode
  :hook ((envrc-mode . (lambda ()
                         (when (executable-find "ipython")
                           (setq python-shell-interpreter (executable-find "ipython"))))))
	:general
	(python-mode-map :states 'normal "gz" nil)
	:init
  (setq python-indent-offset 0)
  :config
  (setq python-shell-interpreter (executable-find "ipython")     ;; FIXME
        python-shell-interpreter-args "-i --simple-prompt --no-color-info"
        python-shell-prompt-regexp "In \\[[0-9]+\\]: "
        python-shell-prompt-block-regexp "\\.\\.\\.\\.: "
        python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
        python-shell-completion-setup-code
        "from IPython.core.completerlib import module_completion"
        python-shell-completion-string-code
        "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"))

  (use-package lsp-pyright
    :init
    (setq lsp-pyright-typechecking-mode "basic") ;; too much noise in "real" projects
    :hook (python-mode . (lambda ()
                           (require 'lsp-pyright)
                           (lsp-deferred))))

(use-package python-pytest
  :general
  (my/local-leader-keys
    :keymaps 'python-mode-map
		"t" '(:ignore t :wk "test")
    "t d" '(python-pytest-dispatch :wk "dispatch")
    "t f" '(python-pytest-file-dwim :wk "file")
    "t t" '(python-pytest-function :wk "function"))
  :init
  (setq python-pytest-arguments '("--color" "--failed-first"))
  (defun my/pytest-use-venv (orig-fun &rest args)
    (if-let ((python-pytest-executable (executable-find "pytest")))
        (apply orig-fun args)
      (apply orig-fun args)))
  :config
  (advice-add 'python-pytest--run :around #'my/pytest-use-venv)
  )

  (use-package flymake
    :straight (:type built-in)
    :hook (emacs-lisp-mode . flymake-mode)
    :init
    (setq python-flymake-command (executable-find "flake8"))
    (setq flymake-fringe-indicator-position 'right-fringe)
    :general
    (general-nmap "] !" 'flymake-goto-next-error)
    (general-nmap "[ !" 'flymake-goto-prev-error)
    )

(use-package jupyter
  :straight (:no-native-compile t :no-byte-compile t) ;; otherwise we get jupyter-channel void
  :general
	(my/local-leader-keys
    :keymaps 'python-mode-map
    "'" '(my/jupyter-repl :wk "jupyter REPL")
    "e e" '(jupyter-eval-line-or-region :wk "line")
    "e d" '(jupyter-eval-defun :wk "defun")
    "e b" '((lambda () (interactive) (my/jupyter-eval-buffer)) :wk "buffer")
    "k" '(:ignore true :wk "kernel")
    "k i" '(jupyter-org-interrupt-kernel :wk "restart kernel")
		"k r" '(jupyter-repl-restart-kernel :wk "restart kernel"))
	(my/local-leader-keys
		:keymaps 'python-mode-map
		:states 'visual
		"e" '(jupyter-eval-region :wk "eval"))
	:init
	(setq jupyter-repl-prompt-margin-width 4)
	(defun jupyter-command-venv (&rest args)
		"This overrides jupyter-command to use the virtualenv's jupyter"
		(let ((jupyter-executable (executable-find "jupyter")))
			(with-temp-buffer
				(when (zerop (apply #'process-file jupyter-executable nil t nil args))
					(string-trim-right (buffer-string))))))
	(defun my/jupyter-eval-buffer ()
		"Send the contents of BUFFER using `jupyter-current-client'."
		(interactive)
		(jupyter-eval-string (jupyter-load-file-code (buffer-file-name))))
	(defun my/jupyter-repl ()
		"If a buffer is already associated with a jupyter buffer, then pop to it. Otherwise start a jupyter kernel."
		(interactive)
		(if (bound-and-true-p jupyter-current-client)
				(jupyter-repl-pop-to-buffer)
			(call-interactively 'jupyter-repl-associate-buffer)))
	(advice-add 'jupyter-command :override #'jupyter-command-venv))

(use-package pyimport
  :general
  (my/local-leader-keys
    :keymaps 'python-mode-map
    "i i" '(pyimport-insert-missing :wk "autoimport")))

(use-package blacken
	:general
	(my/local-leader-keys
      :keymaps 'python-mode-map
      "=" '(blacken-buffer :wk "format"))
	)

    (use-package ess
    :init
  (setq ess-eval-visibly 'nowait)
(setq ess-R-font-lock-keywords '((ess-R-fl-keyword:keywords . t)
                                   (ess-R-fl-keyword:constants . t)
                                   (ess-R-fl-keyword:modifiers . t)
                                   (ess-R-fl-keyword:fun-defs . t)
                                   (ess-R-fl-keyword:assign-ops . t)
                                   (ess-R-fl-keyword:%op% . t)
                                   (ess-fl-keyword:fun-calls . t)
                                   (ess-fl-keyword:numbers . t)
                                   (ess-fl-keyword:operators . t)
                                   (ess-fl-keyword:delimiters . t)
                                   (ess-fl-keyword:= . t)
                                   (ess-R-fl-keyword:F&T . t)))
      )

(use-package elisp-mode
  :straight (:type built-in)
  :general
  (my/local-leader-keys
    :keymaps '(org-mode-map emacs-lisp-mode-map lisp-interaction-mode-map)
    "e l" '(eval-last-sexp :wk "last sexp")
		"e b" '(eval-buffer :wk "buffer"))
  (my/local-leader-keys
    :keymaps '(org-mode-map emacs-lisp-mode-map lisp-interaction-mode-map)
    :states 'visual
    "e" '(eval-region :wk "sexp"))
  )

(use-package evil-lisp-state
  :after evil
  :demand
  :init
  (setq evil-lisp-state-enter-lisp-state-on-command nil)
  (setq evil-lisp-state-global t)
  ;; (setq evil-lisp-state-major-modes '(org-mode emacs-lisp-mode clojure-mode clojurescript-mode lisp-interaction-mode))
  :config
  (evil-lisp-state-leader "SPC l")
  )

(use-package nix-mode
:mode "\\.nix\\'")

(use-package clojure-mode
  :mode "\\.clj$"
  :init
  (setq clojure-align-forms-automatically t))

(use-package cider
  :hook ((cider-repl-mode . evil-normalize-keymaps)
         (cider-mode . (lambda ()
                           (setq-local evil-lookup-func #'cider-doc)))
         (cider-mode . eldoc-mode))
  :general
  (my/local-leader-keys
    :keymaps 'clojure-mode-map
    "c" '(cider-connect-clj :wk "connect")
    "C" '(cider-connect-cljs :wk "connect (cljs)")
    "j" '(cider-jack-in :wk "jack in")
    "J" '(cider-jack-in-cljs :wk "jack in (cljs)")
    "e l" 'cider-eval-last-sexp
    "e E" 'cider-pprint-eval-last-sexp-to-comment
    "e d" '(cider-eval-defun-at-point :wk "defun")
    "e D" 'cider-pprint-eval-defun-to-comment
		"K" 'cider-doc
		"q" '(cider-quit :qk "quit")
		)
  (my/local-leader-keys
    :keymaps 'clojure-mode-map
    :states 'visual
    "e" 'cider-eval-region)
  :init
  (setq nrepl-hide-special-buffers t)
  (setq nrepl-sync-request-timeout nil)
	(setq cider-repl-display-help-banner nil)
	(defun mpereira/cider-eval-sexp-at-point (&optional output-to-current-buffer)
    "Evaluate the expression around point.
If invoked with OUTPUT-TO-CURRENT-BUFFER, output the result to current buffer."
    (interactive "P")
    (save-excursion
      (goto-char (- (cadr (cider-sexp-at-point 'bounds))
                    1))
      (cider-eval-last-sexp output-to-current-buffer)))
  )

(use-package org
:config
(require 'ob-clojure)
(setq org-babel-clojure-backend 'cider))

(use-package evil-cleverparens
  :hook
  ((emacs-lisp-mode . evil-cleverparens-mode)
   (clojure-mode . evil-cleverparens-mode))
  :init
  (setq evil-move-beyond-eol t
        evil-cleverparens-use-additional-bindings nil
        evil-cleverparens-use-s-and-S nil
        ;; evil-cleverparens-swap-move-by-word-and-symbol t
        ;; evil-cleverparens-use-regular-insert t
        )
  ;; :config
  ;; (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  )

  ;; keep the file indented
  (use-package aggressive-indent
    :hook ((clojure-mode . aggressive-indent-mode)
           (emacs-lisp-mode . aggressive-indent-mode)))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package md4rd
	:commands (md4rd))

(use-package xwwp
  :straight (xwwp :type git :host github :repo "canatella/xwwp")
	:commands (xwwp)
	:general
  (my/leader-keys
    "s w" '((lambda () (interactive)
							(let ((current-prefix-arg 4)) ;; emulate C-u universal arg
                (call-interactively 'xwwp)))
            :wk "search or visit")
    "s l" '(xwwp-follow-link :wk "link"))
  ;; :custom
  ;; (setq xwwp-follow-link-completion-system 'ivy)
  ;; :bind (:map xwidget-webkit-mode-map
  ;;             ("v" . xwwp-follow-link))
	)

(use-package dash-at-point
	:general
  (my/leader-keys
    "s d" '(dash-at-point :which-key "search dash"))
	)
