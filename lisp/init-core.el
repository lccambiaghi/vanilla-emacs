(setq straight-use-package-by-default t)
(setq straight-vc-git-default-clone-depth 1)
(setq straight-recipes-gnu-elpa-use-mirror t)
;; (setq straight-check-for-modifications '(check-on-save find-when-checking))
(setq straight-check-for-modifications nil)
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

  (setq read-process-output-max (* 1024 1024)) ;; 1mb

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

  (show-paren-mode t)

  ;; less noise when compiling elisp
  (setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

  ;; clean up the mode line
  (display-time-mode -1)
  (setq column-number-mode t)
  
  ;; use common convention for indentation by default
  (setq-default indent-tabs-mode t)
  (setq-default tab-width 2)
  )

(use-package emacs
  :init

  (defcustom lc/default-font-family "fira code" 
		"Default font family"
    :type 'string
    :group 'lc)

  (defcustom lc/variable-pitch-font-family  "cantarell"
		"Variable pitch font family"
    :type 'string
    :group 'lc)
	
  (defcustom lc/laptop-font-size 150
		"Font size used for laptop"
    :type 'int
    :group 'lc)

  (defcustom lc/theme nil
    "Current theme (light or dark)"
    :type 'symbol
    :options '(light dark)
    :group 'lc)
  )

(use-package emacs
	:init
	(defun lc/get-font-size ()
		"font size is calculated according to the size of the primary screen"
		(let* (;; (command "xrandr | awk '/primary/{print sqrt( ($(nf-2)/10)^2 + ($nf/10)^2 )/2.54}'")
					 (command "osascript -e 'tell application \"finder\" to get bounds of window of desktop' | cut -d',' -f3")
					 (screen-width (string-to-number (shell-command-to-string command))))  ;;<
      (if (> screen-width 2560) lc/laptop-font-size lc/laptop-font-size))) 
	
  ;; Main typeface
  (set-face-attribute 'default nil :font lc/default-font-family :height (lc/get-font-size))
  ;; Set the fixed pitch face
  (set-face-attribute 'fixed-pitch nil :font lc/default-font-family :height (lc/get-font-size))
  ;; Set the variable pitch face
  (set-face-attribute 'variable-pitch nil :font lc/variable-pitch-font-family :height (lc/get-font-size) :weight 'regular)
	)

(use-package emacs
	:init
	(defun lc/adjust-font-size (height)
		"Adjust font size by given height. If height is '0', reset font
size. This function also handles icons and modeline font sizes."
		(interactive "nHeight ('0' to reset): ")
		(let ((new-height (if (zerop height)
													(lc/get-font-size)
												(+ height (face-attribute 'default :height)))))
			(set-face-attribute 'default nil :height new-height)
			(set-face-attribute 'fixed-pitch nil :height new-height)
			(set-face-attribute 'variable-pitch nil :height new-height)
			(set-face-attribute 'mode-line nil :height new-height)
			(set-face-attribute 'mode-line-inactive nil :height new-height)
			(message "Font size: %s" new-height)))

	(defun lc/increase-font-size ()
		"Increase font size by 0.5 (5 in height)."
		(interactive)
		(lc/adjust-font-size 5))

	(defun lc/decrease-font-size ()
		"Decrease font size by 0.5 (5 in height)."
		(interactive)
		(lc/adjust-font-size -5))

	(defun lc/reset-font-size ()
		"Reset font size according to the `lc/default-font-size'."
		(interactive)
		(lc/adjust-font-size 0))
	)

(use-package emacs
  :init
  (when (eq system-type 'darwin)
    (setq mac-command-modifier 'super)     ; command as super
    (setq mac-option-modifier 'meta)     ; alt as meta
    (setq mac-control-modifier 'control)
    (mac-auto-operator-composition-mode)  ;; enables font ligatures
    (global-set-key [(s c)] 'kill-ring-save)
    (global-set-key [(s v)] 'yank)
    (global-set-key [(s x)] 'kill-region)
    (global-set-key [(s q)] 'kill-emacs)
    ))

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
  ;; :if (memq window-system '(mac ns))
  :if (eq system-type 'darwin)
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
  :hook
  ((org-jupyter-mode . (lambda () (lc/set-local-electric-pairs '())))
   (org-mode . (lambda () (lc/set-local-electric-pairs '((?= . ?=) (?~ . ?~))))))
  :init
  ;; auto-close parentheses
  (electric-pair-mode +1)
  (setq electric-pair-preserve-balance nil)
  ;; mode-specific local-electric pairs
  (defconst lc/default-electric-pairs electric-pair-pairs)
  (defun lc/set-local-electric-pairs (pairs)
    "Example usage: 
    (add-hook 'jupyter-org-interaction-mode '(lambda () (set-local-electric-pairs '())))
    "
    (setq-local electric-pair-pairs (append lc/default-electric-pairs pairs))
    (setq-local electric-pair-text-pairs electric-pair-pairs))

  ;; disable auto pairing for <  >
  (add-function :before-until electric-pair-inhibit-predicate
                (lambda (c) (eq c ?<))))  ;; >

(use-package general
  :demand t
  :config
  (general-evil-setup)

  (general-create-definer lc/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  (general-create-definer lc/local-leader-keys
    :states '(normal visual)
    :keymaps 'override
    :prefix ","
    :global-prefix "SPC m")

  (lc/leader-keys
    "SPC" '(execute-extended-command :which-key "execute command")
    "`" '((lambda () (interactive) (switch-to-buffer (other-buffer (current-buffer) 1))) :which-key "prev buffer")
    
    ";" '(eval-expression :which-key "eval sexp")

    "b" '(:ignore t :which-key "buffer")
    "br"  'revert-buffer
    ;; "bs" '((lambda () (interactive)
    ;;          (pop-to-buffer "*scratch*"))
    ;;        :wk "scratch")
    "bd"  'kill-current-buffer

    "c" '(:ignore t :which-key "code")

    "f" '(:ignore t :which-key "file")
    "fD" '((lambda () (interactive) (delete-file (buffer-file-name))) :wk "delete")
    "ff"  'find-file
    "fs" 'save-buffer
    "fr" 'recentf-open-files
    "fR" '((lambda (new-path)
             (interactive (list (read-file-name "Move file to: ") current-prefix-arg))
             (rename-file (buffer-file-name) (expand-file-name new-path)))
           :wk "move/rename")

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

    "k" '(:ignore t :which-key "kubernetes")
    ;; keybindings defined in kubernetes.el

    "o" '(:ignore t :which-key "org")
    ;; keybindings defined in org-mode

    ;; "p" '(:ignore t :which-key "project")
    ;; keybindings defined in projectile

    "s" '(:ignore t :which-key "search")
    ;; keybindings defined in consult

    "t"  '(:ignore t :which-key "toggle")
    "t d"  '(toggle-debug-on-error :which-key "debug on error")
    "t l" '(display-line-numbers-mode :wk "line numbers")
    "t w" '((lambda () (interactive) (toggle-truncate-lines)) :wk "word wrap")
    "t +"	'(lc/increase-font-size :wk "+ font")
    "t -"	'(lc/decrease-font-size :wk "- font")
    "t 0"	'(lc/reset-font-size :wk "reset font")

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
    "wm"  '(delete-other-windows :wk "maximize")

    "x" '(:ignore t :which-key "browser")
    ;; keybindings defined in xwwp
    )

  (lc/local-leader-keys
    :states 'normal
    "d" '(:ignore t :which-key "debug")
    "e" '(:ignore t :which-key "eval")
    "t" '(:ignore t :which-key "test")))

(use-package evil
  :demand
  :general
  (lc/leader-keys
    "wv" 'evil-window-vsplit
    "ws" 'evil-window-split)
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-fu)
	(setq evil-search-module 'evil-search)  ;; enables gn
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
  (defun lc/evil-dont-move-cursor (orig-fn &rest args)
    (save-excursion (apply orig-fn args)))
  (advice-add 'evil-indent :around #'lc/evil-dont-move-cursor)
  )

(use-package evil-collection
  :after evil
  :demand
	:init
	(setq evil-collection-magit-use-z-for-folds nil)
  :config
  (evil-collection-init))

(use-package evil
  :config
  (defcustom evil-extra-operator-eval-modes-alist
    '(;; (emacs-lisp eval-region)
      ;; (scheme-mode geiser-eval-region)
      (clojure-mode cider-eval-region)
			(jupyter-python jupyter-eval-region) ;; when executing in src block
      (python-mode jupyter-eval-region) ;; when executing in org-src-edit mode
      )
    "Alist used to determine evil-operator-eval's behaviour.
Each element of this alist should be of this form:
 (MAJOR-MODE EVAL-FUNC [ARGS...])
MAJOR-MODE denotes the major mode of buffer. EVAL-FUNC should be a function
with at least 2 arguments: the region beginning and the region end. ARGS will
be passed to EVAL-FUNC as its rest arguments"
    :type '(alist :key-type symbol)
    :group 'evil-extra-operator)

  (evil-define-operator evil-operator-eval (beg end)
    "Evil operator for evaluating code."
    :move-point nil
    (interactive "<r>")
    (let* ((mode (if (org-in-src-block-p) (intern (car (org-babel-get-src-block-info))) major-mode))
					 (ele (assoc mode evil-extra-operator-eval-modes-alist))
           (f-a (cdr-safe ele))
           (func (car-safe f-a))
           (args (cdr-safe f-a)))
      (if (fboundp func)
          (apply func beg end args)
        (eval-region beg end t))))
	
  (define-key evil-motion-state-map "gr" 'evil-operator-eval)
  
  )

(use-package evil-goggles
  :after evil
  :demand
  :init
  (setq evil-goggles-duration 0.05)
  :config
  (push '(evil-operator-eval
          :face evil-goggles-yank-face
          :switch evil-goggles-enable-yank
          :advice evil-goggles--generic-async-advice)
        evil-goggles--commands)
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces)
  )

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

(use-package evil-iedit-state
  :general
  (lc/leader-keys
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
	(global-evil-mc-mode 1)
	)

(use-package evil
  :init
  (defun lc/evil-posn-x-y (position)
    (let ((xy (posn-x-y position)))
      (when header-line-format
        (setcdr xy (+ (cdr xy)
                      (or (and (fboundp 'window-header-line-height)
                               (window-header-line-height))
                          evil-cached-header-line-height
                          (setq evil-cached-header-line-height (evil-header-line-height))))))
      (when (fboundp 'window-tab-line-height)
        (setcdr xy (+ (cdr xy) (window-tab-line-height))))
      xy))
  :config
  (advice-add 'evil-posn-x-y :override #'lc/evil-posn-x-y)
  )

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
         (org-mode . variable-pitch-mode))
  :general
  (lc/leader-keys
    "f t" '(org-babel-tangle :wk "tangle")
    "o C" '(org-capture :wk "capture")
    "o l" '(org-todo-list :wk "todo list")
    "o c" '((lambda () (interactive)
              (persp-switch "main")
              (find-file (concat user-emacs-directory "readme.org")))
            :wk "open config")
    "o t" '((lambda () (interactive)
              (persp-switch "main")
              (find-file (concat org-directory "/personal/todo.org")))
            :wk "open todos"))
  (lc/local-leader-keys
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
  :init
  ;; general settings
  (setq org-directory "~/Dropbox/org"
        +org-export-directory "~/Dropbox/org/export"
        org-default-notes-file "~/Dropbox/org/personal/todo.org"
        org-id-locations-file "~/Dropbox/org/.orgids"
        ;; org-export-in-background t
        org-src-preserve-indentation t ;; do not put two spaces on the left
        org-startup-indented t
        ;; org-startup-with-inline-images t
        org-hide-emphasis-markers t
        org-catch-invisible-edits 'smart)
  (setq org-image-actual-width nil)
  (setq org-indent-indentation-per-level 1)
  (setq org-list-demote-modify-bullet '(("-" . "+") ("+" . "*")))
  ;; disable modules for faster startup
  (setq org-modules
        '(ol-docview
          org-habit))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "PROG(p)" "|" "HOLD(h)" "DONE(d)")))
  (setq-default prettify-symbols-alist '(("#+BEGIN_SRC" . "»")
                                         ("#+END_SRC" . "«")
                                         ("#+begin_src" . "»")
                                         ("#+end_src" . "«")
                                         ("lambda"  . "λ")
                                         ("->" . "→")
                                         ("->>" . "↠")))
  (setq prettify-symbols-unprettify-at-point 'right-edge)
  :config
  ;; (efs/org-font-setup)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("clj" . "src clojure"))
  (add-to-list 'org-structure-template-alist '("jp" . "src jupyter-python"))
  (add-to-list 'org-structure-template-alist '("jr" . "src jupyter-R"))
  ;; fontification
  (add-to-list 'org-src-lang-modes '("jupyter-python" . python))
  (add-to-list 'org-src-lang-modes '("jupyter-R" . R))
  ;; latex
  ;; (setq org-latex-compiler "xelatex")
  ;; see https://www.reddit.com/r/emacs/comments/l45528/questions_about_mving_from_standard_latex_to_org/gkp4f96/?utm_source=reddit&utm_medium=web2x&context=3
  (setq org-latex-pdf-process '("TEXINPUTS=:$HOME/git/AltaCV//: tectonic %f"))
  (add-to-list 'org-export-backends 'beamer)
  (plist-put org-format-latex-options :scale 1.5)
  )

(use-package org
  :general
  (lc/leader-keys
    "f t" '(org-babel-tangle :wk "tangle")
    "o a" '(org-agenda-list :wk "agenda")
    "o A" '(org-agenda :wk "agenda")
    "o C" '(org-capture :wk "capture")
    "o l" '(org-todo-list :wk "todo list")
    "o c" '((lambda () (interactive)
              (find-file (concat user-emacs-directory "readme.org")))
            :wk "open config")
    "o n" '((lambda () (interactive) (org-agenda nil "n")) :wk "next")
    "o t" '((lambda () (interactive)
              (find-file (concat org-directory "/personal/todo.org")))
            :wk "open todos"))
  :init
  (setq org-agenda-files '("~/dropbox/org/personal/birthdays.org"
                           "~/dropbox/org/personal/todo.org" "~/dropbox/Notes/Test.inbox.org"))
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
  )

(use-package org
  :config
  ;; temporary hack until straight.el supports building org properly
  (defun org-git-version ()
    "The Git version of org-mode.
  Inserted by installing org-mode or when a release is made."
    ;; (require 'git)
    ;; (let ((git-repo (expand-file-name
    ;;                  "straight/repos/org/" user-emacs-directory)))
    ;;   (string-trim
    ;;    (git-run "describe"
    ;;             "--match=release\*"
    ;;             "--abbrev=6"
    ;;             "HEAD")))
		"9.2.4")

  (defun org-release ()
    "The release version of org-mode.
  Inserted by installing org-mode or when a release is made."
    ;; (require 'git)
    ;; (let ((git-repo (expand-file-name
    ;;                  "straight/repos/org/" user-emacs-directory)))
    ;;   (string-trim
    ;;    (string-remove-prefix
    ;;     "release_"
    ;;     (git-run "describe"
    ;;              "--match=release\*"
    ;;              "--abbrev=0"
    ;;              "HEAD"))))
		"9.2.4"
		)

  ;; (provide 'org-version)
  )

(use-package org
:init
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
	)

(use-package org
	:init
	(defun lc/org-toc ()
		(interactive)
		(let ((headings (delq nil (cl-loop for f in (f-entries "." (lambda (f) (f-ext? f "org")) t)
																			 append
																			 (with-current-buffer (find-file-noselect f)
																				 (org-map-entries
																					(lambda ()                 ;; <
																						(when (> 2 (car (org-heading-components)))
																							(cons f (nth 4 (org-heading-components)))))))))))
			(switch-to-buffer (get-buffer-create "*toc*"))
			(erase-buffer)
			(org-mode)
			(cl-loop for (file . file-headings) in (seq-group-by #'car headings) 
							 do
							 (insert (format "* [[%s][%s]] \n" file (file-relative-name file)))
							 (cl-loop for (file . heading) in file-headings 
												do
												(insert (format "** [[%s::*%s][%s]]\n" file heading heading))))))
	)

(use-package org
	:init
	(defun +org-cycle-only-current-subtree-h (&optional arg)
    "Toggle the local fold at the point, and no deeper.
`org-cycle's standard behavior is to cycle between three levels: collapsed,
subtree and whole document. This is slow, especially in larger org buffer. Most
of the time I just want to peek into the current subtree -- at most, expand
*only* the current subtree.

All my (performant) foldings needs are met between this and `org-show-subtree'
(on zO for evil users), and `org-cycle' on shift-TAB if I need it."
		(interactive "P")
		(unless (eq this-command 'org-shifttab)
			(save-excursion
				(org-beginning-of-line)
				(let (invisible-p)
					(when (and (org-at-heading-p)
										 (or org-cycle-open-archived-trees
												 (not (member org-archive-tag (org-get-tags))))
										 (or (not arg)
												 (setq invisible-p (outline-invisible-p (line-end-position)))))
						(unless invisible-p
              (setq org-cycle-subtree-status 'subtree))
            (org-cycle-internal-local)
            t)))))
  :config
  ;; Only fold the current tree, rather than recursively
  (add-hook 'org-tab-first-hook #'+org-cycle-only-current-subtree-h)
  )

(defun lc/async-process (command &optional name filter)
  "Start an async process by running the COMMAND string with bash. Return the
process object for it.

NAME is name for the process. Default is \"async-process\".

FILTER is function that runs after the process is finished, its args should be
\"(process output)\". Default is just messages the output."
  (make-process
   :command `("bash" "-c" ,command)
   :name (if name name
           "async-process")
   :filter (if filter filter
             (lambda (process output) (message (s-trim output))))))

(defun lc/tangle-config ()
  "Export code blocks from the literate config file
asynchronously."
  (interactive)
  ;; prevent emacs from killing until tangle-process finished
  (add-to-list 'kill-emacs-query-functions
               (lambda ()
                 (or (not (process-live-p (get-process "tangle-process")))
                     (y-or-n-p "\"fk/tangle-config\" is running; kill it? "))))
  ;; tangle config asynchronously
  (lc/async-process
   (format "/Applications/Emacs.app/Contents/MacOS/Emacs %s --batch --eval '(org-babel-tangle nil \"%s\")'"
           (expand-file-name "readme.org" user-emacs-directory)
           (expand-file-name "init.el" user-emacs-directory))
   "tangle-process"))

(use-package org-reverse-datetree
  :after org :demand)

(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  :init
  (setq org-superstar-headline-bullets-list '("✖" "✚" "◉" "○" "▶")
        ;; org-superstar-special-todo-items t
        org-ellipsis " ↴ ")
  )

(use-package hl-todo
	:hook ((prog-mode org-mode) . lc/hl-todo-init)
	:init
	(defun lc/hl-todo-init ()
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
  (lc/local-leader-keys
    :keymaps 'org-mode-map
    "'" '(org-edit-special :wk "edit")
    "-" '(org-babel-demarcate-block :wk "split block")
    "z" '(org-babel-hide-result-toggle :wk "fold result"))
  (lc/local-leader-keys
    :keymaps 'org-src-mode-map
    "'" '(org-edit-src-exit :wk "exit")) ;;FIXME
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
  (lc/local-leader-keys
    :keymaps 'org-mode-map
    "=" '((lambda () (interactive) (jupyter-org-insert-src-block t nil)) :wk "block below")
    "m" '(jupyter-org-merge-blocks :wk "merge")
    "+" '(jupyter-org-insert-src-block :wk "block above")
    "?" '(jupyter-inspect-at-point :wk "inspect")
    "x" '(jupyter-org-kill-block-and-results :wk "kill block"))
  :hook ((jupyter-org-interaction-mode . (lambda () (lc/set-local-electric-pairs '((?' . ?')))))
         (jupyter-repl-persistent-mode . (lambda ()  ;; we activate org-interaction-mode ourselves
                                           (when (derived-mode-p 'org-mode)
                                             ;; (setq-local company-backends '((company-capf)))
																						 (setq-local evil-lookup-func #'jupyter-inspect-at-point)
                                             (jupyter-org-interaction-mode))))
				 (envrc-mode . lc/load-ob-jupyter))
  :init
  (setq org-babel-default-header-args:jupyter-python '((:async . "yes")
                                                       (:pandoc t)
                                                       (:kernel . "python3")))
  (setq org-babel-default-header-args:jupyter-R '((:pandoc t)
                                                  (:async . "yes")
                                                  (:kernel . "ir")))
	(defun lc/org-load-jupyter ()
    (org-babel-do-load-languages 'org-babel-load-languages
                                 (append org-babel-load-languages
                                         '((jupyter . t)))))
  (defun lc/load-ob-jupyter ()
    ;; only try to load in org-mode
    (when (derived-mode-p 'org-mode)
      ;; skip if already loaded
      (unless (member '(jupyter . t) org-babel-load-languages)
        ;; only load if jupyter is available
        (when (executable-find "jupyter")
					(lc/org-load-jupyter)))))
	
  (cl-defmethod jupyter-org--insert-result (_req context result)
    (let ((str
           (org-element-interpret-data
            (jupyter-org--wrap-result-maybe
             context (if (jupyter-org--stream-result-p result)
                         (thread-last result
                           jupyter-org-strip-last-newline
                           jupyter-org-scalar)
                       result)))))
      (if (< (length str) 100000)  ;; >
          (insert str)
        (insert (format ": Result was too long! Length was %d" (length str)))))
    (when (/= (point) (line-beginning-position))
      ;; Org objects such as file links do not have a newline added when
      ;; converting to their string representation by
      ;; `org-element-interpret-data' so insert one in these cases.
      (insert "\n")))
  ;; :config
  ;;Remove text/html since it's not human readable
  ;; (delete :text/html jupyter-org-mime-types)
  ;; (with-eval-after-load 'org-src
  ;;   (add-to-list 'org-src-lang-modes '("jupyter-python" . python))
  ;;   (add-to-list 'org-src-lang-modes '("jupyter-R" . R)))
	)

(use-package org-tree-slide
	:after org
	:hook ((org-tree-slide-play . (lambda () (+remap-faces-at-start-present)))
				 (org-tree-slide-stop . (lambda () (+remap-faces-at-stop-present))))
	:general
	(lc/leader-keys
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
  :bind
  ([remap evil-org-org-insert-heading-respect-content-below] . +org/insert-item-below) ;; "<C-return>" 
  ([remap evil-org-org-insert-todo-heading-respect-content-below] . +org/insert-item-above) ;; "<C-S-return>" 
  :general
  (general-nmap
    :keymaps 'org-mode-map :states 'normal
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
	:after modus-themes
  :straight
  (org-html-themify
   :type git
   :host github
   :repo "DogLooksGood/org-html-themify"
   :files ("*.el" "*.js" "*.css"))
  :hook (org-mode . org-html-themify-mode)
  :config
  ;; otherwise it complains about invalid face
  (require 'hl-line)
  )

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

(use-package org-appear
  :straight (org-appear :type git :host github :repo "awth13/org-appear")
	:hook (org-mode . org-appear-mode)
  :init
  (setq org-appear-autoemphasis  t)
  (setq org-appear-autolinks t)
  (setq org-appear-autosubmarkers t)
	)

(use-package org-fragtog
	:hook (org-mode . org-fragtog-mode))

(use-package org
  :config
  (require 'org-crypt)
  (require 'epa-file)
  (epa-file-enable)
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt")))
  (setq org-crypt-key nil)
  (defun ag/reveal-and-move-back ()
    (org-reveal)
    (goto-char ag/old-point))
  (defun ag/org-reveal-after-save-on ()
    (setq ag/old-point (point))
    (add-hook 'after-save-hook 'ag/reveal-and-move-back))
  (defun ag/org-reveal-after-save-off ()
    (remove-hook 'after-save-hook 'ag/reveal-and-move-back))
  (add-hook 'org-babel-pre-tangle-hook 'ag/org-reveal-after-save-on)
  (add-hook 'org-babel-post-tangle-hook 'ag/org-reveal-after-save-off)
  )

(use-package org
  :init
  (defun lc/org-custom-id-get (&optional pom create prefix)
    "Get the CUSTOM_ID property of the entry at point-or-marker POM.
   If POM is nil, refer to the entry at point. If the entry does
   not have an CUSTOM_ID, the function returns nil. However, when
   CREATE is non nil, create a CUSTOM_ID if none is present
   already. PREFIX will be passed through to `org-id-new'. In any
   case, the CUSTOM_ID of the entry is returned."
    (interactive)
    (org-with-point-at pom
      (let ((id (org-entry-get nil "CUSTOM_ID")))
        (cond
         ((and id (stringp id) (string-match "\\S-" id))
          id)
         (create
          (setq id (org-id-new (concat prefix "h")))
          (org-entry-put pom "CUSTOM_ID" id)
          (org-id-add-location id (buffer-file-name (buffer-base-buffer)))
          id)))))
  
  (defun lc/org-add-ids-to-headlines-in-file ()
    "Add CUSTOM_ID properties to all headlines in the current
   file which do not already have one. Only adds ids if the
   `auto-id' option is set to `t' in the file somewhere. ie,
   #+OPTIONS: auto-id:t"
    (interactive)
    (save-excursion
      (widen)
      (goto-char (point-min))
      (when (re-search-forward "^#\\+OPTIONS:.*auto-id:t" 10000 t)
        (org-map-entries (lambda () (lc/org-custom-id-get (point) 'create))))))
  :config
  (require 'org-id)
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  )

(use-package emacs
  :hook
  ((org-jupyter-mode . (lambda () (visual-line-mode -1)))
   (org-mode . (lambda () (when (lc/is-jupyter-org-buffer?) (org-jupyter-mode)))))
  :general
  (lc/local-leader-keys
    :states 'normal
    "k i" '(jupyter-org-interrupt-kernel :wk "interrupt")
    "k r" '(jupyter-repl-restart-kernel :wk "restart"))
  :init
  (defun lc/is-jupyter-org-buffer? ()
    (with-current-buffer (buffer-name)
      (goto-char (point-min))
      (re-search-forward "begin_src jupyter-" 10000 t)))
  
  (define-minor-mode org-jupyter-mode
    "Minor mode which is active when an org file has the string begin_src jupyter-python
    in the first few hundred rows"
    ;; :keymap (let ((map (make-sparse-keymap)))
    ;;             (define-key map (kbd "C-c f") 'insert-foo)
    ;;             map)
    )
  )

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
  :demand
  :hook (modus-themes-after-load-theme . lc/fix-fill-column-indicator)
  :general
  (lc/leader-keys
    "t t" '((lambda () (interactive) (modus-themes-toggle)) :wk "toggle theme"))
  :init
  (setq modus-themes-slanted-constructs t
        ;; modus-themes-no-mixed-fonts t
        modus-themes-bold-constructs t
        modus-themes-fringes 'nil ; {nil,'subtle,'intense}
        modus-themes-mode-line '3d ; {nil,'3d,'moody}
        modus-themes-intense-hl-line nil
        modus-themes-prompts nil ; {nil,'subtle,'intense}
        modus-themes-completions 'moderate ; {nil,'moderate,'opinionated}
        modus-themes-diffs nil ; {nil,'desaturated,'fg-only}
        modus-themes-org-blocks 'greyscale ; {nil,'greyscale,'rainbow}
        modus-themes-headings  ; Read further below in the manual for this one
        '((1 . line-no-bold)
          (t . rainbow-line-no-bold))
        modus-themes-variable-pitch-headings t
        modus-themes-scale-headings t
        modus-themes-scale-1 1.1
        modus-themes-scale-2 1.15
        modus-themes-scale-3 1.21
        modus-themes-scale-4 1.27
        modus-themes-scale-5 1.33)	
  (defun lc/override-colors ()
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
    )
  (defun lc/load-dark-theme ()
    (setq lc/theme 'dark)
    (with-eval-after-load 'org (plist-put org-format-latex-options :foreground "whitesmoke"))
    (with-eval-after-load 'org-html-themify
      (setq org-html-themify-themes '((light . modus-vivendi) (dark . modus-vivendi))))
    (modus-themes-load-vivendi))
  (defun lc/load-light-theme ()
    (setq lc/theme 'light)
    (with-eval-after-load 'org (plist-put org-format-latex-options :foreground "dark"))
    (with-eval-after-load 'org-html-themify
      (setq org-html-themify-themes '((light . modus-operandi) (dark . modus-operandi))))
    (setenv "BAT_THEME" "ansi")
    (modus-themes-load-operandi))
  (defun lc/change-theme-with-mac-system ()
    (let ((appearance (plist-get (mac-application-state) :appearance)))
      (cond ((equal appearance "NSAppearanceNameAqua")
             (lc/load-light-theme))
            ((equal appearance "NSAppearanceNameDarkAqua")
             (lc/load-dark-theme)))))
  (defun lc/change-theme-with-timers ()
    (run-at-time "00:00" (* 60 60 24) 'lc/load-dark-theme)
    (run-at-time "08:00" (* 60 60 24) 'lc/load-light-theme)
    (run-at-time "18:00" (* 60 60 24) 'lc/load-dark-theme))
  (defun lc/fix-fill-column-indicator ()
    (when (display-graphic-p)
      (modus-themes-with-colors
        (custom-set-faces
         `(fill-column-indicator ((,class :background ,bg-inactive :foreground ,bg-inactive)))))))
  :config
  (when (display-graphic-p)
    (lc/override-colors))
  (if (and (boundp 'mac-effective-appearance-change-hook)
           (plist-get (mac-application-state) :appearance))
      (progn
        (add-hook 'after-init-hook 'lc/change-theme-with-mac-system)
        (add-hook 'mac-effective-appearance-change-hook 'lc/change-theme-with-mac-system))
    ;; (add-hook 'after-init-hook 'lc/change-theme-with-timers)
    ;; (add-hook 'emacs-startup-hook 'lc/load-light-theme)
    (add-hook 'emacs-startup-hook 'lc/change-theme-with-timers)
    )
  )

(use-package dashboard
  :demand
  :init
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (setq dashboard-center-content t)
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (defun lc/is-after-17-or-weekends? ()
    (or (thread-first (nth 4 (split-string (current-time-string) " ")) ;; time of the day e.g. 18
            (substring 0 2)
            (string-to-number)   ;;<
            (> 16))
        (thread-first (substring (current-time-string) 0 3) ;; day of the week e.g. Fri
            (member  '("Sat" "Sun")))))
  (setq dashboard-banner-logo-title nil)
  (setq dashboard-set-footer nil)
  ;; (setq dashboard-startup-banner [VALUE])
  (setq dashboard-set-navigator t)
  (setq dashboard-navigator-buttons
        `((;; Github
           (,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
            "Github"
            "Go to wondercast"
            (lambda (&rest _) (browse-url "https://github.com/Maersk-Global/wondercast")))
           ;; Codebase
           (,(all-the-icons-faicon "briefcase" :height 1.1 :v-adjust -0.1)
            "JIRA"
            "Go to Kanban"
            (lambda (&rest _) (browse-url "https://jira.maerskdev.net/secure/RapidBoard.jspa?rapidView=6378&projectKey=AVOC&quickFilter=15697")))
           ;; Perspectives
           (,(all-the-icons-octicon "history" :height 1.1 :v-adjust 0.0)
            "Restore"
            "Restore"
            (lambda (&rest _) (persp-state-load persp-state-default-file)))
           )))
  (defun lc/dashboard-agenda-entry-format ()
    "Format agenda entry to show it on dashboard. Compared to the original, we remove tags at the end"
    (let* ((schedule-time (org-get-scheduled-time (point)))
           (deadline-time (org-get-deadline-time (point)))
           (item (org-agenda-format-item
                  (dashboard-agenda-entry-time (or schedule-time deadline-time))
                  (org-get-heading)
                  (org-outline-level)
                  (org-get-category)
                  nil;; (org-get-tags)
                  t))
           (loc (point))
           (file (buffer-file-name)))
      (dashboard-agenda--set-agenda-headline-face item)
      (list item loc file)))
  (defun lc/dashboard-get-agenda ()
    "Get agenda items for today or for a week from now."
    (org-compile-prefix-format 'agenda)
    (org-map-entries 'lc/dashboard-agenda-entry-format
                     dashboard-match-agenda-entry
                     'agenda
                     dashboard-filter-agenda-entry))
  (defun lc/dashboard-get-next ()
    "Get agenda items for today or for a week from now."
    (org-compile-prefix-format 'agenda)
    (org-map-entries 'lc/dashboard-agenda-entry-format
                     dashboard-match-next-entry
                     'agenda))
  (defun lc/dashboard-insert-next (list-size)
    "Add the list of LIST-SIZE items of next tasks"
    (require 'org-agenda)
    (let ((next (lc/dashboard-get-next)))
      (dashboard-insert-section
       "Next tasks"
       next
       list-size
       "n"
       `(lambda (&rest ignore)
          (let ((buffer (find-file-other-window (nth 2 ',el))))
            (with-current-buffer buffer
              (goto-char (nth 1 ',el))
              (switch-to-buffer buffer))))
       (format "%s" (nth 0 el)))))
  :config
  ;; exclude work items after 17 and on weekends
  (setq dashboard-match-next-entry "TODO=\"NEXT\"-work")
  (run-at-time "00:00" (* 60 60 24)
               (lambda ()
                 (if (lc/is-after-17-or-weekends?)
                     (setq dashboard-match-agenda-entry "life|habits"
                           dashboard-match-next-entry "TODO=\"NEXT\"-work")
                   (setq dashboard-match-agenda-entry "work|life|habits"
                         dashboard-match-next-entry "TODO=\"NEXT\""
                         ))))
  (dashboard-setup-startup-hook)
  (set-face-attribute 'dashboard-items-face nil :height (lc/get-font-size))
  ;; do not show tags in agenda view
  (advice-add 'dashboard-get-agenda :override #'lc/dashboard-get-agenda)
  ;; show next tasks in dashboard
  (add-to-list 'dashboard-item-generators  '(next . lc/dashboard-insert-next))
  (setq dashboard-items '((agenda . 5)
                          (next . 10)
                          ;; (bookmarks . 5)
                          ;; (recents  . 5)
                          (projects . 5)))
  )

(use-package centaur-tabs
  :hook (emacs-startup . centaur-tabs-mode)
  :general
  (general-nmap "gt" 'centaur-tabs-forward
    "gT" 'centaur-tabs-backward)
	(lc/leader-keys
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
  :general
	(lc/leader-keys
		"t =" '((lambda () (interactive) (centered-cursor-mode 'toggle)) :wk "center cursor")
		)
	)

(use-package hide-mode-line
  :commands (hide-mode-line-mode))

(setq display-buffer-alist
      `((,(rx bos (or "*Apropos*" "*Help*" "*helpful" "*info*" "*Summary*") (0+ not-newline))
         (display-buffer-reuse-mode-window display-buffer-below-selected)
         (window-height . 0.33)
         (mode apropos-mode help-mode helpful-mode Info-mode Man-mode))))

(use-package winum
:general
(lc/leader-keys
"1" '(winum-select-window-1 :wk "win 1")
"2" '(winum-select-window-2 :wk "win 2")
"3" '(winum-select-window-3 :wk "win 3"))
:config
(winum-mode))

(use-package transpose-frame
  :general
  (lc/leader-keys
    "w t" '(transpose-frame :wk "transpose")
    "w f" '(rotate-frame :wk "flip")))

(use-package persistent-scratch
  :hook
  (org-mode . (lambda ()
                "only set initial-major-mode after loading org"
                (setq initial-major-mode 'org-mode)))
  :general
  (lc/leader-keys
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
  (lc/leader-keys
    "t o" '(olivetti-mode :wk "olivetti"))
  :init
  (setq olivetti-body-width 100)
  (setq olivetti-recall-visual-line-mode-entry-state t))

(use-package display-fill-column-indicator
  :straight (:type built-in)
  :hook
  (prog-mode . display-fill-column-indicator-mode)
  :init
  (setq-default fill-column  90)
  ;; (setq display-fill-column-indicator-character "|")
	)

(use-package emacs
  :hook
  ((org-jupyter-mode . (lambda () (whitespace-mode -1)))
   (org-mode . whitespace-mode))
  :init
  (setq-default
   whitespace-line-column 90
   whitespace-style       '(face lines-tail)))

;; add a visual intent guide
(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :init
  ;; (setq highlight-indent-guides-method 'column)
  (setq highlight-indent-guides-method 'character)
  ;; (setq highlight-indent-guides-character ?|)
  ;; (setq highlight-indent-guides-character ?❚)
  (setq highlight-indent-guides-character ?‖)
  ;; (setq highlight-indent-guides-responsive 'stack)
  (setq highlight-indent-guides-responsive 'top)
	;; (setq highlight-indent-guides-auto-enabled nil)
	;; (set-face-background 'highlight-indent-guides-odd-face "darkgray")
  ;; (set-face-background 'highlight-indent-guides-even-face "dimgray")
  ;; (set-face-foreground 'highlight-indent-guides-character-face "dimgray")
  )

(use-package emacs
	:general
  (lc/leader-keys
    "w o" '(doom/window-enlargen :wk "enlargen"))
	:init
	(defun doom/window-enlargen (&optional arg)
		"Enlargen the current window to focus on this one. Does not close other
windows (unlike `doom/window-maximize-buffer'). Activate again to undo."
		(interactive "P")
		(let ((param 'doom--enlargen-last-wconf))
			(cl-destructuring-bind (window . wconf)
					(or (frame-parameter nil param)
							(cons nil nil))
				(set-frame-parameter
				 nil param
				 (if (and (equal window (selected-window))
									(not arg)
									wconf)
						 (ignore
							(let ((source-window (selected-window)))
								(set-window-configuration wconf)
								(when (window-live-p source-window)
									(select-window source-window))))
					 (prog1 (cons (selected-window) (or wconf (current-window-configuration)))
						 (let* ((window (selected-window))
										(dedicated-p (window-dedicated-p window))
										(preserved-p (window-parameter window 'window-preserved-size))
										(ignore-window-parameters t)
										(window-resize-pixelwise nil)
										(frame-resize-pixelwise nil))
							 (unwind-protect
									 (progn
										 (when dedicated-p
											 (set-window-dedicated-p window nil))
										 (when preserved-p
											 (set-window-parameter window 'window-preserved-size nil))
										 (maximize-window window))
								 (set-window-dedicated-p window dedicated-p)
								 (when preserved-p
									 (set-window-parameter window 'window-preserved-size preserved-p))
								 (add-hook 'doom-switch-window-hook #'doom--enlargened-forget-last-wconf-h)))))))))
	)

(use-package alert
  :config
  (if (eq system-type 'darwin)
      (setq
       ;; alert-default-style 'notifier
       alert-default-style 'osx-notifier
       ))
  ;; (alert "This is an alert" :severity 'high)
  ;; (alert "This is an alert" :title "My Alert" :category 'debug)
  )

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
	:commands (consult-ripgrep)
	:general
	(general-nmap
		:states '(normal insert)
		"C-p" 'consult-yank-pop)
	(lc/leader-keys
		"s i" '(consult-isearch :wk "isearch")
		"s o" '(consult-outline :which-key "outline")
		"s s" 'consult-line
		"s p" '(consult-ripgrep :wk "ripgrep project")
		"b b" 'consult-buffer
		;; TODO consult mark
		"f r" 'consult-recent-file
		"s !" '(consult-flymake :wk "flymake"))
	;; (with-eval-after-load 'projectile
	;;   (lc/leader-keys
	;;     "s p" '((lambda () (interactive) (consult-ripgrep (projectile-project-root))) :wk "ripgrep")))
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
  (lc/leader-keys
		:states 'normal
    "p" '(:keymap projectile-command-map :which-key "project")
    "p <escape>" 'keyboard-escape-quit
    "p a" '(projectile-add-known-project :wk "add known")
    "p t" '(projectile-run-vterm :wk "term"))
  :init
  (when (file-directory-p "~/git")
    (setq projectile-project-search-path '("~/git")))
  (setq projectile-completion-system 'default)
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
    (consult-ripgrep))
	(def-projectile-commander-method ?g
    "Git status in project."
    (projectile-vc))
  )

(use-package perspective
  :commands (persp-new persp-switch persp-state-save)
  :general
  (lc/leader-keys
    "<tab>" '(:ignore true :wk "tab")
    "<tab> <tab>" 'persp-switch
    "<tab> `" 'persp-switch-last
    "<tab> d" 'persp-kill
    "<tab> x" '((lambda () (interactive) (persp-kill (persp-current-name))) :wk "kill current")
    "<tab> X" '((lambda () (interactive) (persp-kill (persp-names))) :wk "kill all")
    "<tab> m" '(lc/main-tab :wk "main"))
  :init
	(setq persp-state-default-file (expand-file-name ".persp" user-emacs-directory))
  (defun lc/main-tab ()
    "Jump to the dashboard buffer, if doesn't exists create one."
    (interactive)
    (persp-switch "main")
    (switch-to-buffer dashboard-buffer-name)
    (dashboard-mode)
    (dashboard-insert-startupify-lists)
    (dashboard-refresh-buffer))
  :config
  (persp-mode)
	(add-hook 'kill-emacs-hook #'persp-state-save))

(use-package persp-projectile
  :after projectile
  :general
  (lc/leader-keys
    "p p" 'projectile-persp-switch-project
    ;; "<tab> o"	'((lambda () (interactive)
    ;;               (let ((projectile-switch-project-action #'projectile-find-file))
    ;;                 (projectile-persp-switch-project "org")))
    ;;             :wk "org")
    )
  )

(use-package magit
  :general
  (lc/leader-keys
    "g b" 'magit-blame
    "g g" 'magit-status
    "g G" 'magit-status-here
    "g l" 'magit-log)
  (general-nmap
    :keymaps '(magit-status-mode-map
               magit-stash-mode-map
               magit-revision-mode-map
               magit-process-mode-map
               magit-diff-mode-map)
    "<tab>" #'magit-section-toggle
    "<escape>" #'transient-quit-one)
  :init
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (setq magit-log-arguments '("--graph" "--decorate" "--color"))
  (setq git-commit-fill-column 72)
	;; (setq magit-log-margin (t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))
  :config
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
	(lc/leader-keys
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
  (lc/leader-keys "g m" 'smerge-hydra/body)
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

(use-package git)

(use-package hydra
  :demand)

(use-package rainbow-delimiters
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
         (clojure-mode . rainbow-delimiters-mode))
	      )

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
  :hook (after-init . global-company-mode)
  ;; :hook
  ;; (python-mode . (lambda ()
  ;; 								(setq-local company-backends '((company-capf :with company-files)))))
  ;; :general
  ;; (general-nmap
  ;;   "<tab>" 'company-complete-common-or-cycle)
  :init
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.3)
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-maximum-width 50
        company-tooltip-minimum-width 50)
  (setq company-tooltip-limit 12)
  ;; don't autocomplete when single candidate
  (setq company-auto-complete nil)
  (setq company-auto-complete-chars nil)
  (setq company-dabbrev-code-other-buffers nil)
  (setq company-dabbrev-ignore-case nil)
  (setq company-dabbrev-downcase nil)
  ;; manually configure tng
  ;; (setq company-tng-auto-configure nil)
  ;; (setq company-frontends '(company-tng-frontend
  ;;                           company-pseudo-tooltip-frontend
  ;;                           company-echo-metadata-frontend))
  ;; (setq company-backends '((company-capf company-keywords company-files :with company-yasnippet)))
  (setq company-backends '((company-capf :with company-yasnippet)
                           (company-dabbrev-code company-keywords company-files)
                           company-dabbrev))
  ;; :custom-face
  ;; (company-tooltip
  ;;  ((t (:family "Fira Code"))))
  :config
  (global-company-mode)
  (with-eval-after-load 'evil
    (add-hook 'company-mode-hook #'evil-normalize-keymaps))
  ;; needed in case we only have one candidate
  (define-key company-active-map (kbd "C-j") 'company-select-next)
  ;; (define-key company-mode-map [remap indent-for-tab-command] #'company-indent-or-complete-common)
  )

(use-package company-box
  :if (display-graphic-p)
  :hook (company-mode . company-box-mode)
  :config
  (with-no-warnings
    ;; Prettify icons
    (defun my-company-box-icons--elisp (candidate)
      (when (derived-mode-p 'emacs-lisp-mode)
        (let ((sym (intern candidate)))
          (cond ((fboundp sym) 'Function)
                ((featurep sym) 'Module)
                ((facep sym) 'Color)
                ((boundp sym) 'Variable)
                ((symbolp sym) 'Text)
                (t . nil)))))
    (advice-add #'company-box-icons--elisp :override #'my-company-box-icons--elisp))
  
  (declare-function all-the-icons-faicon 'all-the-icons)
  (declare-function all-the-icons-material 'all-the-icons)
  (declare-function all-the-icons-octicon 'all-the-icons)

  (setq company-box-icons-all-the-icons
        `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.8 :v-adjust -0.15))
          (Text . ,(all-the-icons-faicon "text-width" :height 0.8 :v-adjust -0.02))
          (Method . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
          (Function . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
          (Constructor . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
          (Field . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
          (Variable . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
          (Class . ,(all-the-icons-material "settings_input_component" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
          (Interface . ,(all-the-icons-material "share" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
          (Module . ,(all-the-icons-material "view_module" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
          (Property . ,(all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.02))
          (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.8 :v-adjust -0.15))
          (Value . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
          (Enum . ,(all-the-icons-material "storage" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
          (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.8 :v-adjust -0.15))
          (Snippet . ,(all-the-icons-material "format_align_center" :height 0.8 :v-adjust -0.15))
          (Color . ,(all-the-icons-material "palette" :height 0.8 :v-adjust -0.15))
          (File . ,(all-the-icons-faicon "file-o" :height 0.8 :v-adjust -0.02))
          (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.8 :v-adjust -0.15))
          (Folder . ,(all-the-icons-faicon "folder-open" :height 0.8 :v-adjust -0.02))
          (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15))
          (Constant . ,(all-the-icons-faicon "square-o" :height 0.8 :v-adjust -0.1))
          (Struct . ,(all-the-icons-material "settings_input_component" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
          (Event . ,(all-the-icons-octicon "zap" :height 0.8 :v-adjust 0 :face 'all-the-icons-orange))
          (Operator . ,(all-the-icons-material "control_point" :height 0.8 :v-adjust -0.15))
          (TypeParameter . ,(all-the-icons-faicon "arrows" :height 0.8 :v-adjust -0.02))
          (Template . ,(all-the-icons-material "format_align_left" :height 0.8 :v-adjust -0.15)))
        company-box-icons-alist 'company-box-icons-all-the-icons)

  (setq company-box-show-single-candidate t
        company-box-backends-colors nil
        company-box-max-candidates 10)
  ;; Disable tab-bar in company-box child frames
  (add-to-list 'company-box-frame-parameters '(tab-bar-lines . 0))
  )

(use-package inheritenv
  :straight (inheritenv :type git :host github :repo "purcell/inheritenv"))

(use-package envrc
  :straight (envrc :type git :host github :repo "purcell/envrc")
  :commands (envrc-mode)
  :hook ((python-mode . envrc-mode)
         (org-jupyter-mode . envrc-mode))
  )

(use-package yasnippet
  :general
  (yas-minor-mode-map
   :states 'insert
   "<tab>" 'nil
   "C-<tab>" 'yas-expand)
  :hook
  ((prog-mode org-mode dap-ui-repl-mode vterm-mode) . yas-minor-mode)
  :init
  ;; (setq yas-prompt-functions '(yas-ido-prompt))
  (defun lc/yas-try-expanding-auto-snippets ()
    (when (and (boundp 'yas-minor-mode) yas-minor-mode)
      (let ((yas-buffer-local-condition ''(require-snippet-condition . auto)))
        (yas-expand))))
  :config
  (yas-reload-all)
  (add-hook 'post-command-hook #'lc/yas-try-expanding-auto-snippets)
  )

(use-package undo-fu
  :demand
  :general
  (:states 'normal
           "u" 'undo-fu-only-undo
           "\C-r" 'undo-fu-only-redo))

(when (eq system-type 'darwin)
  (use-package undo-fu
    :general
    (:states '(normal insert)
             "s-z" 'undo-fu-only-undo)))

(use-package vterm
  :config
  (setq vterm-shell (executable-find "fish")
        vterm-max-scrollback 10000))

(use-package vterm-toggle
  :general
  (lc/leader-keys
    "'" 'vterm-toggle))

(use-package dired
  :straight (:type built-in)
  :hook
  (dired-mode . dired-hide-details-mode)
  :general
  (lc/leader-keys
    "f d" 'dired
    "f j" 'dired-jump)
  (general-nmap
    :keymaps 'dired-mode-map
    :states 'normal
    "F" '((lambda () (interactive)
            (let ((fn (dired-get-file-for-visit)))
              (start-process "open-directory" nil "open" "-R" fn)))
          :wk "open finder")
    "X" '((lambda () (interactive)
            (let ((fn (dired-get-file-for-visit)))
              (start-process "open-external" nil "open" fn)))
          :wk "open external"))
  :init
  (setq dired-omit-files "^\\.[^.]\\|$Rhistory\\|$RData\\|__pycache__")
  (setq dired-listing-switches "-lah")
  (setq dired-dwim-target t)
  (defun my/dired-open-externally ()
    "Open marked dired file/folder(s) (or file/folder(s) at point if no marks)
  with external application"
    (interactive)
    (let ((files (dired-get-marked-files)))
      (dired-run-shell-command
       (dired-shell-stuff-it "xdg-open" files t))))
  )

(use-package dired-single
  :after dired
  :general
  (dired-mode-map
   :states 'normal
   "h" 'dired-single-up-directory
   "l" 'dired-single-buffer
   "q" 'kill-current-buffer))

(use-package all-the-icons-dired
  :hook (dired-mode . (lambda () (interactive)
                        (unless (file-remote-p default-directory)
                          (all-the-icons-dired-mode)))))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(use-package dired-subtree
  :general
  (dired-mode-map
   :states 'normal
   "i" 'dired-subtree-toggle)
  :config
  (advice-add 'dired-subtree-toggle
              :after (lambda () (interactive)
                       (when all-the-icons-dired-mode
                         (revert-buffer)))))

(use-package dired-rsync
  :general
  (lc/local-leader-keys
    :keymaps 'dired-mode-map
    :states 'normal
    "r" 'dired-rsync))

(use-package restart-emacs
  :general
  (lc/leader-keys
    "R" '(restart-emacs :wk "restart"))
  )

(use-package toml-mode
	:mode "\\.toml\\'")

(use-package tramp
  :straight (:type built-in)
  :init
  ;; Disable version control on tramp buffers to avoid freezes.
  (setq vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)"
                vc-ignore-dir-regexp
                tramp-file-name-regexp))
  (setq tramp-default-method "ssh")
  (setq tramp-auto-save-directory
        (expand-file-name "tramp-auto-save" user-emacs-directory))
  (setq tramp-persistency-file-name
        (expand-file-name "tramp-connection-history" user-emacs-directory))
  (setq password-cache-expiry nil)
  (setq tramp-use-ssh-controlmaster-options nil)
  :config
  (customize-set-variable 'tramp-ssh-controlmaster-options
                          (concat
                           "-o ControlPath=/tmp/ssh-tramp-%%r@%%h:%%p "
                           "-o ControlMaster=auto -o ControlPersist=yes"))
  (with-eval-after-load 'lsp-mode
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-tramp-connection "pyright")
                      :major-modes '(python-mode)
                      :remote? t
                      :server-id 'pyright-remote))
    )
  )

(use-package docker-tramp)

(use-package yaml-mode
  :mode ((rx ".yml" eos) . yaml-mode))

(use-package kubernetes
  :hook
  (kubernetes-overview-mode . lc/load-kubernetes-evil)
  :general
  (lc/leader-keys
    "k k" 'kubernetes-overview)
  :init
  ;; refresh manually with gr
  (setq kubernetes-poll-frequency 3600)
  (setq kubernetes-redraw-frequency 3600)
  (defun lc/load-kubernetes-evil ()
    "Copied from kubernetes-evil.el"
    (evil-set-initial-state 'kubernetes-mode 'motion)
    (evil-set-initial-state 'kubernetes-display-thing-mode 'motion)
    (evil-set-initial-state 'kubernetes-log-line-mode 'motion)
    (evil-set-initial-state 'kubernetes-logs-mode 'motion)
    (evil-set-initial-state 'kubernetes-overview-mode 'motion)

    (evil-define-key 'motion kubernetes-mode-map
      (kbd "p")   #'magit-section-backward
      (kbd "n")   #'magit-section-forward
      (kbd "M-p") #'magit-section-backward-sibling
      (kbd "M-n") #'magit-section-forward-sibling
      (kbd "C-i") #'magit-section-toggle
      (kbd "^")   #'magit-section-up
      [tab]       #'magit-section-toggle
      [C-tab]     #'magit-section-cycle
      [M-tab]     #'magit-section-cycle-diffs
      [S-tab]     #'magit-section-cycle-global

      [remap evil-next-line] #'next-line
      [remap evil-previous-line] #'previous-line
      [remap evil-next-visual-line] #'next-line
      [remap evil-previous-visual-line] #'previous-line

      (kbd "q") #'quit-window
      (kbd "RET") #'kubernetes-navigate
      (kbd "M-w") #'kubernetes-copy-thing-at-point

      (kbd "?") #'kubernetes-overview-popup
      (kbd "c") #'kubernetes-config-popup
      (kbd "g r") #'kubernetes-refresh
      (kbd "h") #'describe-mode
      (kbd "d") #'kubernetes-describe-popup
      (kbd "D") #'kubernetes-mark-for-delete
      (kbd "e") #'kubernetes-exec-popup
      (kbd "u") #'kubernetes-unmark
      (kbd "U") #'kubernetes-unmark-all
      (kbd "x") #'kubernetes-execute-marks
      (kbd "l") #'kubernetes-logs-popup
      (kbd "L") #'kubernetes-labels-popup)

    (evil-define-key 'motion kubernetes-overview-mode-map
      (kbd "v") #'kubernetes-overview-set-sections)

    (evil-define-key 'motion kubernetes-logs-mode-map
      (kbd "n") #'kubernetes-logs-forward-line
      (kbd "p") #'kubernetes-logs-previous-line
      (kbd "RET") #'kubernetes-logs-inspect-line)

    (evil-define-key 'motion kubernetes-log-line-mode-map
      (kbd "n") #'kubernetes-logs-forward-line
      (kbd "p") #'kubernetes-logs-previous-line))
  )

(provide 'init-core)
;;; init-core.el ends here
