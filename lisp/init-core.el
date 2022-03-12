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
  (setq native-comp-async-report-warnings-errors nil)
  (setq load-prefer-newer t)


  ;; clean up the mode line
  (display-time-mode -1)
  (setq column-number-mode t)
  
  ;; use common convention for indentation by default
  (setq-default indent-tabs-mode t)
  (setq-default tab-width 2)

  ;; Enable indentation+completion using the TAB key.
  ;; Completion is often bound to M-TAB.
  (setq tab-always-indent 'complete)
  )

(use-package emacs
  :init
  (setq lc/is-ipad ( 	;; <
                    > (length (shell-command-to-string "uname -a | grep iPad")) 0))

  (setq lc/is-windows (eq system-type 'windows-nt))

  (defcustom lc/default-font-family "fira code" 
    "Default font family"
    :type 'string
    :group 'lc)

  (defcustom lc/variable-pitch-font-family "Sans Serif" ;; "cantarell" ;; 
    "Variable pitch font family"
    :type 'string
    :group 'lc)
  
  (defcustom lc/laptop-font-size
		(if lc/is-windows 100 150)
    "Font size used for laptop"
    :type 'int
    :group 'lc)

  (defcustom lc/theme nil
    "Current theme (light or dark)"
    :type 'symbol
    :options '(light dark)
    :group 'lc)
  
  ;; (setq lc/is-low-power (string= (system-name) "pntk"))

  
  ;; (setq lc/is-slow-ssh (string= (getenv "IS_TRAMP") "true"))
  
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
  ;; Set the fixed pitch face (monospace)
  (set-face-attribute 'fixed-pitch nil :font lc/default-font-family)
  ;; Set the variable pitch face
  (set-face-attribute 'variable-pitch nil :font lc/variable-pitch-font-family)
	)

(use-package emacs
  :init
  (global-set-key (kbd "C-=") 'text-scale-increase)
  (global-set-key (kbd "C--") 'text-scale-decrease)
  )

(use-package emacs
  :init
  (defun lc/is-macos ()
    (and (eq system-type 'darwin)
         (= 0 (length (shell-command-to-string "uname -a | grep iPad")))))
  (when (lc/is-macos)
    (setq mac-command-modifier 'super)     ; command as super
    (setq mac-option-modifier 'meta)     ; alt as meta
    (setq mac-control-modifier 'control)
    )
	;; when on emacs-mac 
	(when (fboundp 'mac-auto-operator-composition-mode)
      (mac-auto-operator-composition-mode)   ;; enables font ligatures
      (global-set-key [(s c)] 'kill-ring-save)
      (global-set-key [(s v)] 'yank)
      (global-set-key [(s x)] 'kill-region)
      (global-set-key [(s q)] 'kill-emacs)
      )
	)

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
  :if (lc/is-macos)
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
  ((org-jupyter-mode . (lambda () (lc/add-local-electric-pairs '())))
   (org-mode . (lambda () (lc/add-local-electric-pairs '(;(?= . ?=)
																												 (?~ . ?~))))))
  :init
  ;; auto-close parentheses
  (electric-pair-mode +1)
  (setq electric-pair-preserve-balance nil)
  ;; mode-specific local-electric pairs
  (defconst lc/default-electric-pairs electric-pair-pairs)
  (defun lc/add-local-electric-pairs (pairs)
    "Example usage: 
    (add-hook 'jupyter-org-interaction-mode '(lambda () (set-local-electric-pairs '())))
    "
    (setq-local electric-pair-pairs (append lc/default-electric-pairs pairs))
    (setq-local electric-pair-text-pairs electric-pair-pairs))

  ;; disable auto pairing for <  >
  (add-function :before-until electric-pair-inhibit-predicate
                (lambda (c) (eq c ?<   ;; >
																)))
	)

(use-package emacs
  :init
  (defun lc/rename-current-file ()
    "Rename the current visiting file and switch buffer focus to it."
    (interactive)
    (let ((new-filename (lc/expand-filename-prompt
                         (format "Rename %s to: " (file-name-nondirectory (buffer-file-name))))))
      (if (null (file-writable-p new-filename))
          (user-error "New file not writable: %s" new-filename))
      (rename-file (buffer-file-name) new-filename 1)
      (find-alternate-file new-filename)
      (message "Renamed to and now visiting: %s" (abbreviate-file-name new-filename))))
  (defun lc/expand-filename-prompt (prompt)
    "Return expanded filename prompt."
    (expand-file-name (read-file-name prompt)))
  )

(use-package xref
  :straight (:type built-in)
  :init
  (setq xref-prompt-for-identifier nil) ;; always find references of symbol at point
  ;; configured in consult
  ;; (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  ;; (setq xref-show-xrefs-function #'xref-show-definitions-buffer) ; for grep and the like
  ;; (setq xref-file-name-display 'project-relative)
  ;; (setq xref-search-program 'grep)
  )

(use-package emacs
  :init
  (defadvice keyboard-escape-quit
      (around keyboard-escape-quit-dont-close-windows activate)
    (let ((buffer-quit-function (lambda () ())))
      ad-do-it))
  )

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

  (general-nmap
    :states 'normal
    "gD" '(xref-find-references :wk "references")
    )

  (lc/leader-keys
    "SPC" '(execute-extended-command :which-key "execute command")
    "`" '((lambda () (interactive) (switch-to-buffer (other-buffer (current-buffer) 1))) :which-key "prev buffer")
    "<escape>" 'keyboard-escape-quit
    
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
    "fR" '(lc/rename-current-file :wk "rename")

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
    ;; "t +"	'(lc/increase-font-size :wk "+ font")
    ;; "t -"	'(lc/decrease-font-size :wk "- font")
    ;; "t +"	'text-scale-increase
    ;; "t -"	'text-scale-decrease
    ;; "t 0"	'(lc/reset-font-size :wk "reset font")

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
  (setq evil-want-C-i-jump t)
  (setq evil-want-Y-yank-to-eol t)
  ;; (setq evil-respect-visual-line-mode t)
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
	(define-key evil-motion-state-map "_" 'evil-end-of-line)
	(define-key evil-motion-state-map "0" 'evil-beginning-of-line)
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
    '((emacs-lisp-mode eros-eval-region)
      ;; (scheme-mode geiser-eval-region)
      (clojure-mode cider-eval-region)
			(jupyter-python jupyter-eval-region) ;; when executing in src block
      (python-mode python-shell-send-region) ;; when executing in org-src-edit mode
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
	:after evil
  :hook (emacs-lisp-mode . lc/init-cleverparens)
  :init
  (defun lc/init-cleverparens ()
    (require 'evil-cleverparens-util)
    (evil-define-text-object evil-cp-a-defun (count &optional beg end type)
      "An outer text object for a top level sexp (defun)."
      (if (evil-cp--inside-form-p)
          (let ((bounds (evil-cp--top-level-bounds)))
            (evil-range (car bounds) (cdr bounds) 'inclusive :expanded t))
        (error "Not inside a sexp.")))

    (evil-define-text-object evil-cp-inner-defun (count &optional beg end type)
      "An inner text object for a top level sexp (defun)."
      (if (evil-cp--inside-form-p)
          (let ((bounds (evil-cp--top-level-bounds)))
            (evil-range (1+ (car bounds)) (1- (cdr bounds)) 'inclusive :expanded t))
        (error "Not inside a sexp.")))
    
    (define-key evil-outer-text-objects-map "f" #'evil-cp-a-defun)
    (define-key evil-inner-text-objects-map "f" #'evil-cp-inner-defun)
    )
  )

(use-package evil-iedit-state
  :straight (evil-iedit-state :type git :host github :repo "kassick/evil-iedit-state" :branch "master")
  :general
  (lc/leader-keys
		"s e" '(evil-iedit-state/iedit-mode :wk "iedit")
		"s q" '(evil-iedit-state/quit-iedit-mode :wk "iedit quit")))

(use-package evil-mc
	:after evil
	:demand
  :general
	(general-nmap
    "M-n" #'evil-mc-make-and-goto-next-match
		)
  (general-vmap
    ;; "gm" '(:keymap evil-mc-cursors-map)
    "A" #'evil-mc-make-cursor-in-visual-selection-end
    "I" #'evil-mc-make-cursor-in-visual-selection-beg)
  (general-nmap
    "gm" '(:keymap evil-mc-cursors-map)
    "Q" #'evil-mc-undo-all-cursors
    ;; "M-p" #'evil-mc-make-and-goto-prev-cursor
    )
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
  :demand
  :general
  (lc/leader-keys
    "?" 'which-key-show-top-level
    )
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  ;; (setq which-key-idle-delay 0.5)
  :config
  (which-key-mode))

(use-package org
  ;; :straight org-plus-contrib
  ;; :straight (:type built-in)
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
    )
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
    "q" '(org-set-tags-command :wk "tag")
    "s" '(org-sort :wk "sort")
    "t" '(:ignore true :wk "todo")
    "t t" '(org-todo :wk "heading todo")
    "t s" '(org-schedule :wk "schedule")
    "t d" '(org-deadline :wk "deadline")
    "x" '(org-toggle-checkbox :wk "toggle checkbox")
    )
	(org-mode-map
   :states 'insert
   "TAB" 'nil
   "S-TAB" nil)
  (org-mode-map
   :states 'normal
   "z i" '(org-toggle-inline-images :wk "inline images"))
  :init
  ;; general settings
  (when (file-directory-p "~/org")
    (setq org-directory "~/org"
          +org-export-directory "~/org/export"
          org-default-notes-file "~/org/personal/todo.org"
          org-id-locations-file "~/org/.orgids"
          ))	
  (setq ;; org-export-in-background t
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
  ;; (setq org-latex-pdf-process '("TEXINPUTS=:$HOME/git/AltaCV//: tectonic %f"))
  (setq org-latex-pdf-process '("tectonic %f"))
  (add-to-list 'org-export-backends 'beamer)
  (plist-put org-format-latex-options :scale 1.2)
  )

(use-package org
:config
(defun my-adjoin-to-list-or-symbol (element list-or-symbol)
  (let ((list (if (not (listp list-or-symbol))
                  (list list-or-symbol)
                list-or-symbol)))
    (require 'cl-lib)
    (cl-adjoin element list)))

(eval-after-load "org"
  '(mapc
    (lambda (face)
      (set-face-attribute
       face nil
       :inherit
       (my-adjoin-to-list-or-symbol
        'fixed-pitch
        (face-attribute face :inherit))))
    (list 'org-code 'org-block
					;; 'org-table 'org-block-background
					)))
	)

(use-package org
  :general
  (lc/leader-keys
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
  (setq org-agenda-files '())
  ;; if org folder exists, use agenda files
  (when (file-directory-p "~/org/personal")
    (setq org-agenda-files
          (append org-agenda-files
                  '("~/org/personal/birthdays.org"))))   
  
  ;; if roam work folder exists, add to agenda files
  (when (file-directory-p "~/roam/work")
    (setq org-agenda-files
          (append org-agenda-files
                  '("~/roam/work/todo.org"))))   
  
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
	
	;; (use-package git)
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
          ("d" "New Diary Entry" entry(file+olp+datetree"~/org/personal/diary.org" "Daily Logs")
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
           (file+function "~/org/personal/dailies.org" org-reverse-datetree-goto-date-in-file)
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

(use-package org
	:config
	(require 's)
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
  (let ((command (if (file-directory-p "/Applications/Emacs.app")
                     "/Applications/Emacs.app/Contents/MacOS/Emacs %s --batch --eval '(org-babel-tangle nil \"%s\")'"
                   ;; on iPad
                   "emacs %s --batch --eval '(org-babel-tangle nil \"%s\")'  2>&1 | grep -v '^Loading.*\.\.\.$' | grep -v '^Using ' | grep -v '^dump '| grep -v '^Finding '"
                   )))
    ;; prevent emacs from killing until tangle-process finished
    (add-to-list 'kill-emacs-query-functions
                 (lambda ()
                   (or (not (process-live-p (get-process "tangle-process")))
                       (y-or-n-p "\"fk/tangle-config\" is running; kill it? "))))
    ;; tangle config asynchronously
    (lc/async-process
     (format command
             (expand-file-name "readme.org" user-emacs-directory)
             (expand-file-name "init.el" user-emacs-directory))
     "tangle-process")
    )

  )
)

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
     ;; (ledger . t)
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
    :keymaps 'org-mode-map
    :states 'normal
    "RET"   #'org-open-at-point
    ;; "RET"   #'+org/dwim-at-point
		)
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

  )

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

(use-package all-the-icons
  :if (not lc/is-ipad)
  :demand
  )

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

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

(use-package modus-themes
  :straight (modus-themes :type git :host gitlab :repo "protesilaos/modus-themes" :branch "main")
  :demand
  :if (display-graphic-p)
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
        modus-themes-mixed-fonts t
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
    ;; (with-eval-after-load 'org (plist-put org-format-latex-options :foreground "whitesmoke"))
    (with-eval-after-load 'org (plist-put org-format-latex-options :background "Transparent"))
    (with-eval-after-load 'org-html-themify
      (setq org-html-themify-themes '((light . modus-vivendi) (dark . modus-vivendi))))
    (modus-themes-load-vivendi)
    (lc/update-centaur-tabs)
		)
  (defun lc/load-light-theme ()
    (setq lc/theme 'light)
    ;; (with-eval-after-load 'org (plist-put org-format-latex-options :foreground "dark"))
    (with-eval-after-load 'org (plist-put org-format-latex-options :background  "Transparent"))
    (with-eval-after-load 'org-html-themify
      (setq org-html-themify-themes '((light . modus-operandi) (dark . modus-operandi))))
    (setenv "BAT_THEME" "ansi")
    (modus-themes-load-operandi)
    (lc/update-centaur-tabs))
  (defun lc/update-centaur-tabs ()
    (centaur-tabs-display-update)
    (centaur-tabs-headline-match)
    (set-face-attribute 'centaur-tabs-selected nil :overline (face-background 'centaur-tabs-active-bar-face)))
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
  (setq centaur-tabs-enable-ido-completion nil)
  :config
  (centaur-tabs-mode t)
  ;; (centaur-tabs-headline-match)
  (centaur-tabs-group-by-projectile-project)
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
    (or (thread-first (nth 3 (split-string (current-time-string) " ")) ;; time of the day e.g. 18
            ;; (substring 0 2)
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

(use-package emacs
  :init
  (setq display-buffer-alist
        `((,(rx bos (or "*Apropos*" "*Help*" "*helpful" "*info*" "*Summary*") (0+ not-newline))
           (display-buffer-reuse-mode-window display-buffer-below-selected)
           (window-height . 0.33)
           (mode apropos-mode help-mode helpful-mode Info-mode Man-mode))))
  )
;; reuse existing windows
;; (setq display-buffer-alist
;;       '((".*"
;;          (display-buffer-reuse-window display-buffer-same-window)
;;          (reusable-frames . t))))

;; (setq even-window-sizes nil)  ; display-buffer hint: avoid resizing

(use-package display-fill-column-indicator
  :straight (:type built-in)
  :hook
  (python-mode . display-fill-column-indicator-mode)
  :init
  (setq-default fill-column  90)
  ;; (setq display-fill-column-indicator-character "|")
	)

;; add a visual intent guide
(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :init
  ;; (setq highlight-indent-guides-method 'column)
  ;; (setq highlight-indent-guides-method 'bitmap)
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-character ?‖)
  (setq highlight-indent-guides-responsive 'top)
  ;; (setq highlight-indent-guides-responsive 'stack)
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

(use-package emacs
  :init
  (unless (> (display-color-cells) 8)
    (setq custom-theme-directory (concat user-emacs-directory "themes"))
    (custom-set-variables '(custom-enabled-themes '(8colors manoj-dark)))
    ))

(use-package marginalia
  :after vertico
  :init
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  (marginalia-mode)
  (with-eval-after-load 'projectile
    (add-to-list 'marginalia-command-categories '(projectile-find-file . file)))
  )

(use-package embark
  :after vertico
  :general
  (general-nmap "C-l" 'embark-act)
  (vertico-map
   "C-l" #'embark-act
   )
  (:keymaps 'embark-file-map
            ;; "o" 'find-file-other-window
            "x" 'lc/dired-open-externally
						)	
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  ;; (add-hook 'embark-setup-hook 'selectrum-set-selected-candidate)
  )

(use-package wgrep
  :general
  (grep-mode-map "W" 'wgrep-change-to-wgrep-mode)
  :init
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-change-readonly-file t)
  )

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
    ;; "s !" '(consult-flymake :wk "flymake")
    )
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)	
  ;; (setq consult-preview-key "C-l")
  ;; (setq consult-narrow-key ">")
  :config
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root)
  (with-eval-after-load 'selectrum
    (require 'consult-selectrum))
  )

(use-package embark-consult
  :after (embark consult)
  ;; :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  ;; :hook
  ;; (embark-collect-mode . embark-consult-preview-minor-mode)
	)

(use-package vertico
  ;; :straight (vertico :type git :host github :repo "minad/vertico")
  :straight (vertico :files (:defaults "extensions/*")
                     :includes (vertico-indexed
                                vertico-flat
                                vertico-grid
                                vertico-mouse
                                ;; vertico-quick
                                vertico-buffer
                                vertico-repeat
                                vertico-reverse
                                vertico-directory
                                vertico-multiform
                                vertico-unobtrusive
                                ))
  :demand
  :hook
  ((minibuffer-setup . vertico-repeat-save) ; Make sure vertico state is saved for `vertico-repeat'
   (rfn-eshadow-update-overlay . vertico-directory-tidy) ; Clean up file path when typing
   ) 
  :general
  (:keymaps 'vertico-map
            "C-j" #'vertico-next
            "C-k" #'vertico-previous
            "<escape>" #'minibuffer-keyboard-quit ; Close minibuffer
            ;; "C-;" #'kb/vertico-multiform-flat-toggle
            "M-<backspace>" #'vertico-directory-delete-word
            )
  (:keymaps '(normal insert visual motion)
            "M-." #'vertico-repeat) ; Perfectly return to the state of the last Vertico minibuffer usage
  ;; :bind (:map vertico-map
  ;;             ("C-j" . vertico-next)
  ;;             ("C-k" . vertico-previous)
  ;;             ("<escape>" . minibuffer-keyboard-quit)
  ;;             )
  :init
  ;; (setq vertico-resize t)
  
  ;; multiform extension
  (setq vertico-grid-separator "       ")
  (setq vertico-grid-lookahead 50)
  (setq vertico-buffer-display-action '(display-buffer-reuse-window))
  (setq vertico-multiform-categories
        '((file indexed)
          (consult-grep buffer)
          (consult-location)
          (imenu buffer)
          (library reverse indexed)
          (org-roam-node reverse indexed)
          (t reverse)
          ))
  (setq vertico-multiform-commands
        '(("flyspell-correct-*" grid reverse)
          (org-refile grid reverse indexed)
          (consult-yank-pop indexed)
          (consult-flycheck)
          (consult-lsp-diagnostics)
          ))
  (defun kb/vertico-multiform-flat-toggle ()
    "Toggle between flat and reverse."
    (interactive)
    (vertico-multiform--display-toggle 'vertico-flat-mode)
    (if vertico-flat-mode
        (vertico-multiform--temporary-mode 'vertico-reverse-mode -1)
      (vertico-multiform--temporary-mode 'vertico-reverse-mode 1)))

  ;; Workaround for problem with `tramp' hostname completions. This overrides
  ;; the completion style specifically for remote files! See
  ;; https://github.com/minad/vertico#tramp-hostname-completion
  (defun lc/basic-remote-try-completion (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-try-completion string table pred point)))
  (defun lc/basic-remote-all-completions (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-all-completions string table pred point)))
  (add-to-list 'completion-styles-alist
               '(basic-remote           ; Name of `completion-style'
                 lc/basic-remote-try-completion lc/basic-remote-all-completions nil))

  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args)))

  :config
  ;; (vertico-multiform-mode)	
  (vertico-mode)

  ;; Prefix the current candidate with “» ”. From
  ;; https://github.com/minad/vertico/wiki#prefix-current-candidate-with-arrow
  (advice-add #'vertico--format-candidate :around
              (lambda (orig cand prefix suffix index _start)
                (setq cand (funcall orig cand prefix suffix index _start))
                (concat
                 (if (= vertico--index index)
                     (propertize "» " 'face 'vertico-current)
                   "  ")
                 cand)))

  
  )

(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(use-package dabbrev
	:general
	(python-mode-map
   :states 'insert
   "<backtab>" 'dabbrev-completion
   ;; ("C-M-/" . dabbrev-expand)
   )
  )

;; Configure corfu
(use-package corfu
  :straight (corfu :type git :host github :repo "minad/corfu")
  ;; :hook (after-init . corfu-global-mode)
  :hook ((prog-mode . corfu-mode)
         (org-mode . corfu-mode))
  :bind
  (:map corfu-map
        ("C-j" . corfu-next)
        ("C-k" . corfu-previous))
  :general
  (evil-insert-state-map "C-k" nil)
  :init
  (setq corfu-auto nil)                 ;; Enable auto completion
  (setq corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (setq corfu-min-width 80)
  (setq corfu-max-width corfu-min-width)       ; Always have the same width
	 (setq corfu-preselect-first t)   
	 
	 (defun corfu-enable-always-in-minibuffer ()
  "Enable Corfu in the minibuffer if Vertico/Mct are not active."
  (unless (or (bound-and-true-p mct--active) ; Useful if I ever use MCT
              (bound-and-true-p vertico--input))
    (setq-local corfu-auto nil)       ; Ensure auto completion is disabled
    (corfu-mode 1)))
   (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)
  ;; :custom
  ;; (corfu-commit-predicate nil)   ;; Do not commit selected candidates on next input
  ;; (corfu-quit-at-boundary t)     ;; Automatically quit at word boundary
  ;; (corfu-quit-no-match t)        ;; Automatically quit if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin
  )

(use-package kind-icon
  :straight (kind-icon :type git :host github :repo "jdtsmith/kind-icon")
  :after corfu :demand
  :init
  (setq kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  (setq kind-icon-blend-background nil)  ; Use midpoint color between foreground and background colors ("blended")?
  (setq kind-icon-blend-frac 0.08)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  ;; refresh kind icon cache to match theme	
  (with-eval-after-load 'modus-themes
    (add-hook 'modus-themes-after-load-theme-hook #'(lambda () (interactive) (kind-icon-reset-cache))))

)

(use-package projectile
  :demand
  :general
  (lc/leader-keys
    :states 'normal
    "p" '(:keymap projectile-command-map :which-key "project")
    "p <escape>" 'keyboard-escape-quit
    "p a" '(projectile-add-known-project :wk "add known")
    "p F" '(lc/projectile-find-file-all :wk "find file (all)")
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
  (defun lc/projectile-find-file-all ()
    (interactive)
    (let ((projectile-git-command "git ls-files -zco"))
      (projectile-find-file)))
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
    "TAB" '(:ignore true :wk "tab")
    "TAB TAB" 'persp-switch
    "TAB `" 'persp-switch-last
    "TAB d" 'persp-kill
    "TAB h" 'persp-prev
    "TAB l" 'persp-next
    "TAB x" '((lambda () (interactive) (persp-kill (persp-current-name))) :wk "kill current")
    "TAB X" '((lambda () (interactive) (persp-kill (persp-names))) :wk "kill all")
    "TAB m" '(lc/main-tab :wk "main"))
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
  (defun lc/is-persp-empty? ()
    (seq-filter
     ;; filter away buffers which should be hidden
     (lambda (buffer-name) (not (string-prefix-p "*" buffer-name)))
     ;; get list of buffer names in current perspective
     (mapcar (lambda (elm) (buffer-name (car elm)))
             (centaur-tabs-view (centaur-tabs-current-tabset)))
     ))
  :config
  (persp-mode)
  (add-hook 'kill-emacs-hook #'persp-state-save))

(use-package persp-projectile
  :after projectile
  :init
	(defun lc/get-last-folder-from-known-proj (path)
		"/path/to/something/ returns something"
    (car (last (split-string path "\/") 2)))
  (defun lc/find-project-from-persp (persp-name)
		"known-proj returns /path/to/known-proj"
    (car
     (seq-filter
      (lambda (proj) (string= persp-name (lc/get-last-folder-from-known-proj proj)))
      projectile-known-projects-on-file)))
  (defun lc/persp-reload-project ()
    (interactive)
    (let* ((persp (persp-current-name))
           (proj-root (lc/find-project-from-persp persp)))
      (persp-kill persp)
      (projectile-persp-switch-project proj-root)))
  :general
  (lc/leader-keys
    "p p" 'projectile-persp-switch-project
    "TAB r" '(lc/persp-reload-project :wk "reload")
    ;; "TAB o"	'((lambda () (interactive)
    ;;               (let ((projectile-switch-project-action #'projectile-find-file))
    ;;                 (projectile-persp-switch-project "org")))
    ;;             :wk "org")
    )
  )

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
    "X" '(lc/dired-open-externally :wk "open external"))
  :init
  (setq dired-omit-files "^\\.[^.]\\|$Rhistory\\|$RData\\|__pycache__")
  (setq dired-listing-switches "-lah")
	(setq ls-lisp-dirs-first t)
	(setq ls-lisp-use-insert-directory-program nil)
  (setq dired-dwim-target t)
  (defun lc/dired-open-externally ()
    "Open marked dired file/folder(s) (or file/folder(s) at point if no marks)
  with external application"
    (interactive)
    (let ((fn (dired-get-file-for-visit)))
              (start-process "open-external" nil "open" fn)))
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
  :if (display-graphic-p)
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

(use-package rainbow-delimiters
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
         (clojure-mode . rainbow-delimiters-mode))
  )

(use-package restart-emacs
  :general
  (lc/leader-keys
    "R" '(restart-emacs :wk "restart"))
  )

(use-package term
  :if lc/is-ipad
  :straight (:type built-in)
  :general
  (lc/leader-keys
    "'" (lambda () (interactive) (term "/bin/zsh")))
  )

(use-package term
  :if lc/is-windows
  :straight (:type built-in)
  :general
  (lc/leader-keys
    "'" (lambda () (interactive)
          (let ((explicit-shell-file-name "C:/Program Files/Git/bin/bash"))
            (call-interactively 'shell))))
  ;; (setq explicit-shell-file-name "C:/Program Files/Git/bin/bash")
  ;; (setq explicit-bash.exe-args '("--login" "-i"))
  )

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
  (setq remote-file-name-inhibit-cache nil)
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

(use-package undo-fu
  :demand
  :general
  (:states 'normal
           "u" 'undo-fu-only-undo
           "s-z" 'undo-fu-only-undo
           "\C-r" 'undo-fu-only-redo))

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
    "TAB" #'magit-section-toggle
    "<escape>" #'transient-quit-one)
  :init
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (setq magit-log-arguments '("--graph" "--decorate" "--color"))
  (setq git-commit-fill-column 72)
	;; (setq magit-log-margin (t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))
	;; (when lc/is-ipad (require 'sendmail))
  :config
	(setq magit-buffer-name-format (concat "*" magit-buffer-name-format "*"))
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

(use-package hydra
	:after evil
  :demand
  :general
  (lc/leader-keys "w w" 'evil-windows-hydra/body)
  :init
  (defhydra evil-windows-hydra (:hint nil
                                      ;; :pre (smerge-mode 1)
                                      ;; :post (smerge-auto-leave)
                                      )
    "
 [_h_] ⇢⇠ decrease width [_l_] ⇠⇢ increase width
 [_j_] decrease height [_k_] increase height
│ [_q_] quit"
    ("h" evil-window-decrease-width)
    ("l" evil-window-increase-width)
    ("j" evil-window-decrease-height)
    ("k" evil-window-increase-height)
    ("q" nil :color blue)
    )
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
   "TAB" 'nil
   "C-TAB" 'yas-expand)
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

(use-package yasnippet
  :config
  (setq lc/greek-alphabet
        '(("a" . "\\alpha")
          ("b" . "\\beta" )
          ("g" . "\\gamma")
          ("d" . "\\delta")
          ("e" . "\\epsilon")
          ("z" . "\\zeta")
          ("h" . "\\eta")
          ("t" . "\\theta")
          ("i" . "\\iota")
          ("k" . "\\kappa")
          ("l" . "\\lambda")
          ("m" . "\\mu")
          ("n" . "\\nu")
          ("x" . "\\xi")
          ("p" . "\\pi")
          ("r" . "\\rho")
          ("s" . "\\sigma")
          ("t" . "\\tau")
          ("u" . "\\upsilon")
          ("f" . "\\phi")
          ("c" . "\\chi")
          ("v" . "\\psi")
          ("g" . "\\omega")))

  (setq lc/latex-greek-prefix "'")

  ;; The same for capitalized letters
  (dolist (elem lc/greek-alphabet)
    (let ((key (car elem))
          (value (cdr elem)))
      (when (string-equal key (downcase key))
        (add-to-list 'lc/greek-alphabet
                     (cons
                      (capitalize (car elem))
                      (concat
                       (substring value 0 1)
                       (capitalize (substring value 1 2))
                       (substring value 2)))))))

  (yas-define-snippets
   'latex-mode
   (mapcar
    (lambda (elem)
      (list (concat lc/latex-greek-prefix (car elem)) (cdr elem) (concat "Greek letter " (car elem))))
    lc/greek-alphabet))
  
  (setq lc/english-alphabet
        '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"))

  (dolist (elem lc/english-alphabet)
    (when (string-equal elem (downcase elem))
      (add-to-list 'lc/english-alphabet (upcase elem))))

  (setq lc/latex-mathbb-prefix "`")

  (yas-define-snippets
   'latex-mode
   (mapcar
    (lambda (elem)
      (list (concat lc/latex-mathbb-prefix elem) (concat "\\mathbb{" elem "}") (concat "Mathbb letter " elem)))
    lc/english-alphabet))

  (setq lc/latex-math-symbols
        '(("x" . "\\times")
          ("." . "\\cdot")
          ("v" . "\\forall")
          ("s" . "\\sum_{$1}^{$2}$0")
          ("p" . "\\prod_{$1}^{$2}$0")
          ("e" . "\\exists")
          ("i" . "\\int_{$1}^{$2}$0")
          ("c" . "\\cap")
          ("u" . "\\cup")
          ("0" . "\\emptyset")))

  (setq lc/latex-math-prefix "''")

  (yas-define-snippets
   'latex-mode
   (mapcar
    (lambda (elem)
      (let ((key (car elem))
            (value (cdr elem)))
        (list (concat lc/latex-math-prefix key) value (concat "Math symbol " value))))
    lc/latex-math-symbols))
  )

(use-package emacs
  :general
  (lc/leader-keys
    "s g" '(google-search :wk "google"))
  :init
  (defun google-search-str (str)
    (browse-url
     (concat "https://www.google.com/search?q=" str)))
  (defun google-search ()
    "Google search region, if active, or ask for search string."
    (interactive)
    (if (region-active-p)
        (google-search-str
         (buffer-substring-no-properties (region-beginning)
                                         (region-end)))
      (google-search-str (read-from-minibuffer "Search: "))))
  )

(use-package emacs
  :general
  (lc/leader-keys
    "s c" '(github-code-search :wk "code (github)"))
  :init
  (defun github-code-search ()
    "Search code on github for a given language."
    (interactive)
    (let ((language (completing-read
                     "Language: "
                     '("Emacs Lisp" "Python"  "Clojure" "R")))
          (code (read-string "Code: ")))
      (browse-url
       (concat "https://github.com/search?l=" language
               "&type=code&q=" code))))
  )

(use-package transient
  :general
  (lc/leader-keys
    "h h" 'lc/help-transient)
  :config
  (transient-define-prefix lc/help-transient ()
    ["Help Commands"
     ["Mode & Bindings"
      ("m" "Mode" describe-mode)
      ("b" "Major Bindings" which-key-show-full-major-mode)
      ("B" "Minor Bindings" which-key-show-full-minor-mode-keymap)
      ("d" "Descbinds" describe-bindings)
      ]
     ["Describe"
      ("c" "Command" helpful-command)
      ("f" "Function" helpful-callable)
      ("v" "Variable" helpful-variable)
      ("k" "Key" helpful-key)
      ]
     ["Info on"
      ("C-c" "Emacs Command" Info-goto-emacs-command-node)
      ("C-f" "Function" info-lookup-symbol) 
      ("C-v" "Variable" info-lookup-symbol)
      ("C-k" "Emacs Key" Info-goto-emacs-key-command-node)
      ]
     ["Goto Source"
      ("L" "Library" find-library)
      ("F" "Function" find-function)
      ("V" "Variable" find-variable)
      ("K" "Key" find-function-on-key)
      ]
     ]
    [
     ["Internals"
      ("e" "Echo Messages" view-echo-area-messages)
      ("l" "Lossage" view-lossage)
      ]
     ["Describe"
      ("s" "Symbol" helpful-symbol)
      ("." "At Point   " helpful-at-point)
      ;; ("C-f" "Face" counsel-describe-face)
      ("w" "Where Is" where-is)
      ("=" "Position" what-cursor-position)
      ]
     ["Info Manuals"
      ("C-i" "Info" info)
      ("C-4" "Other Window " info-other-window)
      ("C-e" "Emacs" info-emacs-manual)
      ;; ("C-l" "Elisp" info-elisp-manual)
      ]
     ["Exit"
      ("q" "Quit" transient-quit-one)
      ("<escape>" "Quit" transient-quit-one)
      ]
     ;; ["External"
     ;;  ("W" "Dictionary" lookup-word-at-point)
     ;;  ("D" "Dash" dash-at-point)
     ;;  ]
     ]
    )
  )

(use-package transient
  :general
  (lc/leader-keys
    "t f" 'lc/font-size-transient)
  :config
  (transient-define-prefix lc/font-size-transient ()
    "Change font size"
    ["Font size"
     ("+" "Increase" (lambda () (interactive) (progn (text-scale-increase) (lc/font-size-transient))))
     ("-" "Decrease" (lambda () (interactive) (progn (text-scale-decrease) (lc/font-size-transient))))
     ])
  )

(use-package isearch-mb
  :straight (isearch-mb :type git :host github :repo "astoff/isearch-mb")
  :demand
  :init
  (setq-default
   ;; Match count next to minibuffer prompt
   isearch-lazy-count t
   ;; Don't be stingy with history; default is to keep just 16 entries
   search-ring-max 200
   regexp-search-ring-max 200
   ;; fuzzy match with space
   isearch-regexp-lax-whitespace t
   search-whitespace-regexp ".*?"
   )
  :config
  (add-to-list 'isearch-mb--with-buffer #'loccur-isearch)
  (define-key isearch-mb-minibuffer-map (kbd "C-o") #'loccur-isearch)
  )

(provide 'init-core)
;;; init-core.el ends here
