;; NOTE: init.el is now generated from readme.org.  Please edit that file instead

; repeating here in case early-init.el is not loaded with chemacs
(setq gc-cons-threshold most-positive-fixnum)
(setq package-enable-at-startup nil)
(setq comp-deferred-compilation nil)

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
)

;; Ensure Doom is running out of this file's directory
(setq user-emacs-directory (file-name-directory load-file-name))

(setq straight-use-package-by-default t)
(setq straight-vc-git-default-clone-depth 1)
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

;; (setq use-package-compute-statistics t)

(use-package gcmh
  :config
  (gcmh-mode 1)
  )

(use-package emacs
  :straight nil
  :ensure nil
  :config
  (setq inhibit-startup-screen t
        default-fill-column 80
        initial-scratch-message nil
        sentence-end-double-space nil
        ring-bell-function 'ignore
        frame-resize-pixelwise t)

  (setq user-full-name "Luca Cambiaghi"
        user-mail-address "luca.cambiaghi@me.com")

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

  ;; don't show any extra window chrome
  (when (window-system)
    (tool-bar-mode -1)
    (scroll-bar-mode -1)
    (tooltip-mode -1)
    (menu-bar-mode   -1)
    (toggle-scroll-bar -1))

  ;; use a font I like, but fail gracefully if it isn't available
  (ignore-errors (set-frame-font "Fira Code Retina 18"))

  ;; enable winner mode globally for undo/redo window layout changes
  (winner-mode t)

  ;; clean up the mode line
  (display-time-mode -1)
  ;; (setq-default mode-line-format nil) ;TODO
  (setq column-number-mode t))

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'super)     ; command as super
  (setq mac-option-modifier 'meta)     ; alt as meta
  (setq mac-control-modifier 'control)) ; control as... control

(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key)
  )

(use-package eldoc
  :config
  (global-eldoc-mode 1))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (setq exec-path-from-shell-arguments '("-l")) ; removed the -i for faster startup
  (exec-path-from-shell-initialize)
  ;; (exec-path-from-shell-copy-envs
  ;;  '("GOPATH" "GO111MODULE" "GOPROXY"
  ;;    "NPMBIN" "LC_ALL" "LANG" "LC_TYPE"
  ;;    "SSH_AGENT_PID" "SSH_AUTH_SOCK" "SHELL"
  ;;    "JAVA_HOME"))
  )

(use-package general
  :demand t
  :config
  (general-evil-setup)

  (general-create-definer my/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (general-create-definer my/local-leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix ","
    :global-prefix "SPC m")

  (my/leader-keys
    "SPC" '(execute-extended-command :which-key "execute command")
    "`" '(switch-to-other-buffer :which-key "prev buffer")
    ";" '(eval-expression :which-key "eval sexp")

    "b" '(:ignore t :which-key "buffer")
    "br"  'revert-buffer
    "bd"  'kill-current-buffer

    "f" '(:ignore t :which-key "file")
    "ff"  'find-file
    "fs" 'save-buffer
    "fr" 'recentf-open-files

    "g" '(:ignore t :which-key "git")

    "h" '(:ignore t :which-key "describe")
    "hv" 'describe-variable
    "he" 'view-echo-area-messages
    "hp" 'describe-package
    "hf" 'describe-function
    "hF" 'describe-face
    "hk" 'describe-key

    "p" '(:ignore t :which-key "project")

    "s" '(:ignore t :which-key "search")

    "t"  '(:ignore t :which-key "toggle")

    "w" '(:ignore t :which-key "window")
    "wl"  'windmove-right
    "wh"  'windmove-left
    "wk"  'windmove-up
    "wj"  'windmove-down
    "wd"  'delete-window
    "wu" 'winner-undo
    "wr" 'winner-redo
    )

  ;; (my/leader-keys
  ;;   "" '(nil :which-key "local leader")
  ;;   )
  )

(use-package evil
  :demand t
  :general
  (general-nmap "SPC w v" 'evil-window-vsplit)
  (general-nmap "SPC w s" 'evil-window-split)
  (evil-motion-state-map "," nil)
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-want-Y-yank-to-eol t)
  ;; move to window when splitting
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-nerd-commenter
  :demand t
  :general
  (general-nmap "gcc" 'evilnc-comment-or-uncomment-lines)
  (general-vmap "gc" 'evilnc-comment-or-uncomment-lines)
  )

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package which-key
  :demand t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  ;; (setq which-key-idle-delay 0.5)
  :config
  (which-key-mode))

(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq ns-use-proxy-icon  nil)
(setq frame-title-format nil)

(use-package modus-themes
  ;; :straight (modus-themes :type git :host gitlab :repo "protesilaos/modus-themes" :branch "master")
  :demand
  :init
  (setq modus-operandi-theme-override-colors-alist
        '(("bg-main" . "#fefcf4")
          ("bg-dim" . "#faf6ef")
          ("bg-alt" . "#f7efe5")
          ("bg-hl-line" . "#f4f0e3")
          ("bg-active" . "#e8dfd1")
          ("bg-inactive" . "#f6ece5")
          ("bg-region" . "#c6bab1")
          ("bg-header" . "#ede3e0")
          ("bg-tab-bar" . "#dcd3d3")
          ("bg-tab-active" . "#fdf6eb")
          ("bg-tab-inactive" . "#c8bab8")
          ("fg-unfocused" . "#55556f"))
        modus-operandi-theme-slanted-constructs t
        modus-operandi-theme-bold-constructs t
        modus-operandi-theme-fringes 'subtle ; {nil,'subtle,'intense}
        modus-operandi-theme-mode-line '3d ; {nil,'3d,'moody}
        modus-operandi-theme-faint-syntax nil
        modus-operandi-theme-intense-hl-line nil
        modus-operandi-theme-intense-paren-match nil
        modus-operandi-theme-no-link-underline t
        modus-operandi-theme-no-mixed-fonts nil
        modus-operandi-theme-prompts nil ; {nil,'subtle,'intense}
        modus-operandi-theme-completions 'moderate ; {nil,'moderate,'opinionated}
        modus-operandi-theme-diffs nil ; {nil,'desaturated,'fg-only}
        modus-operandi-theme-org-blocks 'greyscale ; {nil,'greyscale,'rainbow}
        modus-operandi-theme-headings  ; Read further below in the manual for this one
        '((1 . line)
          (t . rainbow-line-no-bold))
        modus-operandi-theme-variable-pitch-headings t
        modus-operandi-theme-scale-headings t
        modus-operandi-theme-scale-1 1.1
        modus-operandi-theme-scale-2 1.15
        modus-operandi-theme-scale-3 1.21
        modus-operandi-theme-scale-4 1.27
        modus-operandi-theme-scale-5 1.33)

  (setq modus-vivendi-theme-override-colors-alist
        '(("bg-main" . "#100b17")
          ("bg-dim" . "#161129")
          ("bg-alt" . "#181732")
          ("bg-hl-line" . "#191628")
          ("bg-active" . "#282e46")
          ("bg-inactive" . "#1a1e39")
          ("bg-region" . "#393a53")
          ("bg-header" . "#202037")
          ("bg-tab-bar" . "#262b41")
          ("bg-tab-active" . "#120f18")
          ("bg-tab-inactive" . "#3a3a5a")
          ("fg-unfocused" . "#9a9aab"))
        modus-vivendi-theme-intense-paren-match t
        modus-vivendi-theme-distinct-org-blocks t
        modus-vivendi-theme-slanted-constructs t
        modus-vivendi-theme-bold-constructs t
        modus-vivendi-theme-fringes 'subtle ; {nil,'subtle,'intense}
        modus-vivendi-theme-mode-line '3d ; {nil,'3d,'moody}
        modus-vivendi-theme-faint-syntax nil
        modus-vivendi-theme-intense-hl-line nil
        modus-vivendi-theme-intense-paren-match nil
        modus-vivendi-theme-no-link-underline t
        modus-vivendi-theme-no-mixed-fonts nil
        modus-vivendi-theme-prompts nil ; {nil,'subtle,'intense}
        modus-vivendi-theme-completions 'moderate ; {nil,'moderate,'opinionated}
        modus-vivendi-theme-diffs nil ; {nil,'desaturated,'fg-only}
        modus-vivendi-theme-org-blocks 'greyscale ; {nil,'greyscale,'rainbow}
        modus-vivendi-theme-headings  ; Read further below in the manual for this one
        '((1 . line)
          (t . rainbow-line-no-bold))
        modus-vivendi-theme-variable-pitch-headings t
        modus-vivendi-theme-scale-headings t
        modus-vivendi-theme-scale-1 1.1
        modus-vivendi-theme-scale-2 1.15
        modus-vivendi-theme-scale-3 1.21
        modus-vivendi-theme-scale-4 1.27
        modus-vivendi-theme-scale-5 1.33)
  )

(use-package solar
  :straight nil
  :ensure nil
  :demand
  :config
  (setq calendar-latitude 55.67
        calendar-longitude 12.56))

(use-package circadian
  :after (solar modus-themes)
  :demand
  :config
  (setq circadian-themes '((:sunrise . modus-operandi)
                           (:sunset  . modus-vivendi)))
  (circadian-setup))

(use-package dashboard
  :demand
  :init
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (setq dashboard-center-content t)
  ;; (setq dashboard-startup-banner [VALUE])
  :config
  (dashboard-setup-startup-hook))

(use-package centaur-tabs
  :demand
  :general
  (general-nvmap "gt" 'centaur-tabs-forward)
  (general-nvmap "gT" 'centaur-tabs-backward)
  :init
  (setq centaur-tabs-set-icons t)
  :config
  (centaur-tabs-mode t)
  )

(use-package centered-cursor-mode
  :general (general-nmap "SPC t -" (lambda () (interactive) (centered-cursor-mode 'toggle)))
  )

(use-package selectrum
  :demand t
  :general
  (selectrum-minibuffer-map "C-j" 'selectrum-next-candidate)
  (selectrum-minibuffer-map "C-k" 'selectrum-previous-candidate)
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
  :after selectrum
  :demand
  :general (selectrum-minibuffer-map "C-o" #'embark-act)
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
  (add-hook 'embark-setup-hook 'selectrum-set-selected-candidate))

(use-package consult
    :demand t
    :general
    (general-nmap "SPC o" '(consult-outline :which-key "outline"))
    (general-nmap "SPC y" '(consult-yank-pop :which-key "yank"))
    (general-nmap "SPC b b" 'consult-buffer)
    (general-nmap "SPC f r" 'consult-recent-file)
    (general-nmap "SPC s s" 'consult-line)
    (general-nmap "SPC s !" '(consult-flymake :wk "flymake"))
    (general-nmap "SPC s p" '(consult-ripgrep :wk "ripgrep"))
    (general-nmap "SPC t t" '(consult-theme :wk "theme"))
    :config
    ; (consult-annotate-mode) ;; Enable richer annotations during completion
    (consult-preview-mode) ;; Optionally enable previews

    ;; Enable richer annotations for M-x.
    ;; (add-to-list 'consult-annotate-commands
    ;;              '(execute-extended-command . consult-annotate-symbol))
    )

(use-package consult-selectrum
  :after selectrum
  :demand)

(use-package projectile
  :demand
  ;; :general (general-nvmap "SPC pp" 'projectile-switch-project)
  :general
  (general-nmap
    "SPC p" '(:keymap projectile-command-map
                      :which-key "projectile"))
  (general-nmap
    "SPC p a" 'projectile-add-known-project)

  :custom ((projectile-completion-system 'default))
  :init
  (when (file-directory-p "~/git")
    (setq projectile-project-search-path '("~/git")))
  (setq projectile-switch-project-action #'projectile-find-file)
  :config
  (defadvice projectile-project-root (around ignore-remote first activate)
    (unless (file-remote-p default-directory) ad-do-it))
  (projectile-mode)
  )

(use-package perspective
  :general
  (general-nvmap "SPC <tab> <tab>" 'persp-switch)
  (general-nvmap "SPC <tab> `" 'persp-switch-last)
  (general-nvmap "SPC <tab> d" 'persp-kill)
  :config
  (persp-mode))

(use-package persp-projectile
  :general
  (general-nvmap "SPC p p" 'projectile-persp-switch-project)
  )

(use-package magit
  :general (general-nvmap "SPC gg" 'magit-status)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package evil-magit
  :after magit
  :demand)

(use-package git-timemachine
  :hook (git-time-machine-mode . evil-normalize-keymaps)
  :init (setq git-timemachine-show-minibuffer-details t)
  :general (general-nmap "SPC g t" 'git-timemachine-toggle)
  )

(use-package smerge-mode
  :straight nil
  :ensure nil
  :general (general-nmap "SPC g m" 'smerge-mode))

(use-package emacs
  :straight nil
  :ensure nil
  :config
  ;; use common convention for indentation by default
  (setq-default indent-tabs-mode t)
  (setq-default tab-width 2)

  ;; use a reasonable line length
  (setq-default fill-column 120)

  ;; let emacs handle indentation
  (electric-indent-mode +1)
  ;; and auto-close parentheses
  (electric-pair-mode +1)              
  )

;; add a visual intent guide
(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  ;; :custom
  ;; (highlight-indent-guides-method 'character)
  ;; (highlight-indent-guides-character ?|)
  ;; (highlight-indent-guides-responsive 'stack)
  )

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package tree-sitter
  :hook (python-mode . tree-sitter-hl-mode)
  :config (global-tree-sitter-mode))

(use-package tree-sitter-langs
  :after tree-sitter)

;; (defun my/lsp-mode-setup ()
;;   (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
;;   (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  ;; :hook (lsp-mode . my/lsp-mode-setup)
  :general
  (general-nmap
    "SPC l" '(:keymap lsp-command-map
                      :which-key "lsp"))
  :init
  (setq lsp-restart 'ignore)
  :config
  (lsp-enable-which-key-integration t))

;; (use-package lsp-ui
;;   :hook (lsp-mode . lsp-ui-mode)
;;   :custom
;;   (lsp-ui-doc-position 'bottom))

(use-package dap-mode
  :custom
  (dap-auto-configure nil)
  :config
  (dap-ui-mode 1)

  ;; Bind `C-c l d` to `dap-hydra` for easy access
  ; TODO
  ;; :general (lsp-mode-map "gcc" #'evilnc-comment-or-uncomment-lines)

  (general-define-key
    :keymaps 'lsp-mode-map
    :prefix lsp-keymap-prefix
    "d" '(dap-hydra t :wk "debugger")))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind
  (:map company-active-map
        ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  )

;; (use-package company-box
;;   :hook (company-mode . company-box-mode))

(use-package envrc
  :hook (python-mode . envrc-mode))

(use-package yasnippet
  :hook
  ((text-mode . yas-minor-mode)
   (prog-mode . yas-minor-mode)))

(use-package evil-cleverparens
  :hook
  (
   (emacs-lisp-mode . evil-cleverparens-mode)
   ;; (clojure-mode . evil-cleverparens-mode)
   )
  ;; (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)
  :general
  (evil-cleverparens-mode-map ", (" 'sp-wrap-round)
  :init
  (setq evil-move-beyond-eol t
        evil-cleverparens-use-additional-bindings nil
        evil-cleverparens-use-s-and-S nil
        ;; evil-cleverparens-swap-move-by-word-and-symbol t
        ;; evil-cleverparens-use-regular-insert t
        )
  )

(use-package python-mode
  ;; :hook (python-mode . lsp-deferred)
  :custom
  ;; NOTE: Set these if Python 3 is called "python3" on your system!
  ;; (python-shell-interpreter "python3")
  ;; (dap-python-executable "python3")
  (dap-python-debugger 'debugpy)
  :config
  (require 'dap-python))

(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp-deferred))))  ; or lsp-deferred

(use-package python-pytest
  :general
  (python-mode-map ", t" 'python-pytest-dispatch)
  )

(use-package flymake
  :straight nil
  :ensure nil
  :general
  (general-nmap "] !" 'flymake-goto-next-error)
  (general-nmap "[ !" 'flymake-goto-prev-error)
  )

(use-package vterm
  :commands vterm
  :general
  (general-nmap "SPC '" 'vterm)
  :config
  (setq vterm-shell (executable-find "fish")
        vterm-max-scrollback 10000))

(use-package dired
  :straight nil
  :ensure nil
  :commands (dired dired-jump)
  :general
  (general-nvmap "SPC fd" 'dired)
  ;; :bind (("C-x C-j" . dired-jump))
  :custom
  (dired-listing-switches "-al --group-directories-first")
  (insert-directory-program "gls" dired-use-ls-dired t)
  :config
  (with-eval-after-load 'evil-collection
    (evil-collection-define-key 'normal 'dired-mode-map
                                "h" 'dired-single-up-directory
                                "l" 'dired-single-buffer)))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(defun my/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . my/org-mode-setup)
  :general
  (general-nmap "SPC C" '(org-capture :wk "capture"))
  :init
  (setq org-directory "~/Dropbox/org"
        org-image-actual-width nil
        +org-export-directory "~/Dropbox/org/export"
        org-default-notes-file "~/Dropbox/org/personal/tasks/todo.org"
        org-id-locations-file "~/Dropbox/org/.orgids"
        org-agenda-files '("~/dropbox/org/personal/tasks/birthdays.org" "~/dropbox/org/personal/tasks/todo.org" "~/dropbox/Notes/Test.inbox.org")
        ;; org-export-in-background t
        org-catch-invisible-edits 'smart)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "PROJ(p)" "|" "DONE(d)")))
  (setq org-capture-templates
        `(("b" "Blog" entry
           (file+headline "personal/tasks/todo.org" "Blog")
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
           (file+headline "personal/tasks/todo.org" "Inbox")
           ,(concat "* %^{Title}\n"
                    ":PROPERTIES:\n"
                    ":CAPTURED: %U\n"
                    ":END:\n\n"
                    "%i%l"))
          ("u" "New URL Entry" entry
           (file+function "~/Dropbox/org/personal/dailies.org" org-reverse-datetree-goto-date-in-file)
           "* [[%^{URL}][%^{Description}]] %^g %?")
          ("w" "Work" entry
           (file+headline "personal/tasks/todo.org" "Work")
           ,(concat "* TODO [#A] %^{Title} :@work:\n"
                    "SCHEDULED: %^t\n"
                    ":PROPERTIES:\n:CAPTURED: %U\n:END:\n\n"
                    "%i%?"))

          ))

  ;; (setq org-agenda-custom-commands
  ;;         '(("d" "Dashboard"
  ;;            ((agenda "" ((org-deadline-warning-days 7)))
  ;;             (todo "NEXT"
  ;;                   ((org-agenda-overriding-header "Next Tasks")))
  ;;             (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))
  ;;           ("n" "Next Tasks"
  ;;            ((todo "NEXT"
  ;;                   ((org-agenda-overriding-header "Next Tasks")))))
  ;;           ("W" "Work Tasks" tags-todo "+work-email")
  ;;           ))
  :config
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)

  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("clj" . "src clojure"))

  ;; (efs/org-font-setup)
  )

;; Automatically tangle our readme.org config file when we save it
(defun my/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name user-emacs-directory))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'my/org-babel-tangle-config)))

(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  :init
  (setq org-superstar-headline-bullets-list '("✖" "✚" "◆" "▶" "○")
        org-superstar-special-todo-items t
        org-ellipsis "▼")
  )

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :init
  (setq hl-todo-keyword-faces
        '(("TODO"   . "#FF4500")
          ("FIXME"  . "#FF0000")
          ("DEBUG"  . "#A020F0")
          ("PROJ"   . "#1E90FF")))
  )

;; (use-package org
;;   :config
;;   (org-babel-do-load-languages
;;    'org-babel-load-languages
;;    '((ruby . t)
;;      (shell . t))))

;; enable mermaid diagram blocks
;; (use-package ob-mermaid
;;   :custom (ob-mermaid-cli-path "~/.asdf/shims/mmdc"))

(use-package ox-gfm
  :config (eval-after-load "org" '(require 'ox-gfm nil t)))

;; (use-package ox-ipynb
;;   :config (eval-after-load "org" '(require 'ox-ipynb)))

;; Let's lower our GC thresholds back down to a sane level
(setq gc-cons-threshold 16777216
  gc-cons-percentage 0.1
  file-name-handler-alist doom--initial-file-name-handler-alist)
