;; NOTE: init.el is now generated from readme.org.  Please edit that file instead
;; repeating here in case early-init.el is not loaded with chemacs
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
  (add-hook 'after-init-hook #'(lambda ()
                                 ;; restore after startup
                                 (setq gc-cons-threshold 16777216
                                       gc-cons-percentage 0.1)))
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
;; This is a variable that has been renamed but straight still refers when
;; doing :sraight (:no-native-compile t)
(setq comp-deferred-compilation-black-list nil)

;; (setq use-package-compute-statistics t)

(use-package gcmh
  :config
  (gcmh-mode 1)
  )

(use-package emacs
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
  :hook (emacs-lisp-mode cider-mode))

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
    :states '(normal visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  (general-create-definer my/local-leader-keys
    :states '(normal visual emacs)
    :keymaps 'override
    :prefix ","
    :global-prefix "SPC m")

  (my/leader-keys
    "SPC" '(execute-extended-command :which-key "execute command")
    "`" '(switch-to-prev-buffer :which-key "prev buffer")
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
    "t v" '((lambda () (interactive) (visual-line-mode)) :wk "visual line")

    "w" '(:ignore t :which-key "window")
    "wl"  'windmove-right
    "wh"  'windmove-left
    "wk"  'windmove-up
    "wj"  'windmove-down
    "wd"  'delete-window
    "wu" 'winner-undo
    "wr" 'winner-redo
    )

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
  :demand
  :config
  (evil-collection-init))

(use-package evil-goggles
  :after evil
  :demand
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

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
          ("fg-unfocused" . "#55556f")))

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
          ("fg-unfocused" . "#9a9aab")))

  (defmacro modus-themes-format-sexp (sexp &rest objects)
    `(eval (read (format ,(format "%S" sexp) ,@objects))))

  (dolist (theme '("operandi" "vivendi"))
    (modus-themes-format-sexp
     (setq modus-%1$s-theme-slanted-constructs t
             modus-%1$s-theme-bold-constructs t
             modus-%1$s-theme-fringes 'subtle ; {nil,'subtle,'intense}
             modus-%1$s-theme-mode-line '3d ; {nil,'3d,'moody}
             modus-%1$s-theme-faint-syntax nil
             modus-%1$s-theme-intense-hl-line nil
             modus-%1$s-theme-intense-paren-match nil
             modus-%1$s-theme-no-link-underline t
             modus-%1$s-theme-no-mixed-fonts nil
             modus-%1$s-theme-prompts nil ; {nil,'subtle,'intense}
             modus-%1$s-theme-completions 'moderate ; {nil,'moderate,'opinionated}
             modus-%1$s-theme-diffs nil ; {nil,'desaturated,'fg-only}
             modus-%1$s-theme-org-blocks 'greyscale ; {nil,'greyscale,'rainbow}
             modus-%1$s-theme-headings  ; Read further below in the manual for this one
             '((1 . line)
               (t . rainbow-line-no-bold))
             modus-%1$s-theme-variable-pitch-headings t
             modus-%1$s-theme-scale-headings t
             modus-%1$s-theme-scale-1 1.1
             modus-%1$s-theme-scale-2 1.15
             modus-%1$s-theme-scale-3 1.21
             modus-%1$s-theme-scale-4 1.27
             modus-%1$s-theme-scale-5 1.33)
     theme))
  :config
  ;;Light for the day
  (run-at-time "07:00" (* 60 60 24)
               (lambda () (modus-themes-load-operandi)))

  ;; Dark for the night
	(run-at-time "00:00" (* 60 60 24)
               (lambda () (modus-themes-load-vivendi)))
  (run-at-time "15:00" (* 60 60 24)
               (lambda () (modus-themes-load-vivendi)))

  )

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
  :commands (centaur-tabs-mode centaur-tabs-forward centaur-tabs-backward)
  :general
  (general-nvmap "gt" 'centaur-tabs-forward)
  (general-nvmap "gT" 'centaur-tabs-backward)
  :init
  (setq centaur-tabs-set-icons t)
  :config
  (centaur-tabs-mode t)
  )

(use-package centered-cursor-mode
  :general (my/leader-keys "t -" (lambda () (interactive) (centered-cursor-mode 'toggle))))

(use-package hide-mode-line
  :commands (hide-mode-line-mode))

(use-package emacs
  :config
  (defvar my-popups '()
    "A list of popup matchers that determine if a popup can be escaped")

  (cl-defun my/make-popup (buffer-rx &optional (height 0.4))
    (add-to-list 'my-popups buffer-rx)
    (add-to-list 'display-buffer-alist
                 `(,buffer-rx
                   (display-buffer-reuse-window
                    display-buffer-in-side-window)
                   (reusable-frames . visible)
                   (side            . bottom)
                   (window-height   . ,height))))

  (my/make-popup (rx bos "*Messages*" eos))
  (my/make-popup (rx bos "*Backtrace*" eos))
  (my/make-popup (rx bos "*Warnings*" eos))
  (my/make-popup (rx bos "*compilation*" eos))
  (my/make-popup (rx bos "*Help*" eos))
  (my/make-popup (rx bos "*scratch*" eos) 0.4)
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
  :general
  (my/leader-keys
    "s o" '(consult-outline :which-key "outline")
    "s s" 'consult-line
    "y" '(consult-yank-pop :which-key "yank")
    "b b" 'consult-buffer
    ;; TODO consult mark
    "f r" 'consult-recent-file
    "s !" '(consult-flymake :wk "flymake")
    "s p" '(consult-ripgrep :wk "ripgrep")
    "t t" '(consult-theme :wk "theme")
    )
  ;; :init
  ;; (setq consult-preview-key "C-l")
  ;; (setq consult-narrow-key ">")
  :config
  (consult-preview-mode)
  )

(use-package consult-selectrum
  :after selectrum
  :demand)

(use-package projectile
  :demand
  :general
  (my/leader-keys
   "p" '(:keymap projectile-command-map :which-key "projectile")
   "p a" 'projectile-add-known-project
   "p t" 'projectile-run-vterm)
  :init
  (when (file-directory-p "~/git")
    (setq projectile-project-search-path '("~/git")))
  (setq projectile-completion-system 'default)
  (setq projectile-switch-project-action #'projectile-find-file)
  ;; (add-to-list 'projectile-globally-ignored-directories "straight") ;; TODO
  :config
  (defadvice projectile-project-root (around ignore-remote first activate)
    (unless (file-remote-p default-directory) ad-do-it))
  (projectile-mode))

(use-package perspective
  :general
  (my/leader-keys
   "<tab> <tab>" 'persp-switch
   "<tab> `" 'persp-switch-last
   "<tab> d" 'persp-kill)
  :config
  (persp-mode))

(use-package persp-projectile
  :general
  (my/leader-keys
   "p p" 'projectile-persp-switch-project))

(use-package magit
  :general
  (my/leader-keys
    "g g" 'magit-status
    "g G" 'magit-status-here)
  :init
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  )

(use-package git-timemachine
  :hook (git-time-machine-mode . evil-normalize-keymaps)
  :init (setq git-timemachine-show-minibuffer-details t)
  :general
  (general-nmap "SPC g t" 'git-timemachine-toggle)
  (git-timemachine-mode-map "C-k" 'git-timemachine-show-previous-revision)
  (git-timemachine-mode-map "C-j" 'git-timemachine-show-next-revision)
  (git-timemachine-mode-map "q" 'git-timemachine-quit)
  )

(use-package git-gutter-fringe
  :hook
  ((text-mode
    org-mode
    prog-mode) . git-gutter-mode)
  :config
  (setq-default fringes-outside-margins t)
  )

(use-package smerge-mode
  :straight nil
  :ensure nil
  :general
  (my/leader-keys "g m" 'smerge-mode)
  :config
  (defhydra hydra-smerge (:hint nil
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
  :demand
  :config
  (setq lv-use-seperator t))

(use-package emacs
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
  :hook (lisp-mode . rainbow-delimiters-mode))

(use-package tree-sitter
  :hook (python-mode . tree-sitter-hl-mode)
  :config (global-tree-sitter-mode))

(use-package tree-sitter-langs
  :after tree-sitter)

(use-package company
  :demand
  :hook ((lsp-mode . company-mode)
         (emacs-lisp-mode . company-mode))
  :bind
  (:map company-active-map
        ("<tab>" . company-complete-selection))
  :init
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.0)
  (setq company-backends '(company-dabbrev-code company-capf company-keywords company-files company-dabbrev)))

;; (use-package company-box
;;   :hook (company-mode . company-box-mode))

(use-package envrc
  :hook (python-mode . envrc-mode))

(use-package yasnippet
  :hook
  ((text-mode . yas-minor-mode)
   (prog-mode . yas-minor-mode)
   (org-mode . yas-minor-mode)))

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
  :config
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'org-mode "<" nil :actions nil)
  )

(use-package evil-mc
  :commands (evil-mc-make-and-goto-next-match ;C-n
             evil-mc-make-and-goto-prev-match ;C-p
             evil-mc-make-cursor-here ; grh
             evil-mc-undo-all-cursors ; grq
             evil-mc-make-all-cursors ; grm
             evil-mc-make-cursor-move-next-line ; grj
             evil-mc-make-cursor-move-prev-line ; grk
             )
  :config
  (global-evil-mc-mode +1)
  )

(use-package evil-nerd-commenter
  :general
  (general-nmap "gcc" 'evilnc-comment-or-uncomment-lines)
  (general-vmap "gc" 'evilnc-comment-or-uncomment-lines)
  )

(use-package evil-surround
  :general
  (:states 'visual
           "S" 'evil-surround-region
           "gS" 'evil-Surround-region))

(use-package undo-fu
  :general
  (:states 'normal
           "u" 'undo-fu-only-undo
           "\C-r" 'undo-fu-only-redo))

(use-package vterm
  :general
  (my/leader-keys
    "'" 'vterm-other-window)
  :config
  (setq vterm-shell (executable-find "fish")
        vterm-max-scrollback 10000))

;; (defun my/lsp-mode-setup ()
;;   (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
;;   (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  ;; :hook (lsp-mode . my/lsp-mode-setup)
  :general
  (my/leader-keys
    "l" '(:keymap lsp-command-map :which-key "lsp"))

  (lsp-mode-map "<tab>" 'company-indent-or-complete-common)
  :init
  (setq lsp-restart 'ignore)
  (setq lsp-eldoc-enable-hover nil)
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook ((lsp-mode . lsp-ui-mode))
  :init
  (setq lsp-ui-doc-show-with-cursor nil)
  (setq lsp-ui-doc-show-with-mouse nil)
  )

(use-package dap-mode
  :general
  (my/local-leader-keys
    :keymaps 'python-mode-map
    "d h" '(dap-hydra :wk "hydra"))
  :init
  (setq dap-auto-configure nil)
  :config
  (dap-ui-mode 1))

(use-package python-mode
  :init
  (setq dap-python-debugger 'debugpy)
  :config
  (when (executable-find "ipython")
    (setq python-shell-interpreter (executable-find "ipython")     ;; FIXME
          python-shell-interpreter-args "-i --simple-prompt --no-color-info"
          python-shell-prompt-regexp "In \\[[0-9]+\\]: "
          python-shell-prompt-block-regexp "\\.\\.\\.\\.: "
          python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
          python-shell-completion-setup-code
          "from IPython.core.completerlib import module_completion"
          python-shell-completion-string-code
          "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
    (require 'dap-python)
    ))

(use-package lsp-pyright
  :init
  (setq lsp-pyright-typechecking-mode "off")
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))

(use-package python-pytest
:general
(my/local-leader-keys
    :keymaps 'python-mode-map
    "t h" '(python-pytest-dispatch :wk "dispatch")))

(use-package flymake
:straight nil
:ensure nil
:init
(setq python-flymake-command (executable-find "flake8"))
:general
(general-nmap "] !" 'flymake-goto-next-error)
(general-nmap "[ !" 'flymake-goto-prev-error)
)

(use-package elisp-mode
  :straight nil
  :ensure nil
  :general
  (my/local-leader-keys
    :keymaps '(org-mode-map emacs-lisp-mode-map)
    "e l" '(eval-last-sexp :wk "last sexp"))
  (my/local-leader-keys
    :keymaps '(org-mode-map emacs-lisp-mode-map)
    :states 'visual
    "e" '(eval-last-sexp :wk "sexp")))

(use-package clojure-mode
  :mode "\\.clj$")

(use-package cider
  :commands (cider-jack-in cider-mode)
  :general
  ;; (clojure-mode-map "")
  :init
  (setq nrepl-hide-special-buffers t)
  :config
  (add-hook 'cider-mode-hook #'eldoc-mode))

(use-package dired
  :straight nil
  :ensure nil
  :general
  (my/leader-keys
    "f d" 'dired
    "f j" 'dired-jump))

(use-package dired-single
  :after dired
  :general
  (dired-mode-map
   :states 'normal
   "h" 'dired-single-up-directory
   "l" 'dired-single-buffer
   "q" 'quit-window))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package org
  :hook ((org-mode . my/org-mode-setup)
         (org-mode . (lambda () (add-hook 'after-save-hook #'my/org-babel-tangle-config))))
  :general
  (my/leader-keys
    "C" '(org-capture :wk "capture"))
  (org-mode-map "z i" '(org-toggle-inline-images :wk "inline images"))
  :init
  (setq org-directory "~/Dropbox/org"
        org-image-actual-width nil
        +org-export-directory "~/Dropbox/org/export"
        org-default-notes-file "~/Dropbox/org/personal/tasks/todo.org"
        org-id-locations-file "~/Dropbox/org/.orgids"
        org-agenda-files '("~/dropbox/org/personal/tasks/birthdays.org" "~/dropbox/org/personal/tasks/todo.org" "~/dropbox/Notes/Test.inbox.org")
        ;; org-export-in-background t
        org-catch-invisible-edits 'smart)

;; disable modules for faster startup
(setq org-modules
  '(;; ol-w3m
    ;; ol-bbdb
    ;; ol-bibtex
    ol-docview
    ;; ol-gnus
    ;; ol-info
    ;; ol-irc
    ;; ol-mhe
    ;; ol-rmail
    ;; ol-eww
    ))
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
  (defun my/org-mode-setup ()
    (org-indent-mode)
    (variable-pitch-mode 1)
    (visual-line-mode 1))

  (defun my/org-babel-tangle-config ()
    (when (string-equal (file-name-directory (buffer-file-name))
                        (expand-file-name user-emacs-directory))
      ;; Dynamic scoping to the rescue
      (let ((org-confirm-babel-evaluate nil))
        (org-babel-tangle))))

  :config
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)

  ;; (efs/org-font-setup)
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("clj" . "src clojure"))
  )



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
          ("STRT"  . "#A020F0")
          ("PROJ"   . "#1E90FF")))
  )

(use-package org
  :general
  (my/local-leader-keys
    :keymaps 'org-mode-map
    "," '(org-edit-special :wk "edit")
    "-" '(org-babel-demarcate-block :wk "split block"))
  (my/local-leader-keys
    :keymaps 'org-src-mode-map
    "," '(org-edit-src-exit :wk "exit")) ;;FIXME
  :init
  (setq org-confirm-babel-evaluate nil))

;; enable mermaid diagram blocks
;; (use-package ob-mermaid
;;   :custom (ob-mermaid-cli-path "~/.asdf/shims/mmdc"))

(use-package ob-async
  :hook (org-load . (lambda () (require 'ob-async)))
  :init
  (setq ob-async-no-async-languages-alist '("jupyter-python" "jupyter-R" "jupyter-julia")))

(use-package ox-gfm
  :config (eval-after-load "org" '(require 'ox-gfm nil t)))

;; (use-package ox-ipynb
;;   :config (eval-after-load "org" '(require 'ox-ipynb)))

(use-package org-tree-slide
  :hook ((org-tree-slide-play . (lambda () (+remap-faces-at-start-present)))
         (org-tree-slide-stop . (lambda () (+remap-faces-at-stop-present))))
  :general
  (org-tree-slide-mode-map "C-j" 'org-tree-slide-move-next-tree)
  (org-tree-slide-mode-map "C-j" 'org-tree-slide-move-previous-tree)
  :init
  (defun +remap-faces-at-start-present ()
    (setq-local face-remapping-alist '((default (:height 2.0) variable-pitch)
                                       (org-verbatim (:height 1.75) org-verbatim)
                                       (org-block (:height 1.25) org-block)))
    (hide-mode-line-mode 1)
    (centaur-tabs-mode 0))
  (defun +remap-faces-at-start-present-term ()
    (interactive)
    (setq-local face-remapping-alist '((default (:height 2.0) variable-pitch)
                                       (org-verbatim (:height 1.75) org-verbatim)
                                       (org-block (:height 1.25) org-block))))
  (defun +remap-faces-at-stop-present ()
    (setq-local face-remapping-alist '((default variable-pitch default)))
    (hide-mode-line-mode 0)
    (centaur-tabs-mode 1))
  (setq org-tree-slide-skip-outline-level 0
        org-tree-slide-modeline-display nil
        org-tree-slide-slide-in-effect nil)
  :config
  (org-tree-slide-presentation-profile)
  )
