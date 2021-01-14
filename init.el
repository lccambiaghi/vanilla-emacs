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

(setq use-package-compute-statistics t)

(setq inhibit-startup-screen t
      default-fill-column 80
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

;; auto-close parentheses
(electric-pair-mode +1)
;; disable auto pairing for <
(add-function :before-until electric-pair-inhibit-predicate
              (lambda (c) (eq c ?<)))

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

;; Main typeface
;; (set-face-attribute 'default nil :family "DejaVu Sans Mono" :height 110)
;; Proportionately spaced typeface
;; (set-face-attribute 'variable-pitch nil :family "DejaVu Serif" :height 1.0)
;; Monospaced typeface
;; (set-face-attribute 'fixed-pitch nil :family "DejaVu Sans Mono" :height 1.0)

;; use a font I like, but fail gracefully if it isn't available
(ignore-errors (set-frame-font "Fira Code Retina 18"))

;; enable winner mode globally for undo/redo window layout changes
(winner-mode t)

;; clean up the mode line
(display-time-mode -1)
;; (setq-default mode-line-format nil) ;TODO
(setq column-number-mode t)

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
    "`" '(switch-to-prev-buffer :which-key "prev buffer")
    ";" '(eval-expression :which-key "eval sexp")

    "b" '(:ignore t :which-key "buffer")
    "br"  'revert-buffer
    "bd"  'kill-current-buffer
    "bs" '((lambda () (interactive) (pop-to-buffer "*scratch*")) :wk "scratch")

    "c" '(:ignore t :which-key "code")

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

    "o" '(:ignore t :which-key "org")

    "p" '(:ignore t :which-key "project")

    "s" '(:ignore t :which-key "search")

    "t"  '(:ignore t :which-key "toggle")
    "t d"  '(toggle-debug-on-error :which-key "debug on error")
    "t v" '((lambda () (interactive) (visual-line-mode 'toggle)) :wk "visual line")

    "w" '(:ignore t :which-key "window")
    "wl"  'windmove-right
    "wh"  'windmove-left
    "wk"  'windmove-up
    "wj"  'windmove-down
    "wr" 'winner-redo
    "wd"  'delete-window
    "wu" 'winner-undo
    "wr" 'winner-redo
    "wm"  '(delete-other-windows :wk "maximize")
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
  ;; (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  ;; (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

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
  :init
  (setq evil-goggles-duration 0.05)
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
  :demand
  :init
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-env-enable-python nil)
  (setq doom-modeline-height 15)
  :config
  (doom-modeline-mode 1))

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq ns-use-proxy-icon  nil)
(setq frame-title-format nil)

(use-package modus-themes
  :straight (modus-themes :type git :host gitlab :repo "protesilaos/modus-themes" :branch "main")
  :hook (emacs-startup . my/load-modus-theme)
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
        modus-themes-completions 'nil ; {nil,'moderate,'opinionated}
        modus-themes-diffs nil ; {nil,'desaturated,'fg-only}
        modus-themes-org-blocks 'greyscale ; {nil,'greyscale,'rainbow}
        modus-themes-headings  ; Read further below in the manual for this one
        '((1 . line)
          (t . rainbow-line-no-bold))
        modus-themes-variable-pitch-headings t
        modus-themes-scale-headings t
        modus-themes-scale-1 1.1
        modus-themes-scale-2 1.15
        modus-themes-scale-3 1.21
        modus-themes-scale-4 1.27
        modus-themes-scale-5 1.33)
  (defun my/load-modus-theme ()
    ;;Light for the day
    (run-at-time "07:00" (* 60 60 24)
                 (lambda () (modus-themes-load-operandi)))
    ;; Dark for the night
    (run-at-time "00:00" (* 60 60 24)
                 (lambda () (modus-themes-load-vivendi)))
    (run-at-time "15:00" (* 60 60 24)
                 (lambda () (modus-themes-load-vivendi)))
    ))

(use-package dashboard
  :after projectile
  :demand
  :init
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (setq dashboard-center-content t)
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          ;; (agenda . 5)
                          ))
  ;; (setq dashboard-startup-banner [VALUE])
  :config
  (dashboard-setup-startup-hook))

(use-package centaur-tabs
  :hook (emacs-startup . centaur-tabs-mode)
  :general
  (general-nvmap "gt" 'centaur-tabs-forward)
  (general-nvmap "gT" 'centaur-tabs-backward)
  :init
  (setq centaur-tabs-set-icons t)
  (setq ccentaur-tabs-set-modified-marker t
        centaur-tabs-modified-marker "M"
        centaur-tabs-cycle-scope 'tabs)
  (setq centaur-tabs-set-close-button nil)
  :config
  (centaur-tabs-mode t)
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
    "w t" '(transpose-frame "transpose")
    "w f" '(rotate-frame "flip")))

(use-package persistent-scratch
:demand
:config
(persistent-scratch-setup-default))

(use-package olivetti
  :general
  (my/leader-keys
    "t o" '(olivetti-mode :wk "olivetti"))
  :init
  (setq olivetti-body-width 0.7)
  (setq olivetti-minimum-body-width 80)
  (setq olivetti-recall-visual-line-mode-entry-state t))

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
  :general
  (general-nmap "C-e" #'embark-act)
  ;; (selectrum-minibuffer-map "C-o" #'embark-act)
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
  (setq projectile-project-root-files '("Dockerfile" "pyproject.toml" "project.clj" "deps.edn"))
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
    "g G" 'magit-status-here
    "g l" '(magit-log :wk "log"))
  :init
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (setq magit-log-arguments '("--graph" "--decorate" "--color")))

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
  :hook
  (((text-mode org-mode prog-mode) . diff-hl-mode)
   (magit-pre-refresh . diff-hl-magit-pre-refresh)
   (magit-post-refresh . diff-hl-magit-post-refresh))
  :init
  (setq diff-hl-draw-borders nil)
  )

(use-package smerge-mode
  :straight (:type built-in)
  :after hydra
  :general
  (my/leader-keys "g m" 'hydra-smerge)
  :init
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

(use-package hydra)

;; use common convention for indentation by default
(setq-default indent-tabs-mode t)
(setq-default tab-width 2)

;; use a reasonable line length
(setq-default fill-column 120)


;; add a visual intent guide
(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  ;; :custom
  ;; (highlight-indent-guides-method 'character)
  ;; (highlight-indent-guides-character ?|)
  ;; (highlight-indent-guides-responsive 'stack)
  )

(use-package rainbow-delimiters
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
         (clojure-mode . rainbow-delimiters-mode)
         (org-mode . rainbow-delimiters-mode)))

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
  :general
  (company-active-map
   "TAB"       nil    ;; interferes with yasnippet
   [tab]       nil)
  :init
  (setq company-backends '((company-capf :with company-yasnippet)
                           (company-keywords company-files)))
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.0)
  ;; always show candidates in overlay tooltip
  (setq company-frontends '(company-pseudo-tooltip-frontend))
  ;; don't fill the only candidate
  (setq company-auto-complete nil
        company-auto-complete-chars nil)
  :config
  (global-company-mode)
  )

(use-package envrc
  :hook ((python-mode . envrc-mode)
         (org-mode . envrc-mode)))

(use-package yasnippet
  :hook
  ((text-mode . yas-minor-mode)
   (prog-mode . yas-minor-mode)
   (org-mode . yas-minor-mode)))

(use-package evil-mc
  :general
  (general-nmap "gz" #'mc-hydra/body)
  :config
  (defhydra mc-hydra (:color pink :hint nil
                             :pre (evil-mc-pause-cursors))
    "
^Match^            ^Line-wise^           ^Manual^
^^^^^^----------------------------------------------------
_Z_: match all     _J_: make & go down   _R_: remove all
_m_: make & next   _K_: make & go up     
_M_: make & prev   ^ ^                   
_n_: skip & next   ^ ^                   
_N_: skip & prev

Current pattern: %`evil-mc-pattern

"
    ("Z" #'evil-mc-make-all-cursors)
    ("m" #'evil-mc-make-and-goto-next-match)
    ("M" #'evil-mc-make-and-goto-prev-match)
    ("n" #'evil-mc-skip-and-goto-next-match)
    ("N" #'evil-mc-skip-and-goto-prev-match)
    ("J" #'evil-mc-make-cursor-move-next-line)
    ("K" #'evil-mc-make-cursor-move-prev-line)
    ("R" #'evil-mc-undo-all-cursors)
    ("q" #'evil-mc-resume-cursors "quit" :color blue)
    ("<escape>" #'evil-mc-resume-cursors "quit" :color blue))
  (global-evil-mc-mode +1)
  )

(use-package evil-nerd-commenter
  :general
  (general-nmap "gcc" 'evilnc-comment-or-uncomment-lines)
  (general-vmap "gc" 'evilnc-comment-or-uncomment-lines)
  )

(use-package evil-surround
  :general
  (:states 'operator
   "s" 'evil-surround-edit
   "S" 'evil-Surround-edit)
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

(use-package dired
  :straight (:type built-in)
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
         (org-mode . prettify-symbols-mode)
         (org-mode . (lambda () (add-hook 'after-save-hook #'my/org-babel-tangle-config))))
  :general
  (my/leader-keys
    "o c" '(org-capture :wk "capture")
    "o a" '(org-agenda-list :wk "agenda"))
  (my/local-leader-keys
    :keymaps 'org-mode-map
    "n" '(org-toggle-narrow-to-subtree :wk "narrow subtree")
    "t t" '(org-todo :wk "heading todo"))
  (org-mode-map
   :states '(normal)
   "z i" '(org-toggle-inline-images :wk "inline images"))
  :init
  ;; general settings
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
  (setq-default prettify-symbols-alist '(("#+BEGIN_SRC" . "»")
                                         ("#+END_SRC" . "«")
                                         ("#+begin_src" . "»")
                                         ("#+end_src" . "«")))
  (setq prettify-symbols-unprettify-at-point 'right-edge)
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
  ;; visual
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1)
  ;; org habit
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  ;; (efs/org-font-setup)
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("clj" . "src clojure"))
  (add-to-list 'org-structure-template-alist '("jp" . "src jupyter-python"))
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
  (setq org-confirm-babel-evaluate nil)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t))))

;; enable mermaid diagram blocks
;; (use-package ob-mermaid
;;   :custom (ob-mermaid-cli-path "~/.asdf/shims/mmdc"))

(use-package ob-async
  :hook (org-load . (lambda () (require 'ob-async)))
  :init
  (setq ob-async-no-async-languages-alist '("jupyter-python" "jupyter-R" "jupyter-julia")))

(use-package jupyter
  :straight (:no-native-compile t :no-byte-compile t) ;; otherwise we get jupyter-channel void
  :hook ((envrc-mode . my/load-ob-jupyter)
         (jupyter-repl-persistent-mode . (lambda ()  ;; we activate org-interaction-mode ourselves
                                           (when (derived-mode-p 'org-mode)
                                             (jupyter-org-interaction-mode)))))
  :init
  (setq org-babel-default-header-args:jupyter-python '((:async . "yes")
                                                       (:pandoc t)
                                                       (:kernel . "python3")))
  (setq org-babel-default-header-args:jupyter-R '((:pandoc t)
                                                  (:async . "yes")
                                                  (:kernel . "ir")))
  (defun my/load-ob-jupyter ()
    ;; only try to load in org-mode
    (when (derived-mode-p 'org-mode)
      ;; skip if already loaded
      (unless (member '(jupyter . t) org-babel-load-languages)
        ;; only load if jupyter is available
        (when (executable-find "jupyter")
          (org-babel-do-load-languages 'org-babel-load-languages
                                       (append org-babel-load-languages
                                               '((jupyter . t))))))))
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

(use-package ox-gfm
  :after org)

(use-package ox-ipynb
  :straight (ox-ipynb :type git :host github :repo "jkitchin/ox-ipynb")
  :after org)

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
  (defun +remap-faces-at-start-present ()
    ;; (setq-local face-remapping-alist '((default (:height 1.50) variable-pitch)
    ;;                                    (org-verbatim (:height 1.35) org-verbatim)
    ;;                                    (org-block (:height 1.25) org-block)))
    (hide-mode-line-mode 1)
    (diff-hl-mode 0)
    (centaur-tabs-mode 0))
  ;; TODO maybe also enable olivetti mode?
  (defun +remap-faces-at-start-present-term ()
    (interactive)
    (setq-local face-remapping-alist '((default (:height 1.50) variable-pitch)
                                       (org-verbatim (:height 1.35) org-verbatim)
                                       (org-block (:height 1.25) org-block))))
  (defun +remap-faces-at-stop-present ()
    (setq-local face-remapping-alist '((default variable-pitch default)))
    (hide-mode-line-mode 0)
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
  )

(use-package evil-org-mode
  :straight (evil-org-mode :type git :host github :repo "hlissner/evil-org-mode")
  :hook (org-mode . evil-org-mode)
  :general
  (general-nmap
    :keymaps '(evil-org-mode-map org-mode-map )
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
           (+org--toggle-inline-images-in-subtree beg end)
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
            (org-element-property :end context)))))))

  :config
  (add-hook 'evil-org-mode-hook #'evil-normalize-keymaps)
  )

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . (lambda ()
                      (setq-local evil-lookup-func #'lsp-describe-thing-at-point)))
  :general
  (my/leader-keys
    "c" '(:keymap lsp-command-map))
  (my/local-leader-keys
    :keymaps 'lsp-mode-map
    "r" '(lsp-rename :wk "rename")
    "i" '(:ignore t :which-key "import")
    "i o" '(lsp-rename :wk "optimize"))
  (lsp-mode-map
   :states '(normal)
   "gd" 'lsp-find-definition
   "gD" 'lsp-find-references)
  :init
  (setq lsp-restart 'ignore)
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-enable-file-watchers nil)
  (setq lsp-signature-auto-activate nil)
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook ((lsp-mode . lsp-ui-mode))
  :init
  (setq lsp-ui-doc-show-with-cursor nil)
  (setq lsp-ui-doc-show-with-mouse nil)
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
    "d e" '(dap-eval-thing-at-point :wk "eval")
    "d i" '(dap-step-in :wk "step in")
    "d q" '(dap-disconnect :wk "quit")
    "d r" '(dap-ui-repl :wk "repl")
    "d h" '(dap-hydra :wk "hydra"))
  :init
  (setq dap-auto-configure-features '(locals repl))
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
        `((,dap-ui--locals-buffer . ((side . right) (slot . 1) (window-width . 0.30)))
          ;; (,dap-ui--breakpoints-buffer . ((side . left) (slot . 1) (window-width . ,treemacs-width)))
          ;; (,dap-ui--sessions-buffer . ((side . left) (slot . 2) (window-width . ,treemacs-width)))
          (,dap-ui--repl-buffer . ((side . right) (slot . 2) (window-width . 0.30)))))
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
  ;; :init
  ;; (defun my/ipython-use-venv (orig-fun &rest args)
  ;;   (when (getenv "VIRTUAL_ENV")
  ;;     (when-let ((python-shell-interpreter (executable-find "ipython")))
  ;;       (apply orig-fun args)))
  ;;   (apply orig-fun args))
  ;; (advice-add 'run-python :around #'my/ipython-use-venv)
  :hook (envrc-mode . (lambda ()
                        (when (executable-find "ipython")
                          (setq python-shell-interpreter (executable-find "ipython")))))
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
  (setq lsp-pyright-typechecking-mode "off") ;; too much noise in "real" projects
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))

(use-package python-pytest
  :general
  (my/local-leader-keys
    :keymaps 'python-mode-map
    "t t" '(python-pytest-dispatch :wk "dispatch")
    "t d" '(python-pytest-function :wk "defun"))
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
    "e b" '((call-interactively 'my/jupyter-eval-buffer) :wk "buffer"))
  (my/local-leader-keys
    :keymaps 'jupyter-repl-interaction-mode-map
    "k r" '(jupyter-repl-restart-kernel :wk "restart kernel"))
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
    :keymaps '(org-mode-map emacs-lisp-mode-map)
    "e l" '(eval-last-sexp :wk "last sexp")
    ;; "e" '(eval-last-sexp :states 'visual :wk "sexp")
    )
  (my/local-leader-keys
    :keymaps '(org-mode-map emacs-lisp-mode-map)
    :states 'visual
    "e" '(eval-last-sexp :wk "sexp"))
  )

(use-package evil-lisp-state
  :after evil
  :demand
  :init
  ;; (setq evil-lisp-state-enter-lisp-state-on-command nil)
  (setq evil-lisp-state-global t)
  (setq evil-lisp-state-major-modes '(emacs-lisp-mode clojure-mode))
  :config
  (evil-lisp-state-leader "SPC l")
  )

(use-package nix-mode
:commands (nix-mode) ;;FIXME
:mode "\\.nix\\'")

(use-package clojure-mode
  :mode "\\.clj$"
  :init
  (setq clojure-align-forms-automatically t))

(use-package cider
  :hook ((cider-repl-mode . evil-normalize-keymaps)
         (cider-mode . eldoc-mode))
  :general
  (my/local-leader-keys
    :keymaps 'clojure-mode-map
    "'" '(cider-jack-in :wk "jack in")
    "e l" 'cider-eval-last-sexp
    "e E" 'cider-pprint-eval-last-sexp-to-comment
    "e d" '(cider-eval-defun-at-point :wk "defun")
    "e D" 'cider-pprint-eval-defun-to-comment)
  (my/local-leader-keys
    :keymaps 'clojure-mode-map
    :states 'visual
    "e" 'cider-eval-region)
  :init
  (setq nrepl-hide-special-buffers t)
  (setq nrepl-sync-request-timeout nil)
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
