(setq gc-cons-threshold (* 50 1000 1000))

;; if not using dashboard
(setq inhibit-startup-message t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)

(menu-bar-mode -1)

(setq visible-bell nil)

(defalias 'yes-or-no-p 'y-or-n-p)

(dolist (mode '(org-mode-hook
                term-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))

(global-visual-line-mode)
(setq require-final-newline t)

(require 'package)

(setq package-archives '(("melpa" ."https://melpa.org/packages/")
                         ("org" . "https:/orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq warning-suppress-types '((use-package) (use-package)))
(setq use-package-always-ensure t)
;; (setq use-package-always-defer t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(set-language-environment "UTF-8")
;; (set-face-bold-p 'bold nil)
;; (set-face-attribute 'default nil
;; 		     :font "Iosevka:antialias=true"
;; 		     :height 90)
;; (set-face-bold-p 'bold nil)

(custom-set-faces
 '(default ((t (:inherit nil :height 100 :family "JetBrains Mono")))))

(use-package no-littering)

(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(defun basqs/evil-hook ()
  (dolist (mode '(custom-mode
                  vterm-mode))
    (add-to-list 'evil-emacs-state-modes mode)))

(use-package evil
  :defer nil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-tree)
  :config
  (add-hook 'evil-mode-hook 'basqs/evil-hook)
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  ;; (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  ;; (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-global-set-key 'motion "j" 'evil-next-line)
  (evil-global-set-key 'motion "k" 'evil-previous-line)
  (evil-global-set-key 'motion ";" 'ace-jump-mode)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :diminish
  :defer nil
  :init
  (setq evil-collection-company-use-tng nil)  ;; Is this a bug in evil-collection?
  :custom
  (evil-collection-outline-bind-tab-p nil)
  :config
  (evil-collection-init))

(use-package undo-tree
  :after evil
  :diminish
  :defer nil
  :init
  (global-undo-tree-mode 1))

(use-package evil-nerd-commenter
  :defer nil
  :diminish
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package evil-leader
  :defer nil
  :diminish
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    ;; General
    ".q" 'delete-frame
    "c" 'kill-buffer-and-window
    "e" 'eshell-toggle
    ;; Undo
    "uv" 'undo-tree-visualize
    "uu" 'undo-tree-undo
    "ur" 'undo-tree-redo
    ;; Files
    "d" 'dired
    ;; Bufffers
    "wc" 'evil-window-delete
    "ws" 'evil-window-split
    "wv" 'evil-window-vsplit
    "wl"  'evil-window-next
    "wh"  'evil-window-prev
    ;; Org mode
    "oc" 'org-edit-special
    "ol" 'org-latex-previw
    "ot" 'org-ctrl-c-ctrl-c
    "oi" 'org-toggle-inline-images
    "oa" 'org-agenda
    "os" 'org-schedule
    "o." 'org-toggle-checkbox
    ;; Export
    "oep" 'org-latex-export-to-pdf
    "oeh" 'org-html-export-to-html
    ;; Roam
    "orf" 'org-roam-node-find
    "ori" 'org-roam-node-insert
    "oru" 'org-roam-db-sync
    ;; Babel
    "obs" 'org-babel-execute-src-block
    "obb" 'org-babel-execute-buffer
    "obl" 'org-babel-load-file
    "obt" 'org-babel-tangle
    ;; Help
    "hh" 'help
    "hk" 'helpful-key
    "hv" 'helpful-variable
    "hf" 'helpful-function
    "hs" 'helpful-symbol
    "hm" 'describe-mode
    ;; Magit
    "gs" 'magit-status
    "gs"  'magit-status
    "gd"  'magit-diff-unstaged
    "gc"  'magit-commit
    "glc" 'magit-log-current
    "glf" 'magit-log-buffer-file
    "gb"  'magit-branch
    "gP"  'magit-push-current
    "gp"  'magit-pull-branch
    "gf"  'magit-fetch
    "gF"  'magit-fetch-all
    "gr"  'magit-rebase))

(use-package vertico
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("C-f" . vertico-exit)
              :map minibuffer-local-map
              ("M-h" . backward-kill-word))
  :custom
  (vertico-cicle t)
  :init
  (vertico-mode))

(use-package savehist
  :diminish
  :init (savehist-mode))

(use-package which-key
  :defer 0
  :diminish
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3))

(use-package helpful
  :diminish
  :commands helpful-mode)

(use-package tldr)

(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c b" . consult-bookmark)
         ("C-c k" . consult-kmacro)
         ;; C bindings
         ("C-s" . consult-line)
         ;; C-x bindings (ctl-x-map)
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s F" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi))           ;; needed by consult-line to detect isearch

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI. You may want to also
  ;; enable `consult-preview-at-point-mode` in Embark Collect buffers.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Optionally replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   :preview-key (kbd "M-."))

  (setq consult-narrow-key "<") ;; (kbd "C-+")

  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project))))))
(use-package orderless
  :diminish
  :ensure t
  :custom (completion-styles '(orderless)))

(use-package eshell-toggle
  :bind ("C-M-'" . eshell-toggle)
  :custom
  (eshell-toggle-size-fraction 3)
  (eshell-toggle-use-projectile-root t)
  (eshell-toggle-run-command nil)
  (eshell-directory-name "~/.emacs.d/eshell")
  (eshell-aliases-file (expand-file-name "~/.emacs.d/eshell/alias")))

(setq eshell-prompt-regexp "^[^#$]*:$# ")

(defun configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; Bind some useful keys for evil-mode
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  (evil-normalize-keymaps)

  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell
  :hook (eshell-first-time-mode . configure-eshell)
  :config

  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim"))))

(add-hook 'eshell-pre-command-hook
          (lambda () (setenv "TERM" "xterm-256color")))


;; (use-package eshell-vterm)

;; (use-package eshell-syntax-highlighting
;;   :after eshell-mode
;;   :config
;;   (eshell-syntax-highlighting-global-mode +1))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first")
           (setq dired-omit-files "^\\.[^.].*"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    (evil-collection-define-key 'normal 'dired-mode-map
      "h" 'dired-single-up-directory
      "l" 'dired-single-buffer
      "y" 'dired-ranger-copy
      "X" 'dired-ranger-move
      "p" 'dired-ranger-paste)))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-git)
(use-package dired-ranger)

(use-package dired-imenu
  :after dired)

(use-package dtrt-indent
  :commands dtrt-indent-mode
  :demand
  :config (dtrt-indent-mode 1))


(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(use-package perspective
  :bind ("C-c p c" . persp-switch)
  ("C-c p n" . persp-next)
  ;; ("C-x k" persp-kill-buffer*)
  :custom (persp-initial-frame-name "Main")
  :config (persp-mode))
;; Running `persp-mode' multiple times resets the perspective list...
;; (unless (equal persp-mode t)
;;   (persp-mode)))

(use-package olivetti
  :bind ("C-c o" . olivetti-mode))

(use-package treemacs
  :init 
  (with-eval-after-load 'treemacs
    (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action)
    (treemacs-toggle-show-dotfiles)
    )
  )

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-all-the-icons
  :ensure t)
(treemacs-load-theme "all-the-icons")

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(defun dw/switch-project-action ()
  "Switch to a workspace with the project name and start `magit-status'."
  ;; TODO: Switch to EXWM workspace 1?
  (persp-switch (projectile-project-name))
  (magit-status))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :demand t
  :init
  (projectile-mode +1)
  (when (file-directory-p "~/Projects/")
    (setq projectile-project-search-path '("~/Projects/")))
  (setq projectile-switch-project-action #'switch-project-action))

;; (use-package aggressive-indent
;;   :hook ((emacs-lisp-mode
;;           inferior-emacs-lisp-mode
;;           ielm-mode
;;           lisp-mode
;;           inferior-lisp-mode
;;           isp-interaction-mode
;;           slime-repl-mode) . aggressive-indent-mode))

(use-package magit
  :bind ("C-M-;" . magit-status)
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package ledger-mode
  :custom
  (ledger-reports '(("bal" "%(binary) -f %(ledger-file) bal")
                    ("bal this quarter" "%(binary) -f %(ledger-file) --period \"this quarter\" bal")
                    ("bal last quarter" "%(binary) -f %(ledger-file) --period \"last quarter\" bal")
                    ("reg" "%(binary) -f %(ledger-file) reg")
                    ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
                    ("account" "%(binary) -f %(ledger-file) reg %(account)"))))

(use-package hledger-mode :straight t)

(use-package embark
  :straight t
  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :straight t)

(use-package ace-window
  :straight t)

(global-set-key (kbd "M-o") 'ace-window)
(setq aw-dispatch-always t)

(use-package 0x0
  :straight t)

(use-package crux
  :bind (("C-c D" . crux-delete-file-and-buffer)))

;;  (use-package webkit)
;;  :bind ("s-b" 'webkit)) ;; Bind to whatever global key binding you want if you want
;;  (use-package 'webkit-ace) ;; If you want link hinting
;;  (use-package 'webkit-dark) ;; If you want to use the simple dark mode
;;  (straight-use-package
;;   '(webkit :type git :host github :repo "akirakyle/emacs-webkit"
;;            :branch "main"))


;; (setq browse-url-browser-function 'xwidget-webkit-browse-url)

(setq ispell-dictionary "pt_BR")
(setq ispell-program-name "hunspell")

(use-package flymake
  :diminish
  :hook (lsp-mode)
  :ensure t)
(add-hook 'after-init-hook #'global-flymake-mode)

(use-package writeroom-mode)

(use-package daemons)

(use-package symon)

(use-package esup
  :ensure t
  :pin melpa)

(use-package bug-hunter)

(setq-default tab-width 2)
(setq-default evil-shift-width tab-width)

(use-package sudo-edit
  :bind (("C-c C-r" . sudo-edit)))

(use-package dashboard
  :ensure t
  :defer nil
  :preface
  (defun create-scratch-buffer ()
    "Create a scratch buffer"
    (interactive)
    (switch-to-buffer (get-buffer-create "*scratch*"))
    (lisp-interaction-mode))
  :config (dashboard-setup-startup-hook)
                                        ;      :bind (("C-z d" . open-dashboard))
  )

(setq dashboard-projects-switch-function 'projectile-switch-project)
(setq dashboard-banner-logo-title "")
(setq dashboard-startup-banner "~/.emacs.d/etc/emacs.png")
(setq dashboard-init-info (format "%d packages loaded in %s"
                                  (length package-activated-list) (emacs-init-time)))
(setq dashboard-center-content t)
(setq dashboard-set-navigator t)
(setq dashboard-show-shortcuts t)

(setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (agenda . 10)))


(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)

(setq dashboard-set-navigator t)
(setq dashboard-navigator-buttons
      `(;; line1
        ((,nil
          "elfeed"
          "opens elfeed"
          (lambda (&rest _) (elfeed-load-db-and-open))
          'default)
         (nil
          "open the emacs.org"
          "Opens the config file"
          (lambda (&rest _) (find-file "~/.emacs.d/emacs.org"))
          'default)
         (nil
          "new scratch buffer"
          "Opens a scratch buffer"
          (lambda (&rest _) (create-scratch-buffer))
          'default)
         )))

(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

(use-package all-the-icons)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package smartparens
  :hook (prog-mode . smartparens-mode))

(use-package diminish)

;; (use-package doom-modeline
;;   :hook (after-init . doom-modeline-mode)
;;   :custom (setq doom-modeline-height 20
;;                 doom-modeline-bar-width 6
;;                 doom-modeline-lsp t
;;                 setq doom-modeline-buffer-encoding nil
;;                 doom-modeline-github t
;;                 doom-modeline-mu4e nil
;;                 doom-modeline-irc t
;;                 doom-modeline-minor-modes t
;;                 doom-modeline-major-mode-icon t)
;;   (custom-set-faces '(mode-line ((t (:height 0.85))))
;;                     '(mode-line-inactive ((t (:height 0.85)))))
;; (use-package minions
;;   (:hook doom-modeline-mode)))

(use-package nano-modeline)
  (nano-modeline-mode)

(use-package mode-line-idle

  :commands (mode-line-idle))
(setq-default mode-line-format)

(use-package doom-themes)

(straight-use-package
 '(alduin-emacs :type git :host github :repo "tysteiman/alduin-emacs"))

(consult-theme 'doom-tomorrow-night)

(defun org-mode-setup ()
  (org-indent-mode)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq org-hide-emphasis-markers t)
  (setq truncate-lines t)
  (setq evil-auto-indent nil)
  (diminish org-indent-mode))

(defun side-padding ()
  (lambda () (progn
               (setq left-margin-width 2)
               (setq right-margin-width 2)
               (set-window-buffer nil (current-buffer)))))

(defun org-toggle-todo-and-fold ()
  (interactive)
  (save-excursion
    (org-back-to-heading t) ;; Make sure command works even if point is
    ;; below target heading
    (cond ((looking-at "\*+ TODO")
           (org-todo "DONE")
           (hide-subtree))
          ((looking-at "\*+ DONE")
           (org-todo "TODO")
           (hide-subtree))
          (t (message "Can only toggle between TODO and DONE.")))))

;; (define-key org-mode-map (kbd "C-c C-d") 'org-toggle-todo-and-fold)


(use-package org
  :defer nil
  :hook (org-mode . org-mode-setup))


(setq org-ellipsis " ▾")
(setq org-agenda-files '("~/Documents/org/org-agenda.org"))


(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp .t)
   (shell . t)))

(use-package org-evil
  :defer nil)

(use-package org-pomodoro
  :bind (("C-c p s" . org-timer-set-timer)
         ("C-c p p" . org-timer-pause-or-continue)))

(use-package org-bullets
  :defer nil
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "•" "◆" "○" "●" "◆")))

(let* ((base-font-color     (face-foreground 'default nil 'default))
       (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

  (custom-theme-set-faces 'user
                          `(org-level-8 ((t (,@headline ))))
                          `(org-level-7 ((t (,@headline ))))
                          `(org-level-6 ((t (,@headline ))))
                          `(org-level-5 ((t (,@headline ))))
                          `(org-level-4 ((t (,@headline , :height 1.1))))
                          `(org-level-3 ((t (,@headline , :height 1.25))))
                          `(org-level-2 ((t (,@headline , :height 1.5))))
                          `(org-level-1 ((t (,@headline , :height 1.75))))
                          `(org-document-title ((t (,@headline , :height 1.5 :underline nil))))))

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
        (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src sh"))
(add-to-list 'org-structure-template-alist '("tex" . "src latex"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))

(setq org-confirm-babel-evaluate nil)

(require 'org-habit)
(add-to-list 'org-modules 'org-habit)
(setq org-habit-graph-column 60)

(use-package org-super-agenda
  :bind (("C-c a" . org-agenda))
  :config (let ((org-super-agenda-groups
                 '(;; Each group has an implicit boolean OR operator between its selectors.
                   (:name "Today"  ; Optionally specify section name
                          :time-grid t  ; Items that appear on the time grid
                          :todo "TODAY")  ; Items that have this TODO keyword
                   (:name "Important"
                          ;; Single arguments given alone
                          :tag "bills"
                          :priority "A")
                   ;; Set order of multiple groups at once
                   (:order-multi (2 (:name "Shopping in town"
                                           ;; Boolean AND group matches items that match all subgroups
                                           :and (:tag "shopping" :tag "@town"))
                                    (:name "Food-related"
                                           ;; Multiple args given in list with implicit OR
                                           :tag ("food" "dinner"))
                                    (:name "Personal"
                                           :habit t
                                           :tag "personal")
                                    (:name "Space-related (non-moon-or-planet-related)"
                                           ;; Regexps match case-insensitively on the entire entry
                                           :and (:regexp ("space" "NASA")
                                                         ;; Boolean NOT also has implicit OR between selectors
                                                         :not (:regexp "moon" :tag "planet")))))
                   ;; Groups supply their own section names when none are given
                   (:todo "WAITING" :order 8)  ; Set order of this section
                   (:todo ("SOMEDAY" "TO-READ" "CHECK" "TO-WATCH" "WATCHING")
                          ;; Show this group at the end of the agenda (since it has the
                          ;; highest number). If you specified this group last, items
                          ;; with these todo keywords that e.g. have priority A would be
                          ;; displayed in that group instead, because items are grouped
                          ;; out in the order the groups are listed.
                          :order 9)
                   (:priority<= "B"
                                ;; Show this section after "Today" and "Important", because
                                ;; their order is unspecified, defaulting to 0. Sections
                                ;; are displayed lowest-number-first.
                                :order 1)
                   ;; After the last group, the agenda will display items that didn't
                   ;; match any of these groups, with the default order position of 99
                   )))
            (org-agenda nil "a")))

(use-package org-journal
  :config (setq org-journal-dir "~/Documents/org/journal/")
  :bind (("C-c j n" . org-journal-new-entry)
         ("C-c j s" . org-journal-search)))

(use-package org-ql)

(use-package ox-reveal)

(use-package pandoc)
(use-package ox-pandoc)

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/Documents/org/roam")
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n d n" . org-roam-dailies-capture-today))
  :config (org-roam-setup))
(setq org-roam-v2-ack t)
(setq org-roam-dailies-directory "journal/")

(defun org/prettify-set ()
  (interactive)
  (setq prettify-symbols-alist
        '(("#+begin_src" . "→")
          ("#+BEGIN_SRC" . "→")
          ("#+end_src" . "←")
          ("#+END_SRC" . "←")
          ("#+begin_example" . "")
          ("#+BEGIN_EXAMPLE" . "")
          ("#+end_example" . "")
          ("#+END_EXAMPLE" . "")
          ("#+results:" . "")
          ("#+RESULTS:" . "")
          ("[ ]" . "☐")
          ("[-]" . "◯")
          ("[X]" . "☑"))))
(add-hook 'org-mode-hook 'org/prettify-set)

(defun prog/prettify-set ()
  (interactive)
  (setq prettify-symbols-alist
        '(("->" . "→")
          ("<-" . "←")
          ("<=" . "≤")
          (">=" . "≥")
          ("!=" . "≠")
          ("~=" . "≃")
          ("=~" . "≃"))))
(add-hook 'lsp-mode'prog/prettify-set)

(global-prettify-symbols-mode)

(use-package lsp-mode
  :straight t
  :hook (typescript-mode js2-mode web-mode)
  :bind
  ("TAB" . completion-at-point)
  ("C-c l d" . xref-find-definitions)
  ("C-c l r" . xref-find-references)
  ("C-c l n" . lsp-ui-find-next-reference)
  ("C-c l p" . lsp-ui-find-prev-reference)
  ("C-c l s" . counsel-imenu)
  ("C-c l e" . lsp-ui-flycheck-list)
  ("C-c l S" . lsp-ui-sideline-mode)
  ("C-c l X" . lsp-execute-code-action)
  :custom (lsp-headerline-breadcrumb-enable nil))


(use-package lsp-ui
  :straight t
  :hook (lsp-mode)
  :custom
  ((setq lsp-ui-sideline-enable t)
   (setq lsp-ui-sideline-show-hover nil)
   (setq lsp-ui-doc-position 'bottom)
   (lsp-ui-doc-show)))

(use-package lsp-treemacs
  :after lsp)

 (use-package dap-mode
   :straight t
   :custom (lsp-enable-dap-auto-configure nil)
   (dap-ui-mode 1)
   (dap-tooltip-mode 1)
   (dap-node-setup))

(use-package ccls
  :hook (lsp)
  :bind
  ("C-c c" . compile)
  :config

  (use-package irony
    :commands irony-mode
    :init (add-hooks '(((c++-mode c-mode objc-mode) . irony-mode))))

  (use-package c-eldoc
    :commands c-turn-on-eldoc-mode
    :init (add-hook 'c-mode-common-hook 'c-turn-on-eldoc-mode))

  (use-package irony-eldoc
    :commands irony-eldoc
    :init (add-hook 'irony-mode-hook 'irony-eldoc)))

;; (use-package go-mode
;;   :hook (lsp-deferred)

;;   (use-package flymake-go
;;     :hook (go-mode)))

(use-package python-mode
  :ensure t
  :hook (python-mode . lsp-deferred)
  :custom
  ;; NOTE: Set these if Python 3 is called "python3" on your system!
  ;; (python-shell-interpreter "python3")
  ;; (dap-python-executable "python3")
  (dap-python-debugger 'debugpy)
  :config
  (require 'dap-python))

(use-package pyvenv
  :config
  (pyvenv-mode 1))

(defun set-js-indentation ()
  (setq-default js-indent-level 2)
  (setq-default evil-shift-width js-indent-level)
  (setq-default tab-width 2))

(use-package js2-mode
  :custom
  ;; Use js2-mode for Node scripts
  (add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))

  ;; Don't use built-in syntax checking
  (setq js2-mode-show-strict-warnings nil)

  ;; Set up proper indentation in JavaScript and JSON files
  (add-hook 'js2-mode-hook #'set-js-indentation)
  (add-hook 'json-mode-hook #'set-js-indentation))

(use-package apheleia
  :custom (apheleia-global-mode +1))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package nix-sandbox)

(require 'lsp)
(add-to-list 'lsp-language-id-configuration '(nix-mode . "nix"))
(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection '("rnix-lsp"))
                  :major-modes '(nix-mode)
                  :server-id 'nix))

(setq company-format-margin-function nil)
 (add-hook 'after-init-hook 'global-company-mode)

  (add-hook 'c-mode-hook 'lsp)
  (add-hook 'js2-mode-hook 'lsp)
  (add-hook 'alchemist-mode-hook 'lsp)
  (add-hook 'go-mode-hook 'lsp)
  (add-hook 'haskell-mode-hook 'lsp)
  (add-hook 'JavaScript-mode-hook 'lsp)


(use-package company
   :after lsp-mode
   :hook (lsp-mode . company-mode)
   :custom
   (company-minimum-prefix-length 1)
   (company-idle-delay 0.0))

 (use-package company-irony)

 (use-package company-box
   :hook (company-mode . company-box-mode))


 (use-package xref)

 (use-package eldoc
   :custom (lsp-eldoc-render-all t))

 (use-package yasnippet)

(use-package elfeed
  :bind (:map elfeed-search-mode-map
              ("A" . elfeed-show-all)
              ("E" . elfeed-show-emacs)
              ("D" . elfeed-show-daily)
              ("q" . elfeed-save-db-and-bury)))


(use-package elfeed-org
  :after elfeed
  :ensure t
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org")))

(use-package elfeed-goodies
  :after elfeed)

;;shortcut functions
(defun elfeed-show-all ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-all"))

(defun elfeed-show-emacs ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-emacs"))

(defun elfeed-show-daily ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-daily"))

(defun elfeed-load-db-and-open ()
  "Wrapper to load the elfeed db from disk before opening"
  (interactive)
  (elfeed-db-load)
  (elfeed)
  (elfeed-search-update--force)
  (elfeed-update))

;;write to disk when quiting
(defun elfeed-save-db-and-bury ()
  "Wrapper to save the elfeed db to disk before burying buffer"
  (interactive)
  (elfeed-db-save)
  (quit-window))

(setq gc-cons-threshold (* 50 1000 1000))
