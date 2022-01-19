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
(set-face-bold-p 'bold nil)
(set-face-attribute 'default nil
                    :font "JetBrains Mono Nerd Font:antialias=true"
                    :height 110)
(set-face-bold-p 'bold nil)

;; (custom-set-faces
;;  '(default ((t (:inherit nil :height 100 :family "JetBrains Mono")))))

(use-package no-littering)

(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(defun evil-hook ()
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
  (add-hook 'evil-mode-hook 'evil-hook)
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  ;; (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  ;; (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  ;; (evil-global-set-key 'motion "j" 'evil-next-line)
  ;; (evil-global-set-key 'motion "k" 'evil-previous-line)
  (use-package ace-jump-mode)
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
  (setq which-key-idle-delay 0.2))

(use-package helpful
  :diminish
  :commands helpful-mode)

(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flycheck)               ;; Alternative: consult-flycheck
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep))

  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  :config
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep)

  (setq consult-narrow-key "<") ;; (kbd "C-+")

  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project))))))

(use-package orderless
  :custom (completion-styles '(orderless)))

(use-package vterm)
(use-package vterm-toggle
  :bind ("C-M-'" . vterm-toggle))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first")
           (setq dired-omit-files "^\\.[^.].*")))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-git)

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(use-package olivetti
  :bind ("C-c o" . olivetti-mode))

(use-package treemacs
  :init 
  (with-eval-after-load 'treemacs
    (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action)
    (treemacs-toggle-show-dotfiles))
  :bind(("C-t" . treemacs)))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-all-the-icons
  :ensure t)
(treemacs-load-theme "all-the-icons")

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package aggressive-indent
  :hook ((emacs-lisp-mode
          inferior-emacs-lisp-mode
          ielm-mode
          lisp-mode
          inferior-lisp-mode
          isp-interaction-mode
          slime-repl-mode) . aggressive-indent-mode))

(use-package magit
  :bind ("C-M-;" . magit-status)
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

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

(use-package crux
  :bind (("C-c D" . crux-delete-file-and-buffer)))

(setq browse-url-browser-function 'xwidget-webkit-browse-url)

(setq ispell-dictionary "pt_BR")

(use-package flycheck
  :hook (lsp-mode)
  :ensure t)
(add-hook 'after-init-hook #'global-flycheck-mode)

(use-package bug-hunter)

(setq-default tab-width 4)
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
          "agenda"
          "opens org-agenda"
          (lambda (&rest _) (org-agenda))
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

 (use-package doom-modeline
   :hook (after-init . doom-modeline-mode)
   :custom (setq doom-modeline-height 20
                 doom-modeline-bar-width 6
                 doom-modeline-lsp t
                 setq doom-modeline-buffer-encoding nil
                 doom-modeline-github t
                 doom-modeline-mu4e nil
                 doom-modeline-irc t
                 doom-modeline-minor-modes t
                 doom-modeline-major-mode-icon t)
   (custom-set-faces '(mode-line ((t (:height 0.85))))
                     '(mode-line-inactive ((t (:height 0.85)))))
 (use-package minions
   (:hook doom-modeline-mode)))

(use-package doom-themes)
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
(add-to-list 'org-structure-template-alist '("scm" . "src scheme"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("tex" . "src latex"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))

(setq org-confirm-babel-evaluate nil)

(require 'org-habit)
(add-to-list 'org-modules 'org-habit)
(setq org-habit-graph-column 60)

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
          ("#+RESULTS:" . ""))))
(add-hook 'org-mode-hook 'org/prettify-set)

(global-prettify-symbols-mode)

(use-package lsp-mode
  :straight t
  :hook (typescript-mode js2-mode web-mode)
  :bind
  ("TAB" . completion-at-point)
  ("C-c l n" . lsp-ui-find-next-reference)
  ("C-c l p" . lsp-ui-find-prev-reference)
  ("C-c l s" . counsel-imenu)
  ("C-c l e" . lsp-ui-flycheck-list)
  ("C-c l S" . lsp-ui-sideline-mode))

(use-package lsp-ui
  :straight t
  :hook (lsp-mode)
  :custom
  ((setq lsp-ui-sideline-enable t)
   (setq lsp-ui-sideline-show-hover t)
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

(use-package go-mode
  :hook (lsp-deferred))

(use-package flycheck-golangci-lint
  :hook (go-mode))

(use-package python-mode
  :ensure t
  :hook (python-mode . lsp-deferred)
  :custom
  (python-shell-interpreter "python3")
  (dap-python-executable "python3")
  (dap-python-debugger 'debugpy)
  :config
  (use-package dap-python))

(use-package pyvenv
  :config
  (pyvenv-mode 1))

(use-package js2-mode
  :custom
  (add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))

  (setq js2-mode-show-strict-warnings nil))

(use-package apheleia
  :custom (apheleia-global-mode +1))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package geiser-mit)

(setq company-format-margin-function nil)
(add-hook 'after-init-hook 'global-company-mode)

(add-hook 'c-mode-hook 'lsp)
(add-hook 'js2-mode-hook 'lsp)
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

(setq gc-cons-threshold (* 50 1000 1000))
