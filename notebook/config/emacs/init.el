;; -*- lexical-binding: t; -*-
;; garbage collection
(setq gc-cons-threshold most-positive-fixnum
      read-process-output-max (* 1024 1024)) ;; 1mb

;; enconding and stuff
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; organize backups
(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory)))
      make-backup-files nil
      auto-save-default nil
      create-lockfiles nil)

;; visual stuff
(setq-default inhibit-startup-screen t
              visible-bell nil
              history-length 30
              use-dialog-box nil
              frame-resize-pixelwise t
              frame-title-format '("" "%b")
              truncate-partial-width-windows nil
              truncate-lines nil)

(setq default-frame-alist '((undecorated . t)))
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(save-place-mode 1)
;; (global-hl-line-mode 1) ;; highlight the current line
(blink-cursor-mode 0)
(global-visual-line-mode t)
;; (global-display-line-numbers-mode 1)
(show-paren-mode 1)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
;; (global-set-key (kbd "<f9>") 'display-line-numbers-mode)
(put 'narrow-to-region 'disabled nil)

;; font
(set-face-attribute 'default nil
                    :family "Iosevka"
                    :height 145
                    :width 'normal)

(setq mixed-pitch-set-height nil)

;; remember files I visit
(recentf-mode 1)
(setq recentf-max-saved-items 75)
;; (setq initial-buffer-choice (lambda () (org-todo-list)))
(setq history-length 30)

;; do stuff when I save files
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(global-auto-revert-mode 1)

;; scroll
(setq auto-window-vscroll nil)
(customize-set-variable 'fast-but-imprecise-scrolling t)
(customize-set-variable 'scroll-conservatively 101)
(customize-set-variable 'scroll-margin 0)
(customize-set-variable 'scroll-preserve-screen-position t)

;; margins
(setq-default left-margin-width 1 right-margin-width 1)

;; I don't like writing yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; automatically close delimiters
(electric-pair-mode 1)

;;; straight
;; Initialize package sources
(require 'package)

(setq package-user-dir (expand-file-name "elpa" user-emacs-directory)
      package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ;; ("cselpa" . "https://elpa.thecybershadow.net/packages/")
        ;; ("melpa-cn" . "http://mirrors.cloud.tencent.com/elpa/melpa/")
        ;; ("gnu-cn"   . "http://mirrors.cloud.tencent.com/elpa/gnu/")
        ))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (featurep 'straight)
  ;; Bootstrap straight.el
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
    (load bootstrap-file nil 'nomessage)))

;; Use straight.el for use-package expressions
(straight-use-package 'use-package)

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(unless package-archive-contents
  (package-refresh-contents))

(setq warning-suppress-types '((use-package) (comp)))
(setq comp-deferred-compilation t)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-undo-system 'undo-tree)
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

(use-package vertico
  :init (vertico-mode)
  :custom (setq vertico-cycle t))

;; (use-package corfu
;;   :init (global-corfu-mode
;;          corfu-popupinfo-mode)
;;   :custom (setq corfu-cycle t
;;                 corfu-auto t))

(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  ;; Other useful Dabbrev configurations.
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

(use-package orderless
  :custom (completion-styles '(orderless)))

(use-package consult
  :bind(("C-s" . consult-line)
        ("C-M-l" . consult-imenu)
        ("C-x b" . consult-buffer)
        ("C-c r" . consult-recent-file)
        ("C-c b" . consult-bookmark)
        ("C-r" . consult-history)))

(use-package marginalia
  :init (marginalia-mode)
  :custom (setq marginalia-annotators '(marginalia-annotators-heavy
                                        marginalia-annotators-light
                                        nil)))

(use-package embark
  :after (embark-consult)
  :bind ("C-S-a" . embark-act)
  :custom (setq embark-action-indicator
                (lambda (map)
                  (which-key--show-keymap "Embark" map nil nil 'no-paging)
                  #'which-key--hide-popup-ignore-command)
                embark-become-indicator embark-action-indicator))


;; dired
(setq dired-listing-switches "-agho --group-directories-first"
      dired-omit-files "^\\.[^.].*"
      dired-listing-switches "-al --group-directories-first"
      dired-dwim-target t  ; suggest a target for moving/copying intelligently
      dired-hide-details-hide-symlink-targets nil
      ;; don't prompt to revert, just do (insert )t
      dired-auto-revert-buffer #'dired-buffer-stale-p
      ;; Always copy/delete recursively
      dired-recursive-copies  'always
      dired-recursive-deletes 'top
      ;; Ask whether destination dirs should get created when copying/removing files.
      dired-create-destination-dirs 'ask)
(customize-set-variable 'global-auto-revert-non-file-buffers t)
(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))
;; (use-package dired-details
;;   :init (dired-details-install)
;;   :custom (setq dired-details-hidden-string ""))
(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

;; getting help
(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.2))
(use-package helpful
  :commands helpful-mode
  :diminish helpful-mode
  :config
  (define-key helpful-mode-map [remap revert-buffer] #'helpful-update)
  (global-set-key [remap describe-command] #'helpful-command)
  (global-set-key [remap describe-function] #'helpful-callable)
  (global-set-key [remap describe-key] #'helpful-key)
  (global-set-key [remap describe-symbol] #'helpful-symbol)
  (global-set-key [remap describe-variable] #'helpful-variable)
  (global-set-key (kbd "C-h F") #'helpful-function))

;; async
(use-package async
  :ensure t
  :init
  (dired-async-mode 1)
  :config
  (async-bytecomp-package-mode 1))

;; git
(use-package magit
  :bind ("C-M-;" . magit-status)
  :commands (magit-status magit-get-current-branch))

(use-package flyspell
  :bind (:map flyspell-mode-map
              ("C->" . flyspell-buffer))
  :init (progn (dolist (hook '(org-mode-hook text-mode-hook message-mode-hook))
                 (add-hook hook 'turn-on-flyspell))
               (add-hook 'prog-mode-hook 'flyspell-prog-mode))
  :config
  (setq ispell-program-name "hunspell"
        ispell-dictionary "pt_BR"))

;; ace-jump-mode
(use-package ace-jump-mode
  :bind ("C-;" . ace-jump-mode))

(use-package undo-tree
  :diminish undo-tree-mode
  :init (global-undo-tree-mode))
(setq undo-tree-history-directory-alist '(("." . "~/.config/emacs/undo")))

(use-package no-littering
  :config (setq no-littering-etc-directory
                (expand-file-name "config/" user-emacs-directory))
  (setq no-littering-var-directory
        (expand-file-name "data/" user-emacs-directory)))

;; notifications
(use-package ednc
  :init (ednc-mode))

(defun list-notifications ()
  (mapconcat #'ednc-format-notification (ednc-notifications) ""))

;; snippets
(use-package yasnippet
  :config
  (yas-global-mode 1)
  (setq yasnippet-snippets-dir "/home/basqs/.config/emacs/etc/yasnippet/snippets/")
  (use-package yasnippet-snippets
    :ensure t)
  (use-package auto-yasnippet
    :ensure t)
  (yas-reload-all))
(use-package doom-snippets
  :after yasnippet
  :straight (doom-snippets :type git :host github :repo "doomemacs/snippets" :files ("*.el" "*")))

;; indentation
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil) ;; spaces instead of tabs
(setq-default standard-indent 4)
(setq c-basic-offset tab-width)
(setq-default electric-indent-inhibit t)
(setq backward-delete-char-untabify-method 'nil)

(use-package highlight-indent-guides
  :diminish highlight-indent-guides-mode
  :hook ((prog-mode . (lambda ()
                        (highlight-indent-guides-mode)
                        (highlight-indent-guides-auto-set-faces))))
  :config
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-responsive 'stack
        highlight-indent-guides-delay 0))

;; rainbow
(use-package rainbow-mode
  :diminish
  :init
  (add-hook 'prog-mode-hook 'rainbow-mode))
(use-package rainbow-delimiters
  :diminish
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))
(use-package beacon
  :diminish
  :init
  (beacon-mode 1))
(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

;; modeline
(setq display-time-format "%H:%M"
      display-time-default-load-average nil)
(display-time-mode 1)
;; (line-number-mode -1)
(column-number-mode -1)
(size-indication-mode -1)
(display-battery-mode -1)
;; (use-package mood-line
;;   :init (mood-line-mode))
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))
(setq doom-modeline-buffer-state-icon nil
      doom-modeline-buffer-modification-icon nil
      doom-modeline-column-zero-based nil
      doom-modeline-percent-position nil
      doom-modeline-time-analogue-clock nil
      doom-modeline-buffer-encoding nil)
(display-battery-mode)

;; colortheme
(use-package doom-themes
  ;; :init (load-theme 'doom-tokyo-night t))
  ;; :init (load-theme 'modus-operandi t))
  :init (load-theme 'doom-nord t))

(defun toggle-theme()
  (interactive)
  (if (eq (car custom-enabled-themes) 'doom-tokyo-night)
      (consult-theme 'doom-tokyo-night)
    (consult-theme 'doom-oksolar-light)))
(global-set-key [f5] 'toggle-theme)

;; (use-package nm
;;   :vc (:url "https://github.com/Kodkollektivet/emacs-nm"
;;             :rev :newest))

;; (use-package dimmer
;;   :hook (after-init . dimmer-mode)
;;   :config
;;   (setq dimmer-fraction 0.5
;;         dimmer-adjustment-mode :foreground
;;         dimmer-use-colorspace :rgb
;;         dimmer-watch-frame-focus-events nil)
;;   (dimmer-configure-which-key)
;;   (dimmer-configure-magit)
;;   (dimmer-configure-posframe))

(add-hook 'server-after-make-frame-hook '(lambda ()
                                           (org-todo-list)
                                           (delete-other-windows)))

;; openwith
(use-package openwith
  :custom
  (setq openwith-associations '(
                                ("\\.pdf\\'" "okular" (file))
                                ("\\.mp4\\'" "mpv" (file)))))

(use-package vterm)
(use-package vterm-toggle
  :bind (("C-c v" . vterm-toggle)))

;; org
(use-package org
  ;; :init ((variable-pitch-mode 1)
  ;;        (auto-fill-mode 0)
  ;;        (visual-line-mode 1))

  :bind (("C-c C-f" . org-capture)
         ("C-c a" . org-agenda-list)
         ("C-c t" . org-todo-list)))

(setq
 ;; org-ellipsis "▾"
 org-startup-indented t
 org-pretty-entities t
 org-hide-emphasis-markers t
 org-src-fontify-natively t
 org-fontify-quote-and-verse-blocks t
 org-src-tab-acts-natively t
 org-edit-src-content-indentation 2
 org-hide-block-startup nil
 org-src-preserve-indentation nil
 org-startup-with-inline-images t
 org-directory "~/org"
 org-agenda-files (quote ("~/org/agenda.org" "~/org/mult.org" "~/org/refile-iphone.org"))
 org-agenda-use-time-grid t
 org-agenda-remove-tags t
 org-lowest-priority ?E
 org-capture-templates `(
                         ("f" "faculdade" entry (file+headline "~/org/agenda.org" "Faculdade")
                          "* TODO %? :faculdade:")
                         ("m" "mult"entry (file+headline "~/org/agenda.org" "Mult")
                          "* TODO %? :mult:")
                         ("a" "alemão" entry (file+headline "~/org/agenda.org" "Alemão")
                          "* TODO %? :alemão:")
                         ("t" "tarefas do dia" entry (file+datetree "~/org/tarefas.org")
                          "* %t \n%?" :empty-lines 0)
                         ("l" "ler/pesquisar" entry (file+headline "~/org/agenda.org" "Outros")
                          "* %x \n")
                         ("o" "outros" entry (file+headline "~/org/agenda.org" "Outros")
                          "* TODO %?"))
 ;; org-modules    '(org-crypt
 ;;                  org-habit
 ;;                  org-bookmark
 ;;                  org-eshell
 ;;                  org-irc)
 org-refile-targets '((nil :maxlevel . 1)
                      (org-agenda-files :maxlevel . 1))
 org-outline-path-complete-in-steps nil
 org-refile-use-outline-path t
 org-habit-graph-column 60)

(use-package org-sidebar
  :bind ("C-c s" . org-sidebar-tree-toggle))

(use-package org-journal
  :bind
  ("C-c j n" . org-journal-new-entry)
  :custom
  (setq org-journal-dir "~/org/journal/"
        org-journal-date-prefix "#+title: "
        org-journal-file-format "%Y-%m-%d.org"
        org-journal-date-format "%A, %d %B %Y"
        org-journal-enable-agenda-integration t))

;; roam for zettelkasten
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/org/roam")
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n t" . org-roam-tag-add)
         ("C-c n d n" . org-roam-dailies-capture-today))
  :config (org-roam-setup))
(setq org-roam-v2-ack t)
(setq org-roam-dailies-directory "journal/")
(setq org-roam-capture-templates
      '(("n" "normal" plain
         "%?"
         :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                            "#+title: ${title}\n#+filetags: ")
         :unnarrowed t)
        ("q" "química" plain
         "%?"
         :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                            "#+title: ${title}\n#+filetags: :química:\n#+startup: latexpreview")
         :unnarrowed t)
        ("m" "matemática" plain
         "%?"
         :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                            "#+title: ${title}\n#+filetags: :matemática:\n#+startup: latexpreview")
         :unnarrowed t)))

;;graph
(use-package org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))


(defun org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
   "/DONE" 'tree))

(use-package org-appear
  :hook
  (org-mode . org-appear-mode))

;; (use-package org-bullets ;; not working for some reason
;;   :ensure t
;;   :init
;;   (setq org-bullets-bullet-list '("❯" "❯❯" "❯❯❯" "❯❯❯❯" "❯❯❯❯❯"))
;;   :config
;;   (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; Org agenda
(setq org-agenda-start-day "+0d"
      ;;       org-agenda-skip-timestamp-if-done t
      ;;       org-agenda-skip-schedule-if-done t
      ;;       org-agenda-skip-deadline-if-done t
      ;;       ;; Edit settings
      ;;       org-auto-align-tags
      org-tags-column 0
      org-catch-invisible-edits 'show-and-error
      org-insert-heading-respect-content t
      org-agenda-use-time-grid nil)
(advice-add 'org-agenda :after
            (lambda ()
              (when (equal (buffer-name)
                           "*Org Agenda(a)*")
                (calendar)
                (other-window 1))))

(advice-add 'org-agenda-quit :before
            (lambda ()
              (let ((window (get-buffer-window calendar-buffer)))
                (when (and window (not (one-window-p window)))
                  (delete-window window)))))

(setq org-adapt-indentation t
      org-hide-leading-stars t
      org-hide-emphasis-markers t
      org-pretty-entities t)
(use-package olivetti-mode)
(setq org-src-fontify-natively t
	  org-src-tab-acts-natively t
      org-edit-src-content-indentation 0)
(add-hook 'org-mode-hook 'visual-line-mode)
;; (add-hook 'org-mode-hook 'olivetti-mode)
(setq org-lowest-priority ?F)  ;; Gives us priorities A through F
(setq org-default-priority ?E) ;; If an item has no priority, it is considered [#E].

(setq org-todo-keywords
      '((sequence
		 "NEXT(n)" "TODO(t)" ; Needs further action
		 "|"
		 "DONE(d)")
        (sequence
         "LER(l)" "LENDO"
         "|"
         "DONE(d)")))                           ; Needs no action currently

;; (setq org-todo-keyword-faces
;;       '(("TODO"      :inherit (org-todo region) :foreground "#A3BE8C" :weight bold)
;; 		("LER"     :inherit (org-todo region) :foreground "#81A1C1" :weight bold)
;; 		("NEXT"      :inherit (org-todo region) :foreground "#EBCB8B" :weight bold)
;; 		("DONE"      :inherit (org-todo region) :foreground "#30343d" :weight bold)))

(use-package org-modern
  :config
  (setq
   org-auto-align-tags t
   org-tags-column 0
   org-fold-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Agenda styling
   org-agenda-tags-column 0
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
	 (800 1000 1200 1400 1600 1800 2000)
	 " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────")

  (global-org-modern-mode))


;; (use-package org-superstar
;;   :config
;;   (setq org-superstar-leading-bullet " ")
;;   (setq org-superstar-special-todo-items t) ;; Makes TODO header bullets into boxes
;;   (setq org-superstar-todo-bullet-alist '(("TODO" . 9744)
;;                                           ("DONE" . 9744)
;;                                           ("READ" . 9744)
;;                                           ("IDEA" . 9744)
;;                                           ("WAITING" . 9744)
;;                                           ("CANCELLED" . 9744)
;;                                           ("PROJECT" . 9744)
;;                                           ("POSTPONED" . 9744)))
;;   )



(defun org-super-agenda-list ()
  (let ((org-super-agenda-groups
         '(
           ;; Filter where tag is CRITICAL
           (:name "Faculdade"
                  :tag "faculdade"
                  :order 0
                  )
           ;; Filter where TODO state is IN-PROGRESS
           (:name "Mult"
                  :tag "mult"
                  :order 1
                  )
           ;; Filter where TODO state is BLOCKED or where the tag is obstacle
           (:name "IC"
                  :and (:tag "ic" :todo "TODO")
                  :order 2
                  )
           ;; Filter where tag is @research
           (:name "Pesquisar / ler depois"
                  :todo "LER"
                  :order 3
                  )
           ;; Filter where tag is @write_future_ticket
           (:name "Lembretes"
                  :and (:todo "TODO" :not(:todo "NEXT"))
                  :order 4
                  )
           )))
    (org-agenda-list)))

(use-package org-super-agenda)
(org-super-agenda-mode 1)
(setq org-super-agenda-groups
      '(
        ;; Filter where tag is CRITICAL
        (:name "Faculdade"
               :tag "faculdade"
               :order 0
               )
        ;; Filter where TODO state is IN-PROGRESS
        (:name "Mult"
               :tag "mult"
               :order 1
               )
        ;; Filter where TODO state is BLOCKED or where the tag is obstacle
        (:name "IC"
               :and (:tag "ic" :todo "TODO")
               :order 2
               )
        (:name "Alemão"
               :tag "alemão"
               :order 3)
        ;; Filter where tag is @research
        (:name "Pesquisar / ler depois"
               :order 4
               )
        ;; Filter where tag is @write_future_ticket
        (:name "Lembretes"
               :and (:todo "TODO" :not(:todo "NEXT"))
               :order 5
               )
        (:discard (:anything))
        ))

(setq org-agenda-custom-commands
      '(
        ("b" "bezo agenda"
         (
          (alltodo ""
                   (
                    ;; Remove tags to make the view cleaner
                    (org-agenda-remove-tags t)
                    (org-agenda-prefix-format "  %t  %s")
                    (org-agenda-overriding-header "TAREFAS")

                    ;; Define the super agenda groups (sorts by order)
                    (org-super-agenda-groups
                     '(
                       ;; Filter where tag is CRITICAL
                       (:name "Faculdade"
                              :tag "faculdade"
                              :order 0
                              )
                       ;; Filter where TODO state is IN-PROGRESS
                       (:name "Mult"
                              :tag "mult"
                              :order 1
                              )
                       ;; Filter where TODO state is BLOCKED or where the tag is obstacle
                       (:name "IC"
                              :and (:tag "ic" :todo "TODO")
                              :order 2
                              )
                       ;; Filter where tag is @research
                       (:name "Pesquisar / ler depois"
                              :order 3
                              )
                       ;; Filter where tag is @write_future_ticket
                       (:name "Lembretes"
                              :and (:todo "TODO" :not(:todo "NEXT"))
                              :order 4
                              )
                       )
                     )
                    )
                   )
          (agenda ""
                  (
                   (org-agenda-remove-tags t)
                   (org-agenda-span 7)
                   )
                  )

          ))
        ))


;;(let ((org-super-agenda-groups
;;       '((:auto-group t))))
;;  (org-agenda-list))


(defvar center-document-desired-width 90
  "The desired width of a document centered in the window.")

(defun center-document--adjust-margins ()
  ;; Reset margins first before recalculating
  (set-window-parameter nil 'min-margins nil)
  (set-window-margins nil nil)

  ;; Adjust margins if the mode is on
  (when center-document-mode
    (let ((margin-width (max 0
			                 (truncate
			                  (/ (- (window-width)
				                    center-document-desired-width)
				                 2.0)))))
      (when (> margin-width 0)
	    (set-window-parameter nil 'min-margins '(0 . 0))
	    (set-window-margins nil margin-width margin-width)))))

;; (use-package org-modern
;;   :hook
;;   (org-mode . global-org-modern-mode))

;; LaTeX previews
(use-package org-fragtog
  :after org
  :custom
  (org-startup-with-latex-preview t)
  :hook
  (org-mode . org-fragtog-mode)
  :custom
  (org-format-latex-options
   (plist-put org-format-latex-options :scale 1)
   (plist-put org-format-latex-options :foreground 'auto)
   (plist-put org-format-latex-options :background 'auto)))

;;org babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (sql . t)
   (R . t)
   (js . t)))

;; Define a custom command to save the org agenda to a file
;; (setq org-agenda-custom-commands
;;       `(("X" agenda "" nil ,(list org-agenda-private-local-path))))

(defun org-agenda-export-to-ics ()
  (set-org-agenda-files)
  ;; Run all custom agenda commands that have a file argument.
  (org-batch-store-agenda-views)

  ;; Org mode correctly exports TODO keywords as VTODO events in ICS.
  ;; However, some proprietary calendars do not really work with
  ;; standards (looking at you Google), so VTODO is ignored and only
  ;; VEVENT is read.
  (with-current-buffer (find-file-noselect org-agenda-private-local-path)
    (goto-char (point-min))
    (while (re-search-forward "VTODO" nil t)
      (replace-match "VEVENT"))
    (save-buffer)))

(use-package writeroom-mode
  :bind (("C-c w" . writeroom-mode)))

(define-minor-mode center-document-mode
  "Toggle centered text layout in the current buffer."
  :lighter " Centered"
  :group 'editing
  (if center-document-mode
      (add-hook 'window-configuration-change-hook #'center-document--adjust-margins 'append 'local)
    (remove-hook 'window-configuration-change-hook #'center-document--adjust-margins 'local))
  (center-document--adjust-margins))

(add-hook 'org-mode-hook #'center-document-mode)

;; auto-tangle
(defun tangle-all-org-on-save-h ()
  "Tangle org files on save."
  (if (string= (file-name-extension (buffer-file-name)) "org")
      (org-babel-tangle)))
(add-hook 'after-save-hook #'tangle-all-org-on-save-h)

;;org drag and drop
;;;; from web
(use-package org-download
  :config
  (setq-default org-download-image-dir "~/Pictures/org")
  (add-hook 'dired-mode-hook 'org-download-enable)
  :bind
  ("C-c i" . org-download-clipboard))

(use-package emacs-everywhere)

;;org exports
(use-package org-ql)
(use-package ox-reveal)
;; (setq-default org-latex-pdf-process '("tectonic -Z shell-escape --outdir=%o %f"))
(require 'ox-latex)
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)

(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("org-plain-latex"
                 "\\documentclass{article}
           [NO-DEFAULT-PACKAGES]
           [PACKAGES]
           [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;; (setq latex-run-command "xelatex")
;; (setq-default org-latex-pdf-process
;;     (list "latexmk -pdflatex='%latex -shell-escape -interaction nonstopmode' -pdf -output-directory=%o %f"))
;; pdf
(use-package pdf-tools
  :mode "\\.pdf\\'"
  :config
  (pdf-tools-install)
  (define-pdf-cache-function pagelabels)
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick nil
        pdf-view-display-size 'fit-page)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward))

(use-package pdf-view-restore
  :after pdf-tools
  :ensure t
  :config
  :hook (pdf-view-mode . pdf-view-restore-mode))

(use-package smartparens
  :config (require 'smartparens-config))

(global-prettify-symbols-mode)

(use-package cdlatex) ;;cdlatex-command-help para ver a lista de abreviações
(use-package latexmk)
(use-package xenops)
(use-package evil-tex)
(use-package auctex
  :custom
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-PDF-mode t
        TeX-electric-sub-and-superscript t)

  (setq-default TeX-master nil)
  ;; :hook
  ;; (LaTeX-mode . TeX-fold-mode)
  )
(add-hook 'TeX-mode-hook 'flyspell-mode); Enable Flyspell mode for TeX modes such as AUCTeX. Highlights all misspelled words.
(add-hook 'LaTeX-mode-hook (lambda ()
                             (TeX-fold-mode 1)))
(add-hook 'LaTeX-mode-hook #'turn-on-cdlatex)
(add-hook 'LaTeX-mode-hook #'xenops-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook #'evil-tex-mode)

;; Use pdf-tools to open PDF files
(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-source-correlate-start-server t)

;; Update PDF buffers after successful LaTeX runs
(add-hook 'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer)



;; (use-package lsp-mode
;;   :commands (lsp lsp-deferred)
;;   :init
;;   (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
;;   :config
;;   (lsp-enable-which-key-integration t))
;; (add-hook 'prog-mode-hook #'lsp)

;; (use-package eglot
;;   :ensure t
;;   :config
;;   (setq eglot-autoshutdown 't)
;;   (setq eglot-autoreconnect nil)
;;   (setq eglot-confirm-server-initiated-edits nil))

;; (use-package editorconfig
;;   :ensure t
;;   :config (editorconfig-mode 1))

;; (use-package consult-eglot)
;; (use-package dap-mode
;;   :config
;;   (dap-auto-configure-mode)
;;   :bind (("<f7>" . dap-step-in)
;;          ("<f8>" . dap-next)
;;          ("<f9>" . dap-continue)))

;; (setq js-indent-level 2)
;; (use-package typescript-mode
;;   :mode "\\.ts\\'"
;;   :hook (typescript-mode . lsp-deferred)
;;   :config
;;   (setq typescript-indent-level 2))

;; ;; Eslint
;; (use-package flymake-eslint
;;   :ensure t
;;   :config
;;   ;; If Emacs is compiled with JSON support
;;   (setq flymake-eslint-prefer-json-diagnostics t)

;;   (defun lemacs/use-local-eslint ()
;;     "Set project's `node_modules' binary eslint as first priority.
;; If nothing is found, keep the default value flymake-eslint set or
;; your override of `flymake-eslint-executable-name.'"
;;     (interactive)
;;     (let* ((root (locate-dominating-file (buffer-file-name) "node_modules"))
;;            (eslint (and root
;;                         (expand-file-name "node_modules/.bin/eslint"
;;                                           root))))
;;       (when (and eslint (file-executable-p eslint))
;;         (setq-local flymake-eslint-executable-name eslint)
;;         (message (format "Found local ESLINT! Setting: %s" eslint))
;;         (flymake-eslint-enable))))


;;   (defun lemacs/configure-eslint-with-flymake ()
;; 	(when (or (eq major-mode 'tsx-ts-mode)
;; 			  (eq major-mode 'typescript-ts-mode)
;; 			  (eq major-mode 'typescriptreact-mode))
;;       (lemacs/use-local-eslint)))

;;   (add-hook 'eglot-managed-mode-hook #'lemacs/use-local-eslint))

(use-package flycheck :ensure t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 72 company - complete any aka company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.1)
  (global-set-key (kbd "<C-return>") 'company-complete)
  :hook (prog-mode . company-mode))
(use-package company-box
  :hook (company-mode . company-box-mode))
;; Get auto completion of :emoji: names.
(use-package company-emoji
  :ensure t
  :after company-mode
  :config
  (company-emoji-init))

(setq eldoc-echo-area-use-multiline-p nil)
(use-package eglot
  :config
  (setq eglot-report-progress nil)
  :bind
  (("M-RET" . eglot-code-actions)))

(setq lsp-tex-server 'digestif)

;; Python

(defun my/eglot-python-setup ()
  (unless (consult--preview-p)
    (eglot-ensure)
    ;; eglot wipes out the flymake functions; re-add
    (setq-local python-flymake-command '("flake8" "--max-line-length=100" "-"))
    (add-hook 'eglot-managed-mode-hook
		      (lambda ()
		        (add-hook 'flymake-diagnostic-functions #'python-flymake t t))
		      nil t)))

(defun pyrightconfig-write (virtualenv)
  (interactive "DEnv: ")

  (let* (;; file-truename and tramp-file-local-name ensure that neither `~' nor
         ;; the Tramp prefix (e.g. "/ssh:my-host:") wind up in the final
         ;; absolute directory path.
         (venv-dir (tramp-file-local-name (file-truename virtualenv)))

         ;; Given something like /path/to/.venv/, this strips off the trailing `/'.
         (venv-file-name (directory-file-name venv-dir))

         ;; Naming convention for venvPath matches the field for
         ;; pyrightconfig.json.  `file-name-directory' gets us the parent path
         ;; (one above .venv).
         (venvPath (file-name-directory venv-file-name))

         ;; Grabs just the `.venv' off the end of the venv-file-name.
         (venv (file-name-base venv-file-name))

         ;; Eglot demands that `pyrightconfig.json' is in the project root
         ;; folder.
         (base-dir (vc-git-root default-directory))
         (out-file (expand-file-name "pyrightconfig.json" base-dir))

         ;; Finally, get a string with the JSON payload.
         (out-contents (json-encode (list :venvPath venvPath :venv venv))))

    ;; Emacs uses buffers for everything.  This creates a temp buffer, inserts
    ;; the JSON payload, then flushes that content to final `pyrightconfig.json'
    ;; location
    (with-temp-file out-file (insert out-contents))))

(use-package anaconda-mode)
(use-package conda)

(add-to-list 'auto-mode-alist '("Pipfile" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.env\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.kv\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . js-mode))

(use-package nix-mode)
(use-package sxhkdrc-mode)

(use-package markdown-mode)
(use-package ess)

(use-package notmuch
  :ensure t
  :bind (("C-c m" . notmuch))
  :config
  (setq notmuch-saved-searches '((:name "Unread"
                                        :query "tag:inbox and tag:unread"
                                        :count-query "tag:inbox and tag:unread"
                                        :sort-order newest-first)
                                 (:name "Inbox"
                                        :query "tag:inbox"
                                        :count-query "tag:inbox"
                                        :sort-order newest-first)
                                 (:name "Archive"
                                        :query "tag:archive"
                                        :count-query "tag:archive"
                                        :sort-order newest-first)
                                 (:name "Sent"
                                        :query "tag:sent or tag:replied"
                                        :count-query "tag:sent or tag:replied"
                                        :sort-order newest-first)
                                 (:name "Trash"
                                        :query "tag:deleted"
                                        :count-query "tag:deleted"
                                        :sort-order newest-first))))
(use-package consult-notmuch)
(use-package notmuch-indicator)

(add-hook 'notmuch-hello-mode-hook
          '(lambda ()
             (run-with-timer 0 600  ;; every 5 min fetch email
                             '(lambda ()
                                (if
                                    (get-buffer "*notmuch-hello*")  ;; if notmuch buffer exists fetch email
                                    (async-shell-command "pushd /home/tp/Mail/normal.gmail; gmi sync; popd; notmuch new"))
                                (cancel-function-timers "no-output-shell-run")))))  ;; cancel timer if buffer does not exist
