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
(global-hl-line-mode 1)
(blink-cursor-mode 0)
(global-visual-line-mode t)
;; (global-display-line-numbers-mode 1)
(show-paren-mode 1)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
;; (global-set-key (kbd "<f9>") 'display-line-numbers-mode)
(put 'narrow-to-region 'disabled nil)

(global-set-key (kbd "M-C-5") #'query-replace-regexp)

;; font
(set-face-attribute 'default nil
                    :family "Iosevka"
                    :height 145
                    :width 'normal)

;; remember files I visit
(recentf-mode 1)
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
  :init (load-theme 'doom-gruvbox t))

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
 org-ellipsis "▾"
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
 org-agenda-files (quote ("~/org"))
 org-lowest-priority ?E
 org-capture-templates `(
                         ("f" "faculdade" entry (file+headline "~/org/agenda.org" "Faculdade")
                          "* TODO [#A] %? :faculdade:")
                         ("m" "mult"entry (file+headline "~/org/agenda.org" "Mult")
                          "* TODO [#A] %? :mult:")
                         ("a" "alemão" entry (file+headline "~/org/agenda.org" "Alemão")
                          "* TODO %?")
                         ("o" "outros" entry (file+headline "~/org/agenda.org" "Outros")
                          "* TODO %?"))
 org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                     (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
                     (sequence "|" "CANCELED(c)"))
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
 ;; (org-agenda-skip-entry-if 'nottodo 'done)

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

;; (let* ((base-font-color     (face-foreground 'default nil 'default))
;;        (headline           `(:inherit default :weight bold :foreground ,base-font-color)))
;;   (custom-theme-set-faces 'user
;;                           `(org-level-8 ((t (,@headline ))))
;;                           `(org-level-7 ((t (,@headline ))))
;;                           `(org-level-6 ((t (,@headline ))))
;;                           `(org-level-5 ((t (,@headline ))))
;;                           `(org-level-4 ((t (,@headline , :height 1.1))))
;;                           `(org-level-3 ((t (,@headline , :height 1.25))))
;;                           `(org-level-2 ((t (,@headline , :height 1.5))))
;;                           `(org-level-1 ((t (,@headline , :height 1.75))))
;;                           `(org-document-title ((t (,@headline , :height 2.0 :underline nil))))))

;; (setq face-remapping-alist '(;; Headers - outlines match org
;;                              (outline-1 org-level-1)
;;                              (outline-2 org-level-2)
;;                              (outline-3 org-level-3)

;;                              ;; Modeline - invis. active, monochrome inactive
;;                              (powerline-active1        mode-line)
;;                              (powerline-active2        mode-line)
;;                              (spaceline-highlight-face mode-line)

;;                              (powerline-active0        mode-line)
;;                              (mode-line-active         mode-line)
;;                              (mode-line-inactive       mode-line)
;;                              (powerline-inactive0      mode-line)
;;                              (powerline-inactive1      mode-line)
;;                              (powerline-inactive2      mode-line)
;;                              ))

;; (setq display/headers/common '(:underline t :inherit nil))
;; (setq display/headers/solarized-light
;;       `((org-level-1
;;          ,@display/headers/common
;;          :height 1.35
;;          :foreground "#a71d31")
;;         (org-level-2
;;          ,@display/headers/common
;;          :height 1.25
;;          :foreground "#8D6B94")
;;         (org-level-3
;;          ,@display/headers/common
;;          :height 1.15)))

;;;;; Org-blocks

(setq display/org-blocks/common '(:italic nil :underline nil :box t))
(setq display/org-blocks
      `((org-block-begin-line
         ,@display/org-blocks/common)
        (org-block-end-line
         ,@display/org-blocks/common)))

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

(use-package org-bullets
  :ensure t
  :init
  (setq org-bullets-bullet-list '("❯" "❯❯" "❯❯❯" "❯❯❯❯" "❯❯❯❯❯"))
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

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

(setq org-agenda-use-time-grid nil)
(use-package org-modern)
(global-org-modern-mode 1)

(org-super-agenda-mode 1)
(defun org-super-agenda-list ()
  (let ((org-super-agenda-groups
         '((:auto-group t))))
    (org-agenda-list)))

(setq org-super-agenda-groups '((:auto-group t)))

(use-package org-super-agenda)


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

(define-minor-mode center-document-mode
  "Toggle centered text layout in the current buffer."
  :lighter " Centered"
  :group 'editing
  (if center-document-mode
      (add-hook 'window-configuration-change-hook #'center-document--adjust-margins 'append 'local)
    (remove-hook 'window-configuration-change-hook #'center-document--adjust-margins 'local))
  (center-document--adjust-margins))

(add-hook 'org-mode-hook #'center-document-mode)

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

;; Distraction-free writing
(defun ews-distraction-free ()
  "Distraction-free writing environment using Olivetti package."
  (interactive)
  (if (equal olivetti-mode nil)
      (progn
        (window-configuration-to-register 1)
        (delete-other-windows)
        (text-scale-set 2)
        (olivetti-mode t))
    (progn
      (if (eq (length (window-list)) 1)
          (jump-to-register 1))
      (olivetti-mode 0)
      (text-scale-set 0))))

(use-package olivetti
  :demand t
  :bind
  (("<f9>" . ews-distraction-free)))

(dolist (face '(window-divider
                window-divider-first-pixel
                window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))
(set-face-background 'fringe (face-attribute 'default :background))



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

(use-package auctex
  :custom
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  ;; :hook
  ;; (LaTeX-mode . TeX-fold-mode)
  )

(add-hook 'LaTeX-mode-hook (lambda ()
                             (TeX-fold-mode 1)))

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 80 Java
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq eldoc-echo-area-use-multiline-p nil)
(use-package eglot
  :config
  (setq eglot-report-progress nil)
  :bind
  (("M-RET" . eglot-code-actions)))

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
