;; garbage collection
(setq gc-cons-threshold most-positive-fixnum)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
;; don't fill my directories with backups
(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))
;; visual stuff
(setq inhibit-startup-screen t)
(setq visible-bell nil)
(setq tab-width 4)
(setq evil-shift-width tab-width)
;; (setq comp-async-report-warnings-errors nil)
(setq history-length 25)
(setq use-dialog-box nil)
(setq global-auto-revert-non-file-buffers t)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq frame-resize-pixelwise t)
(setq-default frame-title-format '("" "%b"))
;; stuff
(if (and (fboundp 'native-comp-available-p)
         (native-comp-available-p))
    (setq comp-deferred-compilation t
          package-native-compile t)
  (message "Native complation is *not* available, lsp performance will suffer..."))
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(recentf-mode 1)
(setq initial-buffer-choice (lambda () (recentf-open-more-files)))
(savehist-mode 1)
(global-auto-revert-mode 1)
(global-hl-line-mode t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)
(save-place-mode 1)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(global-set-key (kbd "<f9>") 'display-line-numbers-mode)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
;; (set-face-bold-p 'bold nil)
(set-face-attribute 'default nil
                    :family "Iosevka"
                    :height 145
                    :width 'normal)
(setq user-full-name "Bernardo Basques"
      user-mail-address "bequintao@gmail.com")
;; scroll
(setq auto-window-vscroll nil)
(customize-set-variable 'fast-but-imprecise-scrolling t)
(customize-set-variable 'scroll-conservatively 101)
(customize-set-variable 'scroll-margin 0)
(customize-set-variable 'scroll-preserve-screen-position t)

(defalias 'yes-or-no-p 'y-or-n-p)

(dolist (mode '(org-mode-hook
                term-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))
;; package manager
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
(setq warning-suppress-types '((comp)))
(setq comp-deferred-compilation t)
(setq use-package-always-ensure t)

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

(add-to-list 'load-path "~/.emacs.d/local")

(use-package emacs-everywhere)

(use-package diminish)

(use-package gcmh
  :diminish
  :custom
  (gcmh-idle-delay 1000)
  (gcmh-high-cons-threshold (* 16 1024 1024)) ;; 16 MB
  :hook (after-init . gcmh-mode))

(setq-default shell-file-name "/bin/sh")

(use-package auto-package-update
  :custom
  (setq auto-package-update-interval 7
        auto-package-update-prompt-before-update t
        auto-package-update-hide-results nil))
;; evil mode
(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-respect-visual-line-mode t
        evil-undo-system 'undo-tree)
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
  :config
  ;; (add-hook 'evil-mode-hook 'evil-hook)
  (evil-mode 1))

(use-package undo-tree
  :after evil
  :diminish undo-tree-mode
  :init (global-undo-tree-mode 1))

(use-package evil-nerd-commenter
  :after evil
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package evil-collection
  :after evil
  :diminish evil-collection-unimpaired-mode
  :config (evil-collection-init))

(use-package ace-jump-mode
  :after evil
  :config (evil-global-set-key 'motion ";" 'ace-jump-mode))

;; completion system
(use-package vertico
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("C-f" . vertico-exit))
  :custom (vertico-cicle t)
  :init (vertico-mode)
  :diminish vertico-mode)
(use-package marginalia
  :custom
  (marginalia-annotators
   '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode)
  :diminish marginalia-mode)
(use-package consult
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("M-g e" . consult-compile-error)
         ("M-s f" . consult-flycheck)               ;; Alternative: consult-flycheck
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
  :diminish consult-mode)
(use-package orderless
  :custom (completion-styles '(orderless)))
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
;; terminal
;; vterm
(use-package vterm
  :custom (setq explicit-shell-file-name "zsh"
                term-prompt-regexp "^[^#$%>\n]*[#$%>] *"
  				vterm-kill-buffer-on-exit t)
  :bind (("C-c e" . vterm)))
(use-package vterm-toggle
  :bind (("C-M-'" . vterm-toggle)))

;; eshell
(setq eshell-prompt-regexp "^[^αλ\n]*[αλ] ")
(setq eshell-prompt-function
      (lambda nil
        (concat
         (if (string= (eshell/pwd) (getenv "HOME"))
             (propertize "~" 'face `(:foreground "#99CCFF"))
           (replace-regexp-in-string
            (getenv "HOME")
            (propertize "~" 'face `(:foreground "#99CCFF"))
            (propertize (eshell/pwd) 'face `(:foreground "#99CCFF"))))
         (if (= (user-uid) 0)
             (propertize " α " 'face `(:foreground "#FF6666"))
           (propertize " λ " 'face `(:foreground "#A6E22E"))))))

(setq eshell-highlight-prompt nil)

(defalias 'open 'find-file-other-window)
(defalias 'clean 'eshell/clear-scrollback)

(defun eshell/sudo-open (filename)
  "Open a file as root in Eshell."
  (let ((qual-filename (if (string-match "^/" filename)
                           filename
                         (concat (expand-file-name (eshell/pwd)) "/" filename))))
    (switch-to-buffer
     (find-file-noselect
      (concat "/sudo::" qual-filename)))))

(defun eshell-other-window ()
  "Create or visit an eshell buffer."
  (interactive)
  (if (not (get-buffer "*eshell*"))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (eshell))
    (switch-to-buffer-other-window "*eshell*")))

(global-set-key (kbd "<s-C-return>") 'eshell-other-window)

(global-set-key (kbd "C-c s") 'async-shell-command)

;; async
(use-package async
  :ensure t
  :init
  (dired-async-mode 1)
  :config
  (async-bytecomp-package-mode 1))
;; dired
(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first")
           (setq dired-omit-files "^\\.[^.].*"))
  :config (customize-set-variable 'global-auto-revert-non-file-buffers t))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-git)

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

;; git
(use-package magit
  :bind ("C-M-;" . magit-status)
  :commands (magit-status magit-get-current-branch))

;;indentation
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
;; (setq display-time-format "%H:%M")
;; (display-battery-mode)(display-time-mode)
(display-time-mode -1)
(line-number-mode 1)
(column-number-mode -1)
(size-indication-mode -1)
(display-battery-mode -1)

(use-package powerline
  :ensure t
  :config
  (setq powerline-default-separator 'nil
        powerline-display-buffer-size nil)
  :init
  (powerline-default-theme)
  :hook
  ('after-init-hook) . 'powerline-reset)

;; colortheme
(use-package doom-themes :defer t)
(use-package mindre-theme
  :straight (:host github :repo "erikbackman/mindre-theme"))
(use-package nano-theme)
(defun apply-nano-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (nano-light))
    ('dark (nano-dark))))
;; (load-theme 'doom-tomorrow-night t)
(set-mouse-color "DodgerBlue")

(defun toggle-theme ()
  (interactive)
  (if (eq (car custom-enabled-themes) 'doom-tomorrow-night)
      (consult-theme 'doom-tomorrow-day)
    (consult-theme 'doom-tomorrow-night)))
(global-set-key [f5] 'toggle-theme)

(use-package dimmer
  :hook (after-init . dimmer-mode)
  :config
  (setq dimmer-fraction 0.5
        dimmer-adjustment-mode :foreground
        dimmer-use-colorspace :rgb
        dimmer-watch-frame-focus-events nil)
  (dimmer-configure-which-key)
  (dimmer-configure-magit)
  (dimmer-configure-posframe))
;; org-mode

(use-package org
  :config
  (setq org-startup-with-inline-images t)
  :hook (org-mode . (org-indent-mode
                     prettify-symbols-mode
                     org-toggle-prettty-entities
                     olivetti-mode
                     setq org-hide-emphasis-markers t
                     truncate-lines t
                     evil-auto-indent nil)))

(require 'org-tempo)

(define-key global-map (kbd "C-c a") 'org-agenda-list)

(use-package idle-org-agenda
  :after org-agenda
  :ensure t
  :config (idle-org-agenda-mode))

;; writting
;; (use-package olivetti
;;   :diminish olivetti-mode
;;   :bind ("C-c o" . olivetti-mode))

(use-package writeroom-mode
  :bind ("C-c o" . writeroom-mode))

(setq ispell-program-name "aspell")
(setq ispell-change-dictionary "pt_BR")
;; (defun text-mode-hook-setup ()
;;   ;; Turn off RUN-TOGETHER option when spell check text-mode
;;   (setq-local ispell-extra-args (flyspell-detect-ispell-args)))
;; (add-hook 'text-mode-hook 'text-mode-hook-setup)
;; (dolist (hook '(text-mode-hook))
;;   (add-hook hook (lambda ()
;; 		   (flyspell-mode 1)
;; 		   (visual-line-mode 1))))

(use-package flyspell
  :diminish flyspell-mode)

(use-package guess-language
  :config
  (setq guess-language-languages '(en pt))
  (setq guess-language-min-paragraph-length 10)
  :hook
  (text-mode . guess-language-mode))

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'flyspell-mode)

(setq org-ellipsis " ▾"
      org-hide-emphasis-markers t
      org-special-ctrl-a/e t
      org-special-ctrl-k t
      org-src-fontify-natively t
      org-fontify-whole-heading-line t
      org-fontify-quote-and-verse-blocks t
      org-src-tab-acts-natively t
      org-edit-src-content-indentation 2
      org-log-done t
      org-hide-block-startup nil
      org-src-preserve-indentation nil
      org-startup-folded 'content
      org-cycle-separator-lines 2
      org-agenda-files '("~/Docs/org/agenda.org")
      org-directory  "~/Docs/org/"
      org-lowest-priority ?E
      org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline "~/Docs/org/agenda.org" "Tarefas")
                               "* TODO %i%?")
                              ("c" "Todo [inbox]" entry
                               (file+headline "~/Docs/org/agenda.org" "Lembretes")
                               "* %i%?"))
      org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d)")
                          (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
                          (sequence "|" "CANCELED(c)")))

(define-key global-map (kbd "C-c C-c") 'org-capture)

(add-to-list 'org-structure-template-alist '("sh" . "src sh"))
(add-to-list 'org-structure-template-alist '("scm" . "src scheme"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("tex" . "src latex"))
(add-to-list 'org-structure-template-alist '("go" . "src go"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))

(setq org-confirm-babel-evaluate nil)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp .t)
   (python . t)
   (scheme . t)
   (plantuml . t)
   (latex . t)
   (shell . t)))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "●" "○" "•" "●" "○" "•")))

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
                          `(org-document-title ((t (,@headline , :height 2.0 :underline nil))))))
(require 'org-habit)
(add-to-list 'org-modules 'org-habit)
(setq org-habit-graph-column 60)
(use-package org-journal
  :config (setq org-journal-dir "~/Docs/org/journal/")
  :bind
  ("C-c j n" . org-journal-new-entry)
  :custom
  (org-journal-date-prefix "#+title: ")
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-dir "~/stack/roam/")
  (org-journal-date-format "%A, %d %B %Y")
  (org-journal-enable-agenda-integration t))
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/Docs/org/roam")
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
;; (global-prettify-symbols-mode)
;;org exports
;; (use-package ox-latex)
;; (with-eval-after-load 'ox-latex
;;   (add-to-list 'org-latex-classes
;; 	       '("org-plain-latex"
;; 		 "\\documentclass{article}
;; 	   [NO-DEFAULT-PACKAGES]
;; 	   [PACKAGES]
;; 	   [EXTRA]"
;; 		 ("\\section{%s}" . "\\section*{%s}")
;; 		 ("\\subsection{%s}" . "\\subsection*{%s}")
;; 		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
;; 		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
;; 		 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
(use-package org-ql)
(use-package ox-reveal)
(use-package pandoc)
(use-package ox-pandoc)

(add-to-list 'org-odt-convert-processes
             '("gnumeric" "/usr/bin/env ssconvert %i %o"))

(defun org-table-export-to-spreadsheet (arg)
  "Export org table to spreadsheet formats, e.g. `ods', `xls', `xlsx'."
  (interactive "sFormat: ")
  (let* ((source-file  (file-name-sans-extension (buffer-file-name  (current-buffer))))
         (csv-file (concat source-file ".csv")))
    (org-table-export csv-file "orgtbl-to-csv")
    (org-odt-convert csv-file arg)))


;; pdf
(use-package pdf-tools
  :mode "\\.pdf\\'"
  :config
  (pdf-tools-install)
  (define-pdf-cache-function pagelabels)
  (setq pdf-view-resize-factor 1.1)
  (setq pdf-view-display-size 'fit-page)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  :hook (pdf-tools-enabled . pdf-view-midnight-minor-mode))
(use-package pdf-view-restore
  :after pdf-tools
  :ensure t
  :config
  :hook (pdf-view-mode . pdf-view-restore-mode))

(use-package org-pdftools
  :ensure t
  :hook (org-load-hook . org-pdftools-setup-link))
(use-package org-noter
  :after org-noter-pdftools
  :config
  (require 'org-noter-pdftools)
  (setq org-noter-auto-save-last-location t))
;; lsp
(use-package lsp-mode
  :straight t
  :hook ((c-mode          ; clangd
          c++-mode        ; clangd
          c-or-c++-mode   ; clangd
          js-mode         ; ts-ls (tsserver wrapper)
          typescript-mode ; ts-ls (tsserver wrapper)
          python-mode     ; pyright
          web-mode        ; ts-ls/HTML/CSS
          haskell-mode    ; haskell-language-server
          ) . lsp-deferred)
  :bind
  ("C-c l n" . lsp-ui-find-next-reference)
  ("C-c l p" . lsp-ui-find-prev-reference)
  ("C-c l s" . counsel-imenu)
  ("C-c l e" . lsp-ui-flycheck-list)
  ("C-c l S" . lsp-ui-sideline-mode)
  :config
  (setq lsp-auto-guess-root t)
  (setq lsp-log-io nil)
  (setq lsp-restart 'auto-restart)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-signature-render-documentation nil)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-semantic-tokens-enable nil)
  (setq lsp-enable-folding nil)
  (setq lsp-enable-imenu nil)
  (setq lsp-enable-snippet nil)
  (setq completion-styles '(orderless)
        completion-category-defaults nil)
  (setq lsp-idle-delay 0.2))


(use-package lsp-ui
  :straight t
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-enable t
        lsp-ui-sideline-show-code-actions
        lsp-ui-doc-enable t
        lsp-ui-doc-delay 0.2
        lsp-ui-flycheck-enable t))
;;; debug
(use-package dap-mode
  :straight t
  :custom (lsp-enable-dap-auto-configure nil)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (dap-node-setup))
;; flycheck
(use-package flycheck
  :diminish flycheck-mode
  :hook (after-init . global-flycheck-mode)
  :custom
  (flycheck-check-syntax-automatically '(save mode-enabled)))
;; tree-siter
(use-package tree-sitter
  :ensure t
  :config
  ;; activate tree-sitter on any buffer containing code for which it has a parser available
  (global-tree-sitter-mode)
  ;; you can easily see the difference tree-sitter-hl-mode makes for python, ts or tsx
  ;; by switching on and off
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)
;; company
(use-package company
  :diminish company-mode
  :bind
  (:map company-active-map
        ("C-n". company-select-next)
        ("C-p". company-select-previous))
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
  :config
  (setq company-dabbrev-other-buffers t
        company-dabbrev-code-other-buffers t
        company-show-numbers t
        company-dabbrev-ignore-case t
        company-idle-delay 0)
  :hook ((prog-mode . company-mode)
         (company-mode . yas-minor-mode)))

(use-package company-irony)

(use-package company-box
  :diminish company-box-mode
  :hook (company-mode . company-box-mode))
;; eldoc
(use-package eldoc
  :diminish eldoc-mode
  :custom (lsp-eldoc-render-all t))
;; snippets
(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1)
  (use-package yasnippet-snippets
    :ensure t)
  (use-package auto-yasnippet
    :ensure t)
  (yas-reload-all))
;; C
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
;; Go
;; (use-package go-mode
;;   :mode "\\.go\\'"
;;   (defun go-mode-hook()
;;     ;;(setq-default tab-width 2)
;;     (add-hook 'before-save-hook 'gofmt-before-save))
;;   :hook ((go-mode . go-mode-hook)))
;; (use-package go-snippets)
;; (use-package flycheck-golangci-lint)
;; python
(use-package python-mode
  :ensure t
  :custom
  (python-shell-interpreter "python3")
  ;; (dap-python-executable "python3")
  ;; (dap-python-debugger 'debugpy)
  :config
  ;; (use-package dap-python)
  (use-package lsp-pyright)
  (use-package pyvenv
    :config
    (pyvenv-mode 1)))
;; js/ts
(use-package js2-mode
  :custom
  (add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))
  (setq js2-mode-show-strict-warnings nil))
(use-package apheleia
  :diminish apheleia-mode
  :custom (apheleia-global-mode +1))
(use-package typescript-mode
  :mode "\\.ts\\'"
  :config
  (setq typescript-indent-level 2))
;; web-mode
(use-package web-mode
  :defer t
  :mode ("\\.html\\'"))

(use-package web-beautify
  :defer t
  :bind (:map web-mode-map
              ("C-c b" . web-beautify-html)
              :map js2-mode-map
              ("C-c b" . web-beautify-js)))
;; lisp
(use-package lispy
  :diminish lispy-mode
  :hook ((emacs-lisp-mode scheme-mode) . lispy-mode))
(use-package lispyville
  :diminish lispyville-mode
  :hook (lispy-mode . lispyville-mode))
;; (use-package geiser-mit)
;; sml
(use-package sml-mode
  :mode "\\.sml\\'")
;; nix
(use-package nix-mode
  :mode "\\.nix\\'")
(use-package nix-sandbox)
;; haskell
(use-package haskell-mode
  :mode "\\.hs\\'"
  :config
  (use-package lsp-haskell)
  (add-hook 'haskell-mode-hook #'haskell-indentation-mode)
  (add-hook 'haskell-mode-hook #'yas-minor-mode)
  (add-hook 'haskell-mode-hook #'lsp)
  (setq haskell-stylish-on-save t))
;; other stuff
;; ;; irc
;;  (use-package erc
;; 	:custom
;; 	(erc-autojoin-timing 'ident)
;; 	(erc-autojoin-channels-alist '(("irc.rizon.net" "#rice")))
;; 	(erc-fill-function 'erc-fill-static)
;; 	(erc-fill-static-center 22)
;; 	(erc-hide-list '("JOIN" "PART" "QUIT"))
;; 	(erc-lurker-hide-list '("JOIN" "PART" "QUIT"))
;; 	(erc-lurker-threshold-time 43200)
;; 	;;(erc-prompt-for-nickserv-password nil)
;; 	;;(erc-prompt-for-password nil)
;; 	(erc-server-reconnect-attempts 5)
;; 	(erc-server-reconnect-timeout 3)
;; 	(erc-quit-reason 'erc-quit-reason-normal)
;; 	(erc-track-exclude-types '("JOIN" "MODE" "NICK" "PART" "QUIT"
;; 							   "324" "329" "332" "333" "353" "477"))
;; 	:config
;; 	;; login
;; 	(setq erc-nickserv-identify-mode 'autodetect)
;; 	;; Interpret mIRC-style color commands in IRC chats
;; 	(setq erc-interpret-mirc-color t)
;; 	;; Kill buffers for channels after /part
;; 	(setq erc-kill-buffer-on-part t)
;; 	;; Kill buffers for private queries after quitting the server
;; 	(setq erc-kill-queries-on-quit t)
;; 	;; Kill buffers for server messages after quitting the server
;; 	(setq erc-kill-server-buffer-on-quit t)
;; 	;; open query buffers in the current window
;; 	(setq erc-query-display 'buffer)
;; 	;; misc stuff
;; 	(setq erc-prompt " >"
;; 		  erc-nick '("diamondbond" "diamondbond_"))
;; 	(add-to-list 'erc-modules 'notifications)
;; 	(add-to-list 'erc-modules 'spelling)
;; 	(erc-services-mode 1)
;; 	(erc-update-modules))
;; (server-start)
;; functions
(defun config-visit()
  (interactive)
  (find-file "~/.emacs.d/config.org"))
(global-set-key (kbd "C-c e") 'config-visit)

(defun config-reload ()
  "Reloads ~/.emacs.d/config.org at runtine"
  (interactive)
  (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))
(global-set-key (kbd "C-c r") 'config-reload)
(defun record-screen-start ()
  "Record screen to .mkv"
  (interactive)
  (let ((input (read-file-name "Output file: ")))
    (async-shell-command
     (concat "ffmpeg -y -f x11grab -s 1920x1080 -framerate 30 -i :1 " input))))
(defun record-screen-stop ()
  "Stops recording screen."
  (interactive)
  (shell-command "pkill ffmpeg"))

(defun cmus ()
  "cmus"
  (interactive)
  (run-in-vterm "cmus"))

(defun restart-server()
  "deletes and restart the emacs server"
  (interactive)
  (server-force-delete)
  (async-shell-command "emacs --daemon"))

(defun apply-user ()
  "Reloads home-manager configuration"
  (interactive)
  (async-shell-command "pushd /home/basqs/.nixfiles; nix build .#homeManagerConfigurations.basqs.activationPackage; ./result/activate; popd"))

(defun cht.sh (query)
  "QUERY cht.sh"
  (interactive "sQuery: ")
  (eww (concat "https://cht.sh/" query)))

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))
(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(use-package no-littering)
(setq gc-cons-threshold (* 50 1000 1000))
(provide 'init)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("db5b906ccc66db25ccd23fc531a213a1afb500d717125d526d8ff67df768f2fc" "f0eb51d80f73b247eb03ab216f94e9f86177863fb7e48b44aacaddbfe3357cf1" "98fada4d13bcf1ff3a50fceb3ab1fea8619564bb01a8f744e5d22e8210bfff7b" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-document-title ((t (:inherit default :weight bold :foreground "#c5c8c6" :height 2.0 :underline nil))))
 '(org-level-1 ((t (:inherit default :weight bold :foreground "#c5c8c6" :height 1.75))))
 '(org-level-2 ((t (:inherit default :weight bold :foreground "#c5c8c6" :height 1.5))))
 '(org-level-3 ((t (:inherit default :weight bold :foreground "#c5c8c6" :height 1.25))))
 '(org-level-4 ((t (:inherit default :weight bold :foreground "#c5c8c6" :height 1.1))))
 '(org-level-5 ((t (:inherit default :weight bold :foreground "#c5c8c6"))))
 '(org-level-6 ((t (:inherit default :weight bold :foreground "#c5c8c6"))))
 '(org-level-7 ((t (:inherit default :weight bold :foreground "#c5c8c6"))))
 '(org-level-8 ((t (:inherit default :weight bold :foreground "#c5c8c6")))))
