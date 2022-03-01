;;; i3wm-config-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "i3wm-config-mode" "i3wm-config-mode.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from i3wm-config-mode.el

(autoload 'i3wm-config-mode "i3wm-config-mode" "\


\(fn)" t nil)

(add-to-list 'auto-mode-alist '("i3/config\\'" . i3wm-config-mode))

(register-definition-prefixes "i3wm-config-mode" '("i3wm-config-font-lock-keywords"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8-emacs-unix
;; End:
;;; i3wm-config-mode-autoloads.el ends here
