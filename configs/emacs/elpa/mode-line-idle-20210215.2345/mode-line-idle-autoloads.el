;;; mode-line-idle-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "mode-line-idle" "mode-line-idle.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from mode-line-idle.el

(autoload 'mode-line-idle "mode-line-idle" "\
Delayed evaluation of CONTENT, delayed by DELAY-IN-SECONDS.

Argument KEYWORDS is a property list of optional keywords:

- `:interrupt' When non-nil, interrupt evaluation on keyboard input
  (use for long running actions).
- `:literal' When non-nil, replace `%' with `%%',
  to prevent `mode-line-format' from formatting these characters.

\(fn DELAY-IN-SECONDS CONTENT DEFAULT-TEXT &rest KEYWORDS)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mode-line-idle" '("mode-line-idle--")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; mode-line-idle-autoloads.el ends here
