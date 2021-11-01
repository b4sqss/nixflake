;;; eshell-vterm-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "eshell-vterm" "eshell-vterm.el" (0 0 0 0))
;;; Generated autoloads from eshell-vterm.el

(defvar eshell-vterm-mode nil "\
Non-nil if Eshell-Vterm mode is enabled.
See the `eshell-vterm-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `eshell-vterm-mode'.")

(custom-autoload 'eshell-vterm-mode "eshell-vterm" nil)

(autoload 'eshell-vterm-mode "eshell-vterm" "\
Use Vterm for eshell visual commands.

If called interactively, enable Eshell-Vterm mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "eshell-vterm" '("eshell-vterm-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; eshell-vterm-autoloads.el ends here
