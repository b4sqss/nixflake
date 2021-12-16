;;; 0x0-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "0x0" "0x0.el" (0 0 0 0))
;;; Generated autoloads from 0x0.el

(autoload '0x0-upload-file "0x0" "\
Choose FILE and upload it to SERVER.

\(fn SERVER FILE)" t nil)

(autoload '0x0-upload-text "0x0" "\
Upload full or region text from the current buffer to SERVER.

\(fn SERVER)" t nil)

(autoload '0x0-upload-kill-ring "0x0" "\
Upload content from the \"kill-ring\" to SERVER.

\(fn SERVER)" t nil)

(autoload '0x0-shorten-uri "0x0" "\
Shorten the given URI with SERVER.

\(fn SERVER URI)" t nil)

(autoload '0x0-popup "0x0" "\
Create a new buffer, fill it with content, and upload it to SERVER later.

\(fn SERVER)" t nil)

(autoload '0x0-dwim "0x0" "\
Try to guess what to upload to SERVER.

\(fn SERVER)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "0x0" '("0x0-")))

;;;***

(provide '0x0-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; 0x0-autoloads.el ends here
