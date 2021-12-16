;;; pandoc-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "pandoc" "../../../../../../.emacs.d/elpa/pandoc-20161128.1157/pandoc.el"
;;;;;;  "9272a0d13dbaf0f2a37a00e6dc179d5b")
;;; Generated autoloads from ../../../../../../.emacs.d/elpa/pandoc-20161128.1157/pandoc.el

(autoload 'pandoc-antiword "pandoc" "\
Convert `FILE' to DocBook using Antiword.

\(fn FILE)" nil nil)

(autoload 'pandoc-convert-file "pandoc" "\
Convert `FILE-PATH' as `INPUT-FORMAT' to `OUTPUT-FORMAT'.

\(fn FILE-PATH INPUT-FORMAT OUTPUT-FORMAT)" nil nil)

(autoload 'pandoc-convert-stdio "pandoc" "\
Convert `BODY' as `INPUT-FORMAT' to `OUTPUT-FORMAT'.

\(fn BODY INPUT-FORMAT OUTPUT-FORMAT)" nil nil)

(autoload 'pandoc-open-eww "pandoc" "\
Render `FILE' using EWW and Pandoc.

\(fn FILE)" t nil)

(autoload 'pandoc-turn-on-advice-eww "pandoc" "\
When `eww-open-file' using Pandoc if the file is not HTML.

Remove advice if `ENABLE' equals `-1'.

\(fn &optional ENABLE)" nil nil)

;;;### (autoloads "actual autoloads are elsewhere" "pandoc" "../../../../../../.emacs.d/elpa/pandoc-20161128.1157/pandoc.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../../../.emacs.d/elpa/pandoc-20161128.1157/pandoc.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pandoc" '("pandoc-")))

;;;***

;;;***

;;;### (autoloads nil nil ("../../../../../../.emacs.d/elpa/pandoc-20161128.1157/pandoc-autoloads.el"
;;;;;;  "../../../../../../.emacs.d/elpa/pandoc-20161128.1157/pandoc.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; pandoc-autoloads.el ends here
