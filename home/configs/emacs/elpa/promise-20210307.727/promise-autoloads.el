;;; promise-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "promise" "../../../../../../.emacs.d/elpa/promise-20210307.727/promise.el"
;;;;;;  "9b7bd7209831df12d6323864a228749c")
;;; Generated autoloads from ../../../../../../.emacs.d/elpa/promise-20210307.727/promise.el

(autoload 'promise-chain "promise" "\
Extract PROMISE, BODY include then, catch, done and finally.

Extract the following code...

    (promise-chain (promise-new ...)
      (then
       (lambda (value)
         ...))

      (catch
       (lambda (reason)
         ...))

      (done
       (lambda (value)
         ...))

      (finally
       (lambda () ...))

      ;; Anaphoric versions of `then' and `catch'.

      (thena (message \"result -> %s\" result)
             ...)

      (catcha (message \"error: reason -> %s\" reason)
              ...))

as below.

    (let ((promise (promise-new ...)))
      (setf promise (promise-then promise
                                  (lambda (value)
                                    ...)))

      (setf promise (promise-catch promise
                                   (lambda (value)
                                     ...)))

      (setf promise (promise-done promise
                                  (lambda (reason)
                                    ...)))

      (setf promise (promise-finally promise
                                     (lambda ()
                                       ...)))

      (setf promise (promise-then promise
                                  (lambda (result)
                                    (message \"result -> %s\" result)
                                    ...)))

      (setf promise (promise-catch promise
                                   (lambda (reason)
                                     (message \"error: reason -> %s\" reason)
                                     ...)))
      promise)

\(fn PROMISE &rest BODY)" nil t)

(function-put 'promise-chain 'lisp-indent-function '1)

;;;### (autoloads "actual autoloads are elsewhere" "promise" "../../../../../../.emacs.d/elpa/promise-20210307.727/promise.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../../../.emacs.d/elpa/promise-20210307.727/promise.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "promise" '("promise")))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "promise-core"
;;;;;;  "../../../../../../.emacs.d/elpa/promise-20210307.727/promise-core.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../../../.emacs.d/elpa/promise-20210307.727/promise-core.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "promise-core" '("promise-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "promise-es6-extensions"
;;;;;;  "../../../../../../.emacs.d/elpa/promise-20210307.727/promise-es6-extensions.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../../../.emacs.d/elpa/promise-20210307.727/promise-es6-extensions.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "promise-es6-extensions" '("promise-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "promise-rejection-tracking"
;;;;;;  "../../../../../../.emacs.d/elpa/promise-20210307.727/promise-rejection-tracking.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../../../.emacs.d/elpa/promise-20210307.727/promise-rejection-tracking.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "promise-rejection-tracking" '("promise-")))

;;;***

;;;### (autoloads nil nil ("../../../../../../.emacs.d/elpa/promise-20210307.727/promise-autoloads.el"
;;;;;;  "../../../../../../.emacs.d/elpa/promise-20210307.727/promise-core.el"
;;;;;;  "../../../../../../.emacs.d/elpa/promise-20210307.727/promise-done.el"
;;;;;;  "../../../../../../.emacs.d/elpa/promise-20210307.727/promise-es6-extensions.el"
;;;;;;  "../../../../../../.emacs.d/elpa/promise-20210307.727/promise-finally.el"
;;;;;;  "../../../../../../.emacs.d/elpa/promise-20210307.727/promise-pkg.el"
;;;;;;  "../../../../../../.emacs.d/elpa/promise-20210307.727/promise-rejection-tracking.el"
;;;;;;  "../../../../../../.emacs.d/elpa/promise-20210307.727/promise.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; promise-autoloads.el ends here
