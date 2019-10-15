;; cli.lisp
;; Module that handles all command line arguments.
(in-package #:emacs-term)

(defparameter *commands* '("eshell" "org-agenda" "org-todos"))

(opts:define-opts
  (:name :help
         :description "print this help text"
         :short #\h
         :long "help")
  (:name :command
         :description (format nil "Commands being one of ~{~A ~}~%" *commands*)
         :short #\c
         :long "command"
         :arg-parser #'identity
         :meta-var "COMMAND"))

(defun unknown-option (condition)
  (format t "warning: ~s option is unknown!~%" (opts:option condition))
  (invoke-restart 'opts:skip-option))

(defmacro when-option ((options opt) &body body)
  `(let ((it (getf ,options ,opt)))
     (when it
       ,@body)))

(defun parse-args (&rest args)
  (multiple-value-bind (options args)
      (handler-case
          (handler-bind ((opts:unknown-option #'unknown-option))
            (opts:get-opts))
        (opts:missing-arg (condition)
          (format t "fatal: option ~s needs an argument!~%"
                  (opts:option condition)))
        (opts:arg-parser-failed (condition)
          (format t "fatal: cannot parse ~s as argument of ~s~%"
                  (opts:raw-arg condition)
                  (opts:option condition))))
    ;; Here all options are checked independently, it's trivial to code any
    ;; logic to process them.
    (when-option (options :help)
                 (opts:describe
                  :prefix (format nil "emacs-term allows you to run emacs frames in an XWindow environment with some~% help from wmctrl, and xdootool")
                  :usage-of "emacs-term")
                 (opts:exit 1))
    (when-option (options :command)
                 (let ((cmd (getf options :command)))
                   (cond
                     ((equal cmd "eshell")  '("Eshell" "eshell"))
                     ((equal cmd "org-agenda") '("Agenda" "org-agenda-list"))
                     ((equal cmd "org-todos") '("Todos" "org-todo-list"))
                     ((equal cmd "multi-term") '("Multi-Term" "multi-term-next"))
                     (t '("Emacs" "")))))))
