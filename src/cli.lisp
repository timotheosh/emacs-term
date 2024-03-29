;; cli.lisp
;; Module that handles all command line arguments.
(in-package #:emacs-term)

(defparameter *commands* '("eshell" "org-agenda" "org-todos" "multi-term" "buffer-menu"))

(defparameter *w-name* (format nil "~A@~A" (uiop:getenv "USER") (uiop:hostname)))

(defparameter *socket-path* "server")

(opts:define-opts
  (:name :help
   :description "print this help text"
   :short #\h
   :long "help")
  (:name :dired
   :description "Start a dired session within a running emacs."
   :short #\d
   :long "dired"
   :arg-parser #'identity
   :meta-var "PATH")
  (:name :command
   :description (format nil "Commands being one of ~{~A ~}~%" *commands*)
   :short #\c
   :long "command"
   :arg-parser #'identity
   :meta-var "COMMAND")
  (:name :socket
   :description (format nil "name of or full path to the emacs daemon socket file.")
   :short #\s
   :long "socket"
   :arg-parser #'identity
   :meta-var "SOCKET"))

(defun unknown-option (condition)
  (format t "warning: ~s option is unknown!~%" (opts:option condition))
  (invoke-restart 'opts:skip-option))

(defun set-dired (options)
  (let ((path (or (getf options :dired) (uiop:getenv "HOME"))))
    (list "Dired" (format nil "dired \"~A\"" path))))

(defun set-command (options)
  (let ((cmd (getf options :command)))
    (cond
      ((equal cmd "eshell")  (list "Eshell" "eshell" t))
      ((equal cmd "org-agenda") (list "Agenda" "org-agenda-list"))
      ((equal cmd "org-todos") (list "Todos" "org-todo-list"))
      ((equal cmd "multi-term") (list "Terminal" "multi-term-next"))
      ((equal cmd "multi-vterm") (list "Terminal" "multi-vterm"))
      ((equal cmd "buffer-menu") (list *w-name* "ibuffer"))
      ((equal cmd "dashboard") (list *w-name* "+doom-dashboard/open (selected-frame)"))
      (t '("Emacs-Term" "display-about-screen")))))

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
    (setf *socket-path* (or (getf options :socket) "server"))
    ;; Here all options are checked independently, it's trivial to code any
    ;; logic to process them.
    (cond ((getf options :help) (progn (opts:describe
                                        :prefix (format nil "emacs-term allows you to run emacs frames in an XWindow environment with some~% help from wmctrl, and xdotool")
                                        :usage-of "emacs-term")
                                       (opts:exit 1)))
          ((getf options :dired)  (set-dired options))
          ((getf options :command) (set-command options)))))
