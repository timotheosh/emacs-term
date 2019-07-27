;;;; emacs-term.lisp

(in-package #:emacs-term)

(defparameter *socket-path* "/home/thawes/.emacs.d/server/server")

(defparameter *start-client*
  (concatenate 'string
               "emacsclient"
               " --socket-name=~A"
               " --frame-parameters='(quote (name . \"~A\"))'"
               " -cne \"(~A)\""))

(defparameter *raise-client*
  (concatenate 'string
               "wmctrl -R ~A"
               " &&"
               " wmctrl -r ~A -b toggle,maximized_vert,maximized_horz"))

(defun window-count (name)
  "Runs wmctrl to check if a given named window exists."
  (count-if-not 'null
                (cl-ppcre:all-matches-as-strings
                 name
                 (uiop:run-program '("wmctrl" "-l") :output :string))))

(defun run (name command)
  (let ((startup)))
  (when (zerop (window-count name))
    (uiop:run-program (format nil *start-client* *socket-path* name command)))
  (uiop:launch-program (format nil *raise-client* name name)))

(defun -main ()
  (run "Eshell" "eshell"))
