;;;; emacs-term.lisp
;; This program depends on wmctrl to be on the path
;; You can install it on Ubuntu/Debian with:
;;   sudo apt install wmctrl
;;
(in-package #:emacs-term)

(defparameter *socket-path* "/home/thawes/.emacs.d/server/server")

(defparameter *start-client*
  (concatenate 'string
               "emacsclient"
               " --socket-name=~A"
               " --frame-parameters='(quote (name . \"~A\"))'"
               " -cne \"(~A)\""))

(defun desktop-geometry ()
  "Returns the desktop geometry as a string (W H). Assumes all
workspaces are the same geometry."
  (mapcar 'parse-integer
          (cl-ppcre:split
           "x"
           (fourth (cl-ppcre:split "\\s+"
                                   (uiop:run-program "wmctrl -d" :output :string))))))

(defun window-geometry (name)
  "Returns the current geometry of the given window by name."
  (let ((results
         (cl-ppcre:split
          "\\s+"
          (uiop:run-program
           (format nil "wmctrl -lG |grep ~A" name) :output :string))))
    (mapcar 'parse-integer (list (nth 4 results) (nth 5 results)))))

(defun maximize-client (name)
  "Maximizes window by name."
  (uiop:run-program
   (format nil "wmctrl -r ~A -b toggle,maximized_vert,maximized_horz" name)))

(defun maximize (name)
  "Will make sure window, selected by name, is maximized. Maximize is
  a toggle, so try to make sure window is not already maximized."
  (let ((desktop (desktop-geometry))
        (window (window-geometry name)))
    (when (or (> (- (first desktop) (first window)) 70)
              (> (- (second desktop) (second window)) 70))
      (maximize-client name))))

(defun window-count (name)
  "Runs wmctrl to check if a given named window exists."
  (count-if-not 'null
                (cl-ppcre:all-matches-as-strings
                 name
                 (uiop:run-program '("wmctrl" "-l") :output :string))))

(defun start-window (name command)
  "Starts the window with name and command, waits a maximum of 5
  seconds for window to open for window to open."
  (uiop:run-program (format nil *start-client* *socket-path* name command))
  (loop for n from 0 below 20
     until (plusp (window-count name)) do
       (sleep 0.25)))

(defun active-window ()
  "Returns the ID of the active window"
  (multiple-value-bind (name err status)
      (uiop:run-program
       '("xdotool" "getactivewindow" "getwindowname") :output :string)
    (string-trim '(#\Space #\Tab #\Newline) name)))

(defun eshell-active-p (name)
  (string-equal name (active-window)))

(defun kill-window (name)
  (uiop:run-program (format nil "wmctrl -c ~A" name)))

(defun raise-client (name)
  "Raises the given window by name."
  (uiop:run-program (format nil "wmctrl -R ~A" name)))

(defun run (name command)
  (if (eshell-active-p name)
      (kill-window name)
      (progn(when (zerop (window-count name))
              (start-window name command))
            (raise-client name)
            (maximize name))))

(defun -main ()
  (run "Eshell" "eshell"))
