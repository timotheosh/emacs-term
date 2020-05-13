;;;; -term.lisp
;; This program depends on wmctrl, xdotool, and xprop to be on the path
;; You can install it on Ubuntu/Debian with:
;;   sudo apt install wmctrl xdootool x11-tools
;;
(in-package #:emacs-term)

(defparameter *socket-path* "emacs1")

(defparameter *start-client*
  (concatenate 'string
               "emacsclient"
               " --socket-name=~A"
               " --frame-parameters='(quote (name . \"~A\"))'"
               " -cne '(~A)'"))

(defun trim-all (str)
  (string-trim '(#\Space #\Newline #\Backspace #\Tab
                 #\Linefeed #\Page #\Return #\Rubout)
               str))

(defun get-window-id (window-name)
  (multiple-value-bind (window-id stderr status)
      (uiop:run-program (format nil "xdotool search --classname ~A" window-name)
                        :output :string
                        :ignore-error-status t)
    (let ((id (trim-all window-id)))
      (if (zerop (length id))
          nil
          (first (cl-ppcre:split "\\s+" id))))))

(defun get-window-id-by-name (window-name)
  (multiple-value-bind (window-id stderr status)
      (uiop:run-program (format nil "xdotool search --name ~A" window-name)
                        :output :string
                        :ignore-error-status t)
    (let ((id (trim-all window-id)))
      (if (zerop (length id))
          nil
          (first (cl-ppcre:split "\\s+" id))))))

(defun desktop-geometry ()
  "Returns the desktop geometry as a string (W H). Assumes all
workspaces are the same geometry."
  (multiple-value-bind (geom stderr status)
      (uiop:run-program '("xdotool" "getdisplaygeometry") :output :string
                        :ignore-error-status t)
    (mapcar 'parse-integer (cl-ppcre:split "\\s+" (trim-all geom)))))

(defun window-geometry (name)
  "Returns the current geometry of the given window by name."
  (multiple-value-bind (line backref)
      (multiple-value-bind (results stderr status)
          (uiop:run-program (format nil "xdotool getwindowgeometry ~A"
                                    (get-window-id name))
                            :output :string :ignore-error-status t)
        (cl-ppcre:scan-to-strings "Geometry: ([0-9]*x?[0-9]*)"
                                  (trim-all results)))
    (mapcar 'parse-integer
            (cl-ppcre:split "x" (elt backref 0)))))

(defun maximize-client (name)
  "Maximizes window by name."
  (uiop:run-program
   (format nil "wmctrl -xr ~A -b toggle,maximized_vert,maximized_horz" name)
   :ignore-error-status t))

(defun minimize (name)
  "Minimizes window by name"
  (let ((window-id (get-window-id name)))
    (uiop:run-program
     (format nil "xdotool windowminimize ~A" window-id)
     :ignore-error-status t)))

(defun maximized-p (name)
  "Returns true if client is already maximized."
  (let ((desktop (desktop-geometry))
        (window (window-geometry name)))
    (and (< (* (first desktop) 0.87) (first window))
         (< (* (second desktop) 0.87) (second window)))))

(defun maximize (name)
  "Will make sure window, selected by name, is maximized. Maximize is
  a toggle, so try to make sure window is not already maximized."
  (when (null (maximized-p name))
    (maximize-client name)))

(defun start-window (name command)
  "Starts the window with name and command, waits a maximum of 5
  seconds for window to open for window to open."
  (uiop:run-program (format nil *start-client* *socket-path* name command)
                    :ignore-error-status t)
  (loop for n from 0 below 20
     until (plusp (length (get-window-id-by-name name))) do
       (sleep 0.25))
  (multiple-value-bind (in out rv)
      (uiop:run-program (format nil "xprop -name ~A -f WM_CLASS 8s -set WM_CLASS \"~A\"" name name)) (when (zerop rv) t)))

(defun skip-taskbar (name)
  (uiop:run-program (format nil "wmctrl -xr ~A -b toggle,skip_taskbar" name)
                    :ignore-error-status t))

(defun active-window ()
  "Returns the ID of the active window"
  (multiple-value-bind (name err status)
      (uiop:run-program
       '("xdotool" "getactivewindow" "getwindowname")
       :output :string :ignore-error-status t)
    (string-trim '(#\Space #\Tab #\Newline) name)))

(defun window-active-p (name)
  (string-equal name (active-window)))

(defun kill-window (name)
  (multiple-value-bind (stdout stderr status)
      (uiop:run-program (format nil "wmctrl -xc ~A" name)
                        :ignore-error-status t)
    (zerop status)))

(defun raise-client (name)
  "Raises the given window by name."
  (uiop:run-program
   (format nil "wmctrl -xR ~A" name) :ignore-error-status t)
  (loop for n from 0 below 20
     until (not (null (window-active-p name))) do
       (sleep 0.25)))

(defun run (name command &optional (hide nil))
  (if (window-active-p name)
      (if (maximized-p name)
          (minimize name)
          (maximize name))
      (progn
        (when  (null (get-window-id name))
          (start-window name command)
          (when hide
            (skip-taskbar name)))
        (raise-client name)
        (maximize name))))

(defun -main (&rest args)
  (let ((cmd (parse-args args)))
    (let ((name (first cmd))
          (command (second cmd))
          (hide (third cmd)))
      (run name command hide))))

