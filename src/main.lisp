;;;; -term.lisp
;; This program depends on wmctrl, xdotool, and xprop to be on the path
;; You can install it on Ubuntu/Debian with:
;;   sudo apt install wmctrl xdootool x11-tools
;;
(defpackage #:emacs-term
  (:use #:cl)
  (:export :-main :parse-args))

(in-package #:emacs-term)

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

(defun window-ids-with-class (class-name)
  "Returns the list of ids of all windows currently matching
  class-name in WM_CLASS."
  (multiple-value-bind (output stderr status)
      (uiop:run-program (format nil "xdotool search --classname ~A" class-name)
                        :output :string
                        :ignore-error-status t)
    (declare (ignore stderr status))
    (let ((trimmed (trim-all output)))
      (unless (zerop (length trimmed))
        (cl-ppcre:split "\\s+" trimmed)))))

(defun desktop-geometry ()
  "Returns the desktop geometry as a string (W H). Assumes all
workspaces are the same geometry."
  (multiple-value-bind (geom stderr status)
      (uiop:run-program '("xdotool" "getdisplaygeometry") :output :string
                        :ignore-error-status t)
    (mapcar 'parse-integer (cl-ppcre:split "\\s+" (trim-all geom)))))

(defun window-geometry (name)
  "Returns the current geometry of the given window by name as a list
  of (width height), or nil if no such window exists or its geometry
  can't be determined."
  (let ((window-id (get-window-id name)))
    (when window-id
      (multiple-value-bind (results stderr status)
          (uiop:run-program (format nil "xdotool getwindowgeometry ~A" window-id)
                            :output :string :ignore-error-status t)
        (declare (ignore stderr status))
        (multiple-value-bind (line backref)
            (cl-ppcre:scan-to-strings "Geometry: ([0-9]*x?[0-9]*)"
                                      (trim-all results))
          (declare (ignore line))
          (when (plusp (length backref))
            (mapcar 'parse-integer (cl-ppcre:split "x" (elt backref 0)))))))))

(defun maximize-client (name)
  "Maximizes window by name.

  Sets maximized_vert and maximized_horz as two separate wmctrl calls
  rather than one combined toggle action: under KWin, a single
  \"-b toggle,maximized_vert,maximized_horz\" call was observed to only
  reliably apply one of the two flags, leaving the window maximized in
  one dimension only."
  (uiop:run-program
   (format nil "wmctrl -xr ~A -b add,maximized_vert" name)
   :ignore-error-status t)
  (uiop:run-program
   (format nil "wmctrl -xr ~A -b add,maximized_horz" name)
   :ignore-error-status t))

(defun minimize (name)
  "Minimizes window by name"
  (let ((window-id (get-window-id name)))
    (uiop:run-program
     (format nil "xdotool windowminimize ~A" window-id)
     :ignore-error-status t)))

(defun maximized-p (name)
  "Returns true if client is already maximized. Returns nil (not
  maximized) if the window can't be found."
  (let ((desktop (desktop-geometry))
        (window (window-geometry name)))
    (and window
         (< (* (first desktop) 0.87) (first window))
         (< (* (second desktop) 0.87) (second window)))))

(defun maximize (name)
  "Will make sure window, selected by name, is maximized. Maximize is
  a toggle, so try to make sure window is not already maximized."
  (when (null (maximized-p name))
    (maximize-client name)))

(defun start-window (name command)
  "Starts the window with name and command, waits a maximum of 5
  seconds for a new Emacs window to appear, then tags it with WM_CLASS
  name so later lookups by name succeed.

  The new window is identified by diffing the set of Emacs window ids
  before and after launch, rather than by matching its title: Doom's
  frame-title-format can override the frame's name in the title bar,
  so title text is not a reliable identifier.

  A nonzero exit status from emacsclient does not necessarily mean no
  frame was created -- e.g. an error partway through the requested
  elisp command still leaves the already-created frame on screen -- so
  window discovery always runs regardless of emacsclient's exit
  status. The exit status is only consulted for diagnostics, and only
  once polling has confirmed no new window ever appeared."
  (let ((known-ids (window-ids-with-class "emacs")))
    (multiple-value-bind (stdout stderr status)
        (uiop:run-program (format nil *start-client* *socket-path* name command)
                          :output :string :error-output :string
                          :ignore-error-status t)
      (declare (ignore stdout))
      (let ((window-id (loop for n from 0 below 20
                             for new-ids = (set-difference
                                            (window-ids-with-class "emacs")
                                            known-ids :test #'string=)
                             until new-ids do (sleep 0.25)
                             finally (return (first new-ids)))))
        (cond
          (window-id
           (multiple-value-bind (in out rv)
               (uiop:run-program
                (format nil "xprop -id ~A -f WM_CLASS 8s -set WM_CLASS \"~A\"" window-id name)
                :ignore-error-status t)
             (declare (ignore in out))
             (when (zerop rv) t)))
          ((not (zerop status))
           (format *error-output* "emacs-term: emacsclient failed for socket ~S:~%~A~%"
                   *socket-path* stderr)
           nil)
          (t
           (format *error-output*
                   "emacs-term: no new Emacs window appeared within 5 seconds~%")
           nil))))))

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

