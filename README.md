# emacs-term
Simple shell program written in Common Lisp, that open Emacs as a terminal.

You can map a key in your favorite window manager or desktop
environment and this will open an Emacs window

## Requirements
This program uses emacsclient program to launch eshell. If you are
going to use this program as is, you'll need to change the top line in
emacs-term.lisp for the path to your socket file for your running
Emacs. You could substitute this with executing Emacs each time.

This program makes external commands to wmctrl and xdotool. Both can
be installed on an Ubuntu/Debian system with:

``` shellsession
sudo apt install wmctrl xdotool
```

This program will not work without them.
