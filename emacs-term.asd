;;;; emacs-term.asd

(asdf:defsystem #:emacs-term
  :description "Describe emacs-term here"
  :author "Tim Hawes <tim@selfdidactic.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-ppcre #:memoize)
  :components ((:file "package")
               (:file "emacs-term"))
  :build-operation "asdf:program-op"
  :build-pathname "target/emacs-term"
  :entry-point "emacs-term:-main")
