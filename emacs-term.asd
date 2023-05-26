(defsystem "emacs-term"
  :version "0.1.0"
  :author "Tim Hawes <trhawes@gmail.com>"
  :license "MIT"
  :depends-on ("cl-ppcre"
               "unix-opts")
  :components ((:module "src"
                :components
                ((:file "main")
		 (:file "cli"))))
  :description "A program for managing Emacs in an X Windows environment."
  :build-operation "asdf:program-op"
  :build-pathname "target/emacs-term"
  :entry-point "emacs-term:-main"
  :in-order-to ((test-op (test-op "emacs-term/tests"))))

(defsystem "emacs-term/tests"
  :author "Tim Hawes <trhawes@gmail.com>"
  :license "MIT"
  :depends-on ("emacs-term"
               "fiveam")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for emacs-term"
  :perform (test-op (op c) (symbol-call :fiveam :run! (find-symbol* :all-tests :emacs-term/tests))))
