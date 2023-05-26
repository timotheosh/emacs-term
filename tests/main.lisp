(defpackage emacs-term/tests
  (:use :cl
        :emacs-term
        :fiveam))
(in-package :emacs-term/tests)

;; NOTE: To run this test file, execute `(asdf:test-system :emacs-term)' in your Lisp.
;; 

(def-suite all-tests
  :description "The master suite of all run-testing tests.")

(in-suite all-tests)

(test dummy-tests
  "Some fake tests. You should add your own i.e. (is (= (emacs-term::some-function 2) 5)"
  (is (listp (list 1 2)))
  (is (= 5 (+ 2 3))))
  
