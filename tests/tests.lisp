(defpackage :fwoar.cl-git.tests
  (:use :cl )
  (:export ))
(in-package :fwoar.cl-git.tests)

(5am:def-suite :fwoar.cl-git
  :description "tests of cl-git")

(fiveam:def-suite :fwoar.cl-git.git-objects
  :description "testing branch resolution"
  :in :fwoar.cl-git)
