(defpackage :fwoar.cl-git.branch-resolution
  (:use :cl )
  (:export ))
(in-package :fwoar.cl-git.branch-resolution)

(defclass fake-repository (fwoar.cl-git::repository)
  ())
(defclass fake-ref ()
  ((%repository :initarg :repository :reader repository)
   (%id :initarg :id :reader id)
   (%parents :initarg :parents :reader parents :initform ())))

(defparameter *expected-branches*
  '(("master" "ref1")
    ("other" "ref2")))

(defmethod fwoar.cl-git::repository ((object symbol))
  (fwoar.cl-git::resolve-repository object))
(defmethod fwoar.cl-git::resolve-repository fwoar.cl-git::alts :branch-resolution
    ((o (eql :branch-resolution)))
  (make-instance 'fake-repository :root "the-root"))
(defmethod fwoar.cl-git::branches ((repository fake-repository))
  *expected-branches*)
(defmethod fwoar.cl-git::ref ((repository fake-repository) id)
  (fw.lu:new 'fake-ref repository id))

(fiveam:def-suite :fwoar.cl-git.branch-resolution
  :description "testing branch resolution"
  :in :fwoar.cl-git)
(fiveam:in-suite :fwoar.cl-git.branch-resolution)

(fiveam:def-test simple ()
  (5am:is (typep (git:with-repository (:branch-resolution)
                   (git:repository))
                 'fake-repository))
  (5am:is (equal *expected-branches*
                 (git:with-repository (:branch-resolution)
                   (git:git (branches))))))
