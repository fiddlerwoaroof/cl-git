(in-package :cl-user)

(defpackage :fwoar.cl-git
  (:use :cl )
  (:export
   #:ensure-ref
   #:repository
   #:component))

(defpackage :cl-git-user
  (:use :cl :fwoar.cl-git))

(defpackage :co.fwoar.git
  (:use)
  (:export #:show #:branch #:branches #:commit-parents #:in-repository
           #:with-repository #:current-repository #:show-repository #:git
           #:tree #:contents #:component #:rev-list #:repository
           #:parents #:filter-tree))
