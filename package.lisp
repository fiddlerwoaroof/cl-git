(defpackage :fwoar.cl-git.package
  (:use :cl)
  (:export))
(in-package :fwoar.cl-git.package)

(defpackage :fwoar.cl-git.protocol
  (:use :cl)
  (:export #:-extract-object-of-type #:component #:defcomponents))

(defpackage :fwoar.cl-git.commit
  (:use :cl :fwoar.cl-git.protocol)
  (:export #:git-commit))

(defpackage :fwoar.cl-git
  (:use :cl :fwoar.cl-git.protocol)
  (:import-from :fwoar.cl-git.commit #:git-commit)
  (:export #:ensure-ref #:repository #:*want-delta* #:git-object
           #:hash #:*git-encoding* #:git-commit))

(defpackage :fwoar.cl-git.types
  (:use :cl )
  (:export
   #:octet))

(defpackage :cl-git-user
  (:use :cl :fwoar.cl-git))

(defpackage :co.fwoar.git
  (:use)
  (:export #:show #:branch #:branches #:commit-parents #:in-repository
           #:with-repository #:current-repository #:show-repository #:git
           #:tree #:contents #:component #:rev-list #:repository
           #:parents #:filter-tree))
