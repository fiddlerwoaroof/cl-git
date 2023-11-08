(defpackage :fwoar.cl-git.package
  (:use :cl)
  (:export))
(in-package :fwoar.cl-git.package)

(defpackage :fwoar.cl-git.protocol
  (:use :cl)
  (:export #:-extract-object-of-type #:component #:defcomponents))

(defpackage :fwoar.cl-git.commit
  (:use :cl :fwoar.cl-git.protocol)
  (:export #:git-commit #:metadata #:data))

(defpackage :fwoar.cl-git.delta
  (:use :cl :fwoar.cl-git.protocol)
  (:export #:delta #:repository #:base #:commands #:src-size
           #:delta-size #:resolve-delta))

(defpackage :fwoar.cl-git.pack
  (:use :cl)
  (:export #:pack #:pack-file #:index-file #:idx-toc
           #:with-pack-streams #:seek-to-object-in-pack #:packed-ref
           #:packed-ref-pack #:packed-ref-offset #:extract-object #:git-ref
           #:loose-ref
           #:extract-object-at-pos))

(defpackage :fwoar.cl-git
  (:use :cl :fwoar.cl-git.protocol)
  (:import-from :fwoar.cl-git.commit #:git-commit)
  (:import-from :fwoar.cl-git.pack #:packed-ref)
  (:export #:ensure-ref #:repository #:*want-delta* #:git-object
           #:hash #:*git-encoding* #:git-commit #:ref #:component
           #:*git-repository* #:git-ref #:extract-object
           #:git-tree
           #:blob))

(defpackage :fwoar.cl-git.types
  (:use :cl )
  (:export #:octet))

(defpackage :cl-git-user
  (:use :cl :fwoar.cl-git))

(defpackage :co.fwoar.git
  (:use)
  (:export #:show #:branch #:branches #:commit-parents #:in-repository
           #:with-repository #:current-repository #:show-repository #:git
           #:tree #:contents #:component #:rev-list #:repository #:parents
           #:filter-tree))
