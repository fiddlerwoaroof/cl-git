(in-package :fwoar.cl-git)

(defun root-of (repo)
  (typecase repo
    (repository (root repo))
    ((or pathname string) (namestring
                           (truename repo)))))

(defgeneric ref (repo id)
  (:documentation "Given a REPOsitory and a ref ID return the ref-id object.")
  (:method ((repo git-repository) (id string))
    (or (alexandria:when-let ((object-file (loose-object repo id)))
          (make-instance 'loose-ref
                         :repo repo
                         :hash (concatenate 'string
                                            (subseq id 0 2)
                                            (pathname-name object-file))
                         :file object-file))
        (packed-ref repo id))))

(defvar *ref-intern-table*
  (make-hash-table :test 'equal #+sbcl :weakness #+sbcl :key-and-value))

(defun ensure-ref (thing &optional (repo *git-repository*))
  (typecase thing
    (fwoar.cl-git.ref:ref thing)
    (t (alexandria:when-let ((maybe-result (ref repo thing)))
         (alexandria:ensure-gethash (component :hash maybe-result)
                                    *ref-intern-table*
                                    maybe-result)))))

(defun ensure-repository (thing)
  (repository thing))
