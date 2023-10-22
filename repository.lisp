(in-package :fwoar.cl-git)

(defun root-of (repo)
  (typecase repo
    (repository (root repo))
    ((or pathname string) (namestring
                           (truename repo)))))

(defun packed-ref (repo id)
  (multiple-value-bind (pack offset sha) (find-object-in-pack-files repo id)
    (when pack
      (make-instance 'packed-ref
                     :hash sha
                     :repo repo
                     :offset offset
                     :pack pack))))

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

(defun ensure-ref (thing &optional (repo *git-repository*))
  (typecase thing
    (git-ref thing)
    (t (ref repo thing))))

(defun ensure-repository (thing)
  (repository thing))
