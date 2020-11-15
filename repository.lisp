(in-package :fwoar.cl-git)

(defun root-of (repo)
  (typecase repo
    (repository (root repo))
    ((or pathname string) (namestring
                           (truename repo)))))

(defun ref (repo id)
  "Given a REPOsitory and a ref ID return the ref-id object."
  (let ((repo-root (root-of repo)))
    (or (alexandria:when-let ((object-file (loose-object repo id)))
          (make-instance 'loose-ref
                         :repo repo-root
                         :hash id
                         :file object-file))
        (multiple-value-bind (pack offset) (find-object-in-pack-files repo-root id)
          (when pack
            (make-instance 'packed-ref
                           :hash id
                           :repo repo-root
                           :offset offset
                           :pack pack))))))

(defun ensure-ref (thing &optional (repo *git-repository*))
  (typecase thing
    (git-ref thing)
    (t (ref repo thing))))

(defun ensure-repository (thing)
  (etypecase thing
    (repository thing)
    (string (repository thing))
    (pathname (repository thing))))
