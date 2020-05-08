(in-package :fwoar.cl-git)

(defun root-of (repo)
  (typecase repo
    (repository (root repo))
    ((or pathname string) (namestring
                           (truename repo)))))

(defun ref (repo id)
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
