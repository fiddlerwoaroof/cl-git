(in-package :fwoar.cl-git.blob)

(fwoar.cl-git.utils:defclass+ blob (fwoar.cl-git::git-object)
  ((%data :reader data :initarg :data)))

(defmethod -extract-object-of-type ((type (eql :blob)) s repository &key)
  (blob s))

(defcomponents blob (object _)
  ((eql :data) (data object)))
