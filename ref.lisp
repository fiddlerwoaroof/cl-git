(in-package :fwoar.cl-git.ref)

(defclass ref ()
  ((%repo :initarg :repo :reader ref-repo)
   (%hash :initarg :hash :reader ref-hash)))
(defclass loose-ref (ref)
  ((%file :initarg :file :reader loose-ref-file)))

(defmethod print-object ((obj ref) s)
  (print-unreadable-object (obj s :type t :identity t)
    (format s "~a of ~a"
            (subseq (ref-hash obj) 0 6)
            (ref-repo obj)
            #+(or)
            (serapeum:string-replace (namestring (user-homedir-pathname))
                                     (root-of (ref-repo obj))
                                     "~/"))))

(defmethod fwoar.cl-git:component ((component (eql :hash)) (object ref))
  (ref-hash object))
