(in-package :fwoar.cl-git)

(defparameter *object-data-lens*
  (data-lens.lenses:make-alist-lens :object-data))

(defclass pack ()
  ((%pack :initarg :pack :reader pack-file)
   (%index :initarg :index :reader index-file)
   (%repository :initarg :repository :reader repository)))

(defclass repository ()
  ((%root :initarg :root :reader root)))

(defclass git-object ()
  ())

(defgeneric object-type->sym (object-type)
  (:documentation "Canonicalizes different representations of an
  object type to their symbol representation."))

(defmethod object-type->sym ((o-t symbol))
    o-t)

(defmethod object-type->sym ((object-type number))
  (ecase object-type
    (1 :commit)
    (2 :tree)
    (3 :blob)
    (4 :tag)
    (6 :ofs-delta)
    (7 :ref-delta)))

(defmethod object-type->sym ((object-type string))
  (string-case:string-case ((string-downcase object-type))
    ("commit" :commit)
    ("tree" :tree)
    ("blob" :blob)
    ("tag" :tag)
    ("ofs-delta" :ofs-delta)
    ("ref-delta" :ref-delta)))

(defgeneric repository (root)
  (:method ((root string))
    (let ((root (parse-namestring root)))
      (repository root)))
  (:method ((root pathname))
    (let ((root (truename root)))
      (fw.lu:new 'repository root))))

(defun get-local-branches (root)
  (append (get-local-unpacked-branches root)
          (get-local-packed-branches root)))

(defun loose-object-path (sha)
  (let ((obj-path (fwoar.string-utils:insert-at 2 #\/ sha)))
    (merge-pathnames obj-path ".git/objects/")))

(defun pack (index pack repository)
  (fw.lu:new 'pack index pack repository))

(defun pack-files (repo)
  (mapcar (serapeum:op
            (pack _1
                  (merge-pathnames
                   (make-pathname :type "pack") _1)
                  (repository repo)))
          (uiop:directory*
           (merge-pathnames ".git/objects/pack/*.idx"
                            repo))))

(defgeneric loose-object (repository id)
  (:method ((repository string) id)
    (when (probe-file (merge-pathnames ".git" repository))
      (loose-object (repository repository) id)))
  (:method ((repository pathname) id)
    (when (probe-file (merge-pathnames ".git" repository))
      (loose-object (repository repository) id)))
  (:method ((repository repository) id)
    (car
     (uiop:directory*
      (merge-pathnames (loose-object-path (serapeum:concat id "*"))
                       (root repository))))))

(defun loose-object-p (repository id)
  "Is ID an ID of a loose object?"
  (loose-object repository id))

(defclass git-ref ()
  ((%repo :initarg :repo :reader ref-repo)
   (%hash :initarg :hash :reader ref-hash)))
(defclass loose-ref (git-ref)
  ((%file :initarg :file :reader loose-ref-file)))
(defclass packed-ref (git-ref)
  ((%pack :initarg :pack :reader packed-ref-pack)
   (%offset :initarg :offset :reader packed-ref-offset)))

(defmethod print-object ((obj git-ref) s)
  (print-unreadable-object (obj s :type t)
    (format s "~a of ~a"
            (subseq (ref-hash obj) 0 7)
            (serapeum:string-replace (namestring (user-homedir-pathname))
                                     (root-of (ref-repo obj))
                                     "~/"))))
