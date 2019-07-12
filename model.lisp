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

(defclass commit (git-object)
  ())

(defgeneric object-type->sym (object-type)
  (:method ((o-t symbol))
    o-t))

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
    (fw.lu:new 'repository root))
  (:method ((root pathname))
    (fw.lu:new 'repository root)))

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

(defclass git-object ()
  ((%repo :initarg :repo :reader object-repo)
   (%hash :initarg :hash :reader object-hash)))
(defclass loose-object (git-object)
  ((%file :initarg :file :reader loose-object-file)))
(defclass packed-object (git-object)
  ((%pack :initarg :pack :reader packed-object-pack)
   (%offset :initarg :offset :reader packed-object-offset)))

(defmethod print-object ((obj git-object) s)
  (print-unreadable-object (obj s :type t)
    (format s "~a of ~a"
            (subseq (object-hash obj) 0 7)
            (serapeum:string-replace (namestring (user-homedir-pathname))
                                     (root-of (object-repo obj))
                                     "~/"))))
