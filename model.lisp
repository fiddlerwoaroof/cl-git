(in-package :fwoar.cl-git)

(defparameter *object-data-lens*
  (data-lens.lenses:make-alist-lens :object-data))

(defclass pack ()
  ((%pack :initarg :pack :reader pack-file)
   (%index :initarg :index :reader index-file)))

(defclass repository ()
  ((%root :initarg :root :reader root)))

(defclass git-object ()
  ())

(defclass commit (git-object)
  ())

(defun object-type->sym (object-type)
  (ecase object-type
    (1 :commit)
    (2 :tree)
    (3 :blob)
    (4 :tag)
    (6 :ofs-delta)
    (7 :ref-delta)))

(defun repository (root)
  (fw.lu:new 'repository root))

(defun get-local-branches (root)
  (append (get-local-unpacked-branches root)
          (get-local-packed-branches root)))

(defun loose-object-path (sha)
  (let ((obj-path (fwoar.string-utils:insert-at 2 #\/ sha)))
    (merge-pathnames obj-path ".git/objects/")))

(defun pack (index pack)
  (fw.lu:new 'pack index pack))

(defun pack-files (repo)
  (mapcar 'pack
          (uiop:directory*
           (merge-pathnames ".git/objects/pack/*.idx"
                            repo))
          (uiop:directory*
           (merge-pathnames ".git/objects/pack/*.pack"
                            repo))))
