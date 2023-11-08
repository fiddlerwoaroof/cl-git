(in-package :fwoar.cl-git)

(defparameter *object-data-lens*
  (data-lens.lenses:make-alist-lens :object-data))

(defclass repository ()
  ((%root :initarg :root :reader root)))
(defclass git-repository (repository)
  ())

(defclass git-object ()
  ((%hash :initarg :hash :accessor hash)))

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

(define-condition alts-fallthrough (error)
  ((%fallthrough-message :initarg :fallthrough-message :reader fallthrough-message)
   (%args :initarg :args :reader args))
  (:report (lambda (c s)
             (format s "~a ~s"
                     (fallthrough-message c)
                     (args c)))))

;; TODO: figure out how to handle ambiguity? restarts?
(define-method-combination alts (&key fallthrough-message) ((methods *))
  (:arguments arg)
  (progn
    (mapc (serapeum:op
            (let ((qualifiers (method-qualifiers _1)))
              (unless (and (eql 'alts (car qualifiers))
                           (if (null (cdr qualifiers))
                               t
                               (and (symbolp (cadr qualifiers))
                                    (null (cddr qualifiers)))))
                (invalid-method-error _1 "invalid qualifiers: ~s" qualifiers))))
          methods)
    `(or ,@(mapcar (serapeum:op `(call-method ,_1))
                   methods)
         (error 'alts-fallthrough
                :fallthrough-message ,fallthrough-message
                :args ,arg))))

(defgeneric resolve-repository (object)
  (:documentation "resolve an OBJECT to a repository implementation")
  (:method-combination alts :fallthrough-message "failed to resolve repository"))

(defmethod resolve-repository alts :git ((root pathname))
  (alexandria:when-let ((root (probe-file root)))
    (let* ((git-dir (merge-pathnames (make-pathname :directory '(:relative ".git"))
                                     root)))
      (when (probe-file git-dir)
        (fw.lu:new 'git-repository root)))))

(defgeneric repository (object)
  (:documentation "get the repository for an object")
  (:method ((root repository))
    root)
  (:method ((root pathname))
    (resolve-repository root))
  (:method ((root string))
    (let ((root (parse-namestring root)))
      (repository root))))

(defun get-local-branches (root)
  (append (get-local-unpacked-branches root)
          (get-local-packed-branches root)))

(defun loose-object-path (sha)
  (let ((obj-path (fwoar.string-utils:insert-at 2 #\/ sha)))
    (merge-pathnames obj-path ".git/objects/")))

(defgeneric pack-files (repo)
  (:method ((repo git-repository))
    (mapcar (serapeum:op
              (fwoar.cl-git.pack:pack _1
                                      (merge-pathnames
                                       (make-pathname :type "pack") _1)
                                      repo))
            (uiop:directory*
             (merge-pathnames ".git/objects/pack/*.idx"
                              (root-of repo))))))

(defgeneric loose-object (repository id)
  (:method ((repository string) id)
    (when (probe-file (merge-pathnames ".git" repository))
      (loose-object (repository repository)
                    id)))
  (:method ((repository pathname) id)
    (when (probe-file (merge-pathnames ".git" repository))
      (loose-object (repository repository)
                    id)))
  (:method ((repository repository) id)
    (car
     (uiop:directory*
      (merge-pathnames (loose-object-path (serapeum:concat id "*"))
                       (root repository))))))

(defun loose-object-p (repository id)
  "Is ID an ID of a loose object?"
  (loose-object repository id))

(defmethod component ((component (eql :hash)) (object git-object))
  (hash object))

(fw.lu:defclass+ blob (fwoar.cl-git::git-object)
  ((%data :reader data :initarg :data)))
(defmethod -extract-object-of-type ((type (eql :blob)) s repository &key)
  (blob s))
