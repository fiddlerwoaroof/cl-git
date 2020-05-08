(in-package :fwoar.cl-git)

(defclass git-tree (git-object)
  ((%entries :initarg :entries :reader entries)))

(defun git-tree (entries)
  (fw.lu:new 'git-tree entries))

(defclass tree-entry ()
  ((%mode :initarg :mode :reader te-mode)
   (%name :initarg :name :reader te-name)
   (%hash :initarg :hash :reader te-hash)))

(defun tree-entry (name mode hash)
  (fw.lu:new 'tree-entry name mode hash))

(defmethod print-object ((o tree-entry) s)
  (if *print-readably*
      (format s "#.(~s ~s ~s ~s)"
              'tree-entry
              (te-name o)
              (te-mode o)
              (te-hash o))
      (print-unreadable-object (o s :type t :identity t)
        (format s "(~a: ~a)"
                (te-name o)
                (subseq (te-hash o) 0 7)))))

(defun parse-tree-entry (data)
  (values-list (partition 0 data :with-offset 20)))

(defun format-tree-entry (entry)
  (destructuring-bind (info sha) (partition 0 entry)
    (destructuring-bind (mode name) (partition #\space
                                               (babel:octets-to-string info :encoding *git-encoding*))
      (tree-entry name mode (elt (->sha-string sha) 0)))))

(defun tree-entries (data &optional accum)
  (if (<= (length data) 0)
      (nreverse accum)
      (multiple-value-bind (next rest) (parse-tree-entry data)
        (tree-entries rest
                      (list* (format-tree-entry next)
                             accum)))))

(defmethod -extract-object-of-type ((type (eql :tree)) s repository &key)
  (git-tree (tree-entries s)))

(defmethod component ((component (eql :entries)) (object git-tree))
  (entries object))
(defmethod component ((component string) (object git-tree))
  (remove component (entries object)
          :test-not #'equal
          :key 'te-name))
(defmethod component ((component pathname) (object git-tree))
  (remove-if-not (lambda (it)
                   (pathname-match-p it component))
                 (entries object)
                 :key 'te-name))

(defmethod component ((component (eql :name)) (object tree-entry))
  (te-name object))
(defmethod component ((component (eql :mode)) (object tree-entry))
  (te-mode object))
(defmethod component ((component (eql :hash)) (object tree-entry))
  (te-hash object))
