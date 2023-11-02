(defpackage :co.fwoar.cl-git.graph
  (:use :cl :fwoar.cl-git)
  (:export ))
(in-package :co.fwoar.cl-git.graph)

(defclass git-graph ()
  ((%repo :initarg :repo :reader repo)
   (%depth :initarg :depth :reader depth)
   (%stops :initarg :stops :reader stops :initform ())
   (%branches :reader branches)
   (%node-cache :reader node-cache :initform (make-hash-table :test 'equal))
   (%edge-cache :reader edge-cache :initform (make-hash-table :test 'equal))))

(defmethod initialize-instance :after ((object git-graph) &key)
  (setf
   (slot-value object '%branches)
   (fw.lu:alist-string-hash-table
    (funcall (data-lens:over
              (data-lens:<>1 (data-lens:applying #'cons)
                             (data-lens:transform-head
                              (serapeum:op (subseq _1 0
                                                   (min (length _1)
                                                        8))))
                             #'reverse))
             (fwoar.cl-git::branches (repo object))))))

(defun git-graph (repo)
  (fw.lu:new 'git-graph repo))

(defun get-commit-parents (repository commit)
  #+lispworks
  (declare (notinline mismatch serapeum:string-prefix-p))
  (when commit
    (co.fwoar.git:with-repository (repository)
      (alexandria:when-let*
          ((ref (fwoar.cl-git:ensure-ref commit))
           (direct-obj (fwoar.cl-git::extract-object
                        ref))
           (obj (etypecase direct-obj
                  (fwoar.cl-git::delta
                   (fwoar.cl-git::-extract-object-of-type
                    :commit
                    (fwoar.cl-git::trace-bases
                     (fwoar.cl-git::packed-ref-pack
                      ref)
                     direct-obj)
                    fwoar.cl-git::*git-repository*
                    :hash (fwoar.cl-git::ref-hash ref)))
                  (fwoar.cl-git::git-object
                   direct-obj)))
           (parents (fwoar.cl-git:component
                     :parents
                     obj)))
        (when parents
          parents)))))

(defmethod cl-dot:graph-object-node ((graph git-graph) (commit string))
  (alexandria:ensure-gethash
   commit
   (node-cache graph)
   (make-instance 'cl-dot:node
                  :attributes `(:label ,(gethash #1=(subseq commit 0 8)
                                                 (branches graph)
                                                 #1#)))))

(defmethod cl-dot:graph-object-points-to
    ((graph git-graph) (commit string))
  (unless (member commit (stops graph)
                  :test 'serapeum:string-prefix-p)
    (funcall (data-lens:<>1
              (data-lens:over (serapeum:op
                                (setf (gethash (list commit _1)
                                               (edge-cache graph))
                                      t)
                                _1))
              (data-lens:exclude (serapeum:op
                                   (gethash (list commit _1)
                                            (edge-cache graph))))
              (data-lens:over (serapeum:op (subseq _ 0 8))))
             (get-commit-parents (repo graph) commit))))
