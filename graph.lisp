(in-package :fwoar.cl-git)

(defclass git-graph ()
  ((%repo :initarg :repo :reader repo)
   (%depth :initarg :depth :reader depth)
   (%branches :reader branches)
   (%node-cache :reader node-cache :initform (make-hash-table :test 'equal))
   (%edge-cache :reader edge-cache :initform (make-hash-table :test 'equal))))

(defmethod initialize-instance :after ((object git-graph) &key)
  (setf (slot-value object '%branches)
        (fw.lu:alist-string-hash-table
         (funcall (data-lens:over
                   (<>1 (data-lens:applying #'cons)
                        (data-lens:transform-head
                         (serapeum:op (subseq _1 0 (min (length _1) 7))))
                        #'reverse))
                  (branches (repo object))))))

(defun git-graph (repo)
  (fw.lu:new 'git-graph repo))

(defun get-commit-parents (repository commit)
  #+lispworks
  (declare (notinline mismatch serapeum:string-prefix-p))
  (map 'list 
       (serapeum:op (second (partition #\space _)))
       (remove-if-not (lambda (it)
                        (serapeum:string-prefix-p "parent" it))
                      (nth-value 1 (parse-commit
                                    (split-object
                                     (chipz:decompress nil (chipz:make-dstate 'chipz:zlib)
                                                       (loose-object repository
                                                                     commit))))))))

(defmethod cl-dot:graph-object-node ((graph git-graph) (commit string))
  (alexandria:ensure-gethash commit
                             (node-cache graph)
                             (make-instance 'cl-dot:node
                                            :attributes `(:label ,(gethash #1=(subseq commit 0 7)
                                                                           (branches graph)
                                                                           #1#)))))

(defmethod cl-dot:graph-object-points-to ((graph git-graph) (commit string))
  (mapcar (lambda (c)
            (setf (gethash (list commit c)
                           (edge-cache graph))
                  t)
            c)
          (remove-if (lambda (it)
                       (gethash (list commit it)
                                (edge-cache graph)))
                     (mapcar (serapeum:op (subseq _ 0 7))
                             (get-commit-parents (repo graph) commit)
                             #+nil
                             (loop
                               for cur = (list commit) then parents
                               for parents = (let ((f (get-commit-parents (repo graph) (car cur))))
                                               f)
                               until (or (not parents)
                                         (cdr parents))
                               finally (return (or parents
                                                   (when (not (equal commit (car cur)))
                                                     cur))))))))
