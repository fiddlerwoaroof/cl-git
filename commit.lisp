(in-package :fwoar.cl-git)

(defclass git-commit ()
  ((%hash :initarg :hash :reader hash)
   (%metadata :initarg :metadata :reader metadata)
   (%data :initarg :data :reader data)))

(defmethod print-object ((o git-commit) s)
  (print-unreadable-object (o s :type t :identity t)
    (when (slot-boundp o '%hash)
      (princ (subseq (hash o) 0 10) s)
      (princ #\space s))))

(defun git-commit (metadata data &optional (hash nil hash-p))
  (if hash-p
      (fw.lu:new 'git-commit metadata data hash)
      (fw.lu:new 'git-commit metadata data)))

(defun parse-commit (commit)
  (destructuring-bind (metadata message)
      (partition-subseq #(#\newline #\newline)
                        commit #+(or)(babel:octets-to-string commit :encoding :latin1))
    (values message
            (map 'vector (serapeum:op (partition #\space _))
                 (fwoar.string-utils:split #\newline metadata)))))

(defun make-commit (data hash)
  (multiple-value-bind (message metadata)
      (parse-commit data)
    (git-commit metadata message hash)))

(defmethod -extract-object-of-type ((type (eql :commit)) s repository &key hash)
  (make-commit (babel:octets-to-string s :encoding *git-encoding*) hash))


(defmethod component ((component (eql :tree)) (object git-commit))
  (ensure-ref
   (cadr
    (fw.lu:v-assoc :tree (metadata object)
                   :test 'string-equal))))
(defmethod component ((component (eql :author)) (object git-commit))
  (second
   (fw.lu:v-assoc :author (metadata object)
                  :test 'string-equal)))
(defmethod component ((component (eql :committer)) (object git-commit))
  (second
   (fw.lu:v-assoc :committer (metadata object)
                  :test 'string-equal)))
(defmethod component ((component (eql :parents)) (object git-commit))
  (map 'list
       'cdr
       (remove-if-not (serapeum:op
                        (string= "parent" _))
                      (metadata object)
                      :key #'car)))
(defmethod component ((component (eql :message)) (object git-commit))
  (data object))
