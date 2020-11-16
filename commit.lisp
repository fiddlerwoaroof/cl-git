(in-package :fwoar.cl-git)

(defclass git-commit ()
  ((%metadata :initarg :metadata :reader metadata)
   (%data :initarg :data :reader data)))

(defun git-commit (metadata data)
  (fw.lu:new 'git-commit metadata data))

(defun parse-commit (commit)
  (destructuring-bind (metadata message)
      (partition-subseq #(#\newline #\newline)
                        commit #+(or)(babel:octets-to-string commit :encoding :latin1))
    (values message
            (map 'vector (serapeum:op (partition #\space _))
                 (fwoar.string-utils:split #\newline metadata)))))

(defun make-commit (data)
  (multiple-value-bind (message metadata)
      (parse-commit data)
    (git-commit metadata message)))

(defmethod -extract-object-of-type ((type (eql :commit)) s repository &key)
  (make-commit (babel:octets-to-string s :encoding *git-encoding*)))


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
  (coerce (remove-if-not (serapeum:op
                           (string= "parent" _))
                         (metadata object)
                         :key #'car)
          'list))
(defmethod component ((component (eql :message)) (object git-commit))
  (data object))
