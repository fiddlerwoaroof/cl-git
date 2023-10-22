(in-package :fwoar.cl-git)

(defclass git-commit ()
  ((%hash :initarg :hash :reader hash)
   (%metadata :initarg :metadata :reader metadata)
   (%data :initarg :data :reader data)))

(defmethod print-object ((o git-commit) s)
  (if *print-readably*
      (format s "#.(git-commit ~<~s~_~s~_~s~:>)"
              (list (hash o)
                    (metadata o)
                    (data o)))
      (print-unreadable-object (o s :type t :identity t)
        (format s "~a" (subseq (hash o) 0 6)))))

(defun git-commit (hash metadata data)
  (fw.lu:new 'git-commit hash metadata data))

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
    (git-commit hash metadata message)))

(defmethod -extract-object-of-type ((type (eql :commit)) s repository &key hash)
  (make-commit (babel:octets-to-string s :encoding *git-encoding*)
               hash))


(defmethod component ((component (eql :hash)) (object git-commit))
  (hash object))

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
  (data-lens.transducers:into '()
                              (data-lens:•
                               (data-lens.transducers:filtering
                                (data-lens:on (data-lens:== "parent" :test 'equal)
                                              #'car))
                               (data-lens.transducers:mapping #'cadr))
                              (metadata object)))
(defmethod component ((component (eql :message)) (object git-commit))
  (data object))
