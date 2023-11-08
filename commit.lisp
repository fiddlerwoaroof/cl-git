(in-package :fwoar.cl-git.commit)

(defclass git-commit (fwoar.cl-git:git-object)
  ((%metadata :initarg :metadata :reader metadata)
   (%data :initarg :data :reader data)))

(defun git-commit (hash metadata data)
  (fw.lu:new 'git-commit hash metadata data))

(defun clamp-string (s len)
  (subseq s 0 (min len (length s))))

(defmethod print-object ((o git-commit) s)
  (if *print-readably*
      (format s "#.(git-commit ~<~s~_~s~_~s~:>)"
              (list (fwoar.cl-git:hash o)
                    (metadata o)
                    (data o)))
      (print-unreadable-object (o s :type t :identity t)
        (format s "~a" (format nil "~7,1,1,'x@a" (clamp-string (fwoar.cl-git:hash o) 7))))))

(defun parse-commit (commit)
  (destructuring-bind (metadata message)
      (fwoar.cl-git.utils:partition-subseq
       #(#\newline #\newline)
       commit
       #+(or)(babel:octets-to-string commit :encoding :latin1))
    (values message
            (map 'vector
                 (serapeum:op (fwoar.string-utils:partition #\space _))
                 (fwoar.string-utils:split #\newline metadata)))))

(defun make-commit (data hash)
  (multiple-value-bind (message metadata)
      (parse-commit data)
    (git-commit hash metadata message)))

(defmethod -extract-object-of-type
    ((type (eql :commit)) s repository &key hash)
  (make-commit (babel:octets-to-string s :encoding fwoar.cl-git:*git-encoding*)
               hash))

(defcomponents git-commit (object _)
  ((eql :tree) (fwoar.cl-git:ensure-ref
                (cadr
                 (fw.lu:v-assoc :tree (metadata object)
                                :test 'string-equal))))

  ((eql :author) (second
                  (fw.lu:v-assoc :author (metadata object)
                                 :test 'string-equal)))

  ((eql :committer) (second
                     (fw.lu:v-assoc :committer (metadata object)
                                    :test 'string-equal)))

  ((eql :parents) (data-lens.transducers:into
                   '()
                   (data-lens:•
                    (data-lens.transducers:filtering
                     (data-lens:on (data-lens:== "parent" :test 'equal)
                                   #'car))
                    (data-lens.transducers:mapping #'cadr))
                   (metadata object)))

  ((eql :message) (data object)))
