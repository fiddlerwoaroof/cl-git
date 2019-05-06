(in-package :fwoar.cl-git)

(defvar *git-repository* nil
  "The git repository path for porcelain commands to operate on.")
(defvar *git-encoding* :utf-8
  "The encoding to use when parsing git objects")

(defun git:in-repository (root)
  (setf *git-repository*
        (truename root)))

(defun git:show (object)
  (babel:octets-to-string
   (extract-object (repository *git-repository*)
                   object)
   :encoding *git-encoding*))

(defun git:branch (&optional (branch "master"))
  (let ((branches (branches (repository *git-repository*))))
    (nth-value 0 (serapeum:assocadr branch branches
                                    :test 'equal))))

(defun git:branches ()
  (branches (repository *git-repository*)))

(defun git:commit-parents (commit)
  (map 'list #'cadr
       (remove-if-not (serapeum:op
                        (string= "parent" _))
                      (nth-value 1
                                 (fwoar.cl-git::parse-commit
                                  (git:show commit)))
                      :key #'car)))
