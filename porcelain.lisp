(in-package :fwoar.cl-git)

(defvar *git-repository* nil
  "The git repository path for porcelain commands to operate on.")
(defvar *git-encoding* :utf-8
  "The encoding to use when parsing git objects")

(defun git:in-repository (root)
  (setf *git-repository*
        (truename root)))

(defun git:show-repository ()
  *git-repository*)

(defmacro git:git (&rest commands)
  `(uiop:nest ,@(reverse
                 (mapcar (serapeum:op (case (car _1)
                                        ((<<=) (list* 'mapcan
                                                      (list 'quote
                                                            (intern (symbol-name (cadadr _1))
                                                                    :git))
                                                      (cddr _1)))
                                        ((map) (list* 'mapcar
                                                      (list 'quote
                                                            (intern (symbol-name (cadadr _1))
                                                                    :git))
                                                      (cddr _1)))
                                        (t (cons (intern (symbol-name (car _1))
                                                         :git)
                                                 (cdr _1)))))
                         commands))))

(defun git:show (object)
  (babel:octets-to-string
   (coerce (extract-object (repository *git-repository*)
                           object)
           '(vector serapeum:octet))
   :encoding *git-encoding*))

(defun git:branch (&optional (branch "master"))
  #+lispworks
  (declare (notinline serapeum:assocadr))
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
