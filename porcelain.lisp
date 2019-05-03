(in-package :fwoar.cl-git)

(defvar *git-repository* nil
  "The git repository path for porcelain commands to operate on.")
(defvar *git-encoding* :utf-8
  "The encoding to use when parsing git objects")

(defun git-show (object)
  (babel:octets-to-string (extract-object (repository *git-repository*)
                                          object)
                          :encoding *git-encoding*))
