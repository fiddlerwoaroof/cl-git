(in-package :fwoar.cl-git)

(defun get-local-unpacked-branches (repository)
  (mapcar (data-lens:juxt #'pathname-name
                          (alexandria:compose #'serapeum:trim-whitespace
                                              #'alexandria:read-file-into-string))
          (uiop:directory*
           (merge-pathnames "refs/heads/*"
                            (root repository)))))

(defun get-local-packed-branches (repository)
  (let* ((packed-ref-file-name (merge-pathnames "packed-refs"
                                                (root repository))))
    (when (probe-file packed-ref-file-name)
      (with-open-file (s packed-ref-file-name)
        (loop for line = (read-line s nil)
              for parts = (partition #\space line)
              for branch-name = (second parts)
              while line
              unless (alexandria:starts-with-subseq "#" line)
                when (alexandria:starts-with-subseq "refs/heads" branch-name)
                  collect (list (subseq branch-name
                                        (1+ (position #\/ branch-name
                                                      :from-end t)))
                                (first parts)))))))

(defun get-local-branches (repository)
  (append (get-local-unpacked-branches repository)
          (get-local-packed-branches repository)))

(defgeneric branches (repository)
  (:method ((repository git-repository))
    (get-local-branches repository)))

(defgeneric branch (repository name)
  (:method ((repository git-repository) name)
    (second
     (find name (get-local-branches repository)
           :test 'equal
           :key 'car))))
