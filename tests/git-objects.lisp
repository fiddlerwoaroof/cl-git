(defpackage :fwoar.cl-git.git-objects
  (:use :cl )
  (:export ))
(in-package :fwoar.cl-git.git-objects)

(defparameter *fake-repo* :fwoar.cl-git.git-objects)
(fiveam:def-suite :fwoar.cl-git.git-objects
  :description "testing branch resolution"
  :in :fwoar.cl-git)
(fiveam:in-suite :fwoar.cl-git.git-objects)

(defclass fake-ref (fwoar.cl-git::git-ref)
  ())
(defun fake-ref (repo hash)
  (fwoar.lisputils:new 'fake-ref repo hash))

(defmethod fwoar.cl-git::ref ((repo (eql *fake-repo*)) hash)
  (fake-ref repo hash))


(fiveam:def-test basic-commit ()
  (let ((fwoar.cl-git::*git-repository* *fake-repo*)
        (object (fwoar.cl-git::extract-loose-object
                 nil
                 (asdf:system-relative-pathname
                  :co.fwoar.cl-git
                  "tests/sample-git-objects/hello-world-commit.git-obj")
                 (make-instance 'fake-ref :hash "the-hash"))))
    (5am:is (typep object 'fwoar.cl-git::git-commit))
    (5am:is (equal "hello, git!
"
                   (fwoar.cl-git:component :message object)))
    (5am:is (equal ()
                   (fwoar.cl-git:component :parents object)))
    (5am:is (equal "L Edgley <foo@bar.com> 1605513585 -0800"
                   (fwoar.cl-git:component :author object)))
    (5am:is (equal "Ed L <el-github@elangley.org> 1605513585 -0800"
                   (fwoar.cl-git:component :committer object)))
    (5am:is (equal ()
                   (fwoar.cl-git:component :parents object)))
    (5am:is (equal "1da546ab4697b719efb62f11fd785d6ad3b226d2"
                   (fwoar.cl-git::ref-hash (fwoar.cl-git:component :tree object))))
    (5am:is (equal *fake-repo*
                   (fwoar.cl-git::ref-repo (fwoar.cl-git:component :tree object))))
    (5am:is (equal '(("author" "L Edgley <foo@bar.com> 1605513585 -0800")
                     ("committer" "Ed L <el-github@elangley.org> 1605513585 -0800")
                     ("tree" "1da546ab4697b719efb62f11fd785d6ad3b226d2"))
                   (coerce (sort (copy-seq (fwoar.cl-git::metadata object))
                                 'string-lessp
                                 :key 'car)
                           'list)))))

(fiveam:def-test basic-tree ()
  (let ((object (fwoar.cl-git::extract-loose-object
                 nil
                 (asdf:system-relative-pathname
                  :co.fwoar.cl-git
                  "tests/sample-git-objects/hello-world-tree.git-obj")
                 (make-instance 'fake-ref :hash "the-hash"))))
    (5am:is (typep object 'fwoar.cl-git::git-tree))
    (let* ((entries (fwoar.cl-git::entries object))
           (entry (progn (5am:is (= (length entries) 1))
                         (car entries))))
      (5am:is (equal "4b5fa63702dd96796042e92787f464e28f09f17d"
                     (fwoar.cl-git:component :hash entry)))
      (5am:is (equal "a"
                     (fwoar.cl-git:component :name entry)))
      (5am:is (equal "100644"
                     (fwoar.cl-git:component :mode entry))))))

(defparameter *fake-repo* :fwoar.cl-git.git-objects.pack)
(defmethod fwoar.cl-git::ref ((repo (eql *fake-repo*)) hash)
  (fake-ref repo hash))
(defmethod fwoar.cl-git::pack-files ((repo (eql *fake-repo*)))
  (list
   (fwoar.cl-git::pack (asdf:system-relative-pathname
                        :co.fwoar.cl-git
                        "tests/sample-git-objects/hello-world-pack.idx")
                       (asdf:system-relative-pathname
                        :co.fwoar.cl-git
                        "tests/sample-git-objects/hello-world-pack.pack")
                       repo)))

(fiveam:def-test pack-files-commit ()
  (let* ((hash "7d7b56a6a64e090041f55293511f48aba6699f1a")
         (ref (fwoar.cl-git::packed-ref
               :fwoar.cl-git.git-objects.pack
               hash))
         (object (progn (fiveam:is (not (null ref)))
                        (fiveam:is (equal hash (fwoar.cl-git::ref-hash ref)))
                        (fiveam:is (equal *fake-repo* (fwoar.cl-git::ref-repo ref)))
                        (fwoar.cl-git::extract-object ref))))

    (5am:is (typep object 'fwoar.cl-git::git-commit))
    (5am:is (equal "hello, git!
"
                   (fwoar.cl-git:component :message object)))
    (5am:is (equal ()
                   (fwoar.cl-git:component :parents object)))
    (5am:is (equal "L Edgley <foo@bar.com> 1605513585 -0800"
                   (fwoar.cl-git:component :author object)))
    (5am:is (equal "Ed L <el-github@elangley.org> 1605513585 -0800"
                   (fwoar.cl-git:component :committer object)))
    (5am:is (equal ()
                   (fwoar.cl-git:component :parents object)))
    (let ((fwoar.cl-git::*git-repository* *fake-repo*))
      (5am:is (equal "1da546ab4697b719efb62f11fd785d6ad3b226d2"
                     (fwoar.cl-git::ref-hash (fwoar.cl-git:component :tree object))))
      (5am:is (equal *fake-repo*
                     (fwoar.cl-git::ref-repo (fwoar.cl-git:component :tree object)))))
    (5am:is (equal '(("author" "L Edgley <foo@bar.com> 1605513585 -0800")
                     ("committer" "Ed L <el-github@elangley.org> 1605513585 -0800")
                     ("tree" "1da546ab4697b719efb62f11fd785d6ad3b226d2"))
                   (coerce (sort (copy-seq (fwoar.cl-git::metadata object))
                                 'string-lessp
                                 :key 'car)
                           'list)))))

(fiveam:def-test pack-files-tree ()
  (let* ((hash "1da546ab4697b719efb62f11fd785d6ad3b226d2")
         (ref (fwoar.cl-git::packed-ref
               :fwoar.cl-git.git-objects.pack
               hash))
         (object (progn (fiveam:is (not (null ref)))
                        (fiveam:is (equal hash (fwoar.cl-git::ref-hash ref)))
                        (fiveam:is (equal *fake-repo* (fwoar.cl-git::ref-repo ref)))
                        (fwoar.cl-git::extract-object ref))))
    (5am:is (typep object 'fwoar.cl-git::git-tree))
    (let* ((entries (fwoar.cl-git::entries object))
           (entry (progn (5am:is (= (length entries) 1))
                         (car entries))))
      (5am:is (equal "4b5fa63702dd96796042e92787f464e28f09f17d"
                     (fwoar.cl-git:component :hash entry)))
      (5am:is (equal "a"
                     (fwoar.cl-git:component :name entry)))
      (5am:is (equal "100644"
                     (fwoar.cl-git:component :mode entry))))))

(fiveam:def-test pack-files-blob ()
  (let* ((hash "4b5fa63702dd96796042e92787f464e28f09f17d")
         (ref (fwoar.cl-git::packed-ref
               :fwoar.cl-git.git-objects.pack
               hash))
         (object (progn (fiveam:is (not (null ref)))
                        (fiveam:is (equal hash (fwoar.cl-git::ref-hash ref)))
                        (fiveam:is (equal *fake-repo* (fwoar.cl-git::ref-repo ref)))
                        (fwoar.cl-git::extract-object ref))))
    (5am:is (typep object 'fwoar.cl-git::blob))
    (5am:is (equal "hello, world
"
                   (babel:octets-to-string
                    (fwoar.cl-git::data
                     (fwoar.cl-git::extract-object
                      (fwoar.cl-git::packed-ref
                       :fwoar.cl-git.git-objects.pack
                       "4b5fa63702dd96796042e92787f464e28f09f17d")))
                    :encoding :utf-8)))))


(defparameter *fake-repo-2* :fwoar.cl-git.git-objects.pack-2)
(defmethod fwoar.cl-git::ref ((repo (eql *fake-repo-2*)) hash)
  (fake-ref repo hash))
(defmethod fwoar.cl-git::pack-files ((repo (eql *fake-repo-2*)))
  (list
   (let* ((pack-file (asdf:system-relative-pathname
                      :co.fwoar.cl-git/tests
                      "tests/sample-git-objects/pack-a0533639fdee4493fdbfc1b701872ace63b95e5f.pack"))
          (index-file (asdf:system-relative-pathname
                       :co.fwoar.cl-git/tests
                       "tests/sample-git-objects/pack-a0533639fdee4493fdbfc1b701872ace63b95e5f.idx")))
     (make-instance 'fwoar.cl-git::pack
                    :repository nil
                    :index index-file
                    :pack pack-file))))

(fiveam:def-test pack-files-offsets ()
  (let* ((expectations-file
           (asdf:system-relative-pathname
            :co.fwoar.cl-git/tests
            "tests/sample-git-objects/pack-a0533639fdee4493fdbfc1b701872ace63b95e5f.delta-bases"))
         (expectations (uiop:read-file-form expectations-file)))
    (loop for (ref . base-offset) in expectations
          do (5am:is (equal base-offset
                            (second
                             (fwoar.cl-git::base
                              (fwoar.cl-git::extract-object
                               (fwoar.cl-git::packed-ref *fake-repo-2* ref)))))))
    ))

(fiveam:def-test pack-file-apply-delta-commands ()
  (flet ((test-ref (ref)
           (let* ((extracted-ref
                    (fwoar.cl-git::extract-object
                     (fwoar.cl-git::packed-ref :fwoar.cl-git.git-objects.pack-2 ref)))
                  (base-desc (fwoar.cl-git::base extracted-ref))
                  (pack (car (fwoar.cl-git::pack-files *fake-repo-2*)))
                  (expectations-file
                    (asdf:system-relative-pathname
                     :co.fwoar.cl-git/tests
                     (format nil "tests/sample-git-objects/blob-~a-fixture"
                             (subseq ref 0 7))))
                  (expectations
                    (alexandria:read-file-into-byte-vector expectations-file)))
             (5am:is
              (serapeum:vector=
               expectations
               (fwoar.cl-git::trace-bases pack extracted-ref))))))
    (test-ref "87c2b9b2dfaa1fbf66b3fe88d3a925593886b159")

    (test-ref "9776df71b5ddf298c56e99b7291f9e68906cf049")

    #+(or) ;; broken
    (test-ref "31576396aff0fff28f69e0ef84571c0dc8cc43ec")

    #+(or) ;; broken
    (test-ref "c516dfc248544509c3ae58e3a8c2ab81c225aa9c")

    #+(or) ;; broken
    (test-ref "53d13ed284f8b57297d1b216e2bab7fb43f8db60")

    #+(or) ;; broken
    (test-ref "912d31a169ddf1fca122d4c6fe1b1e6be7cd1176")))
