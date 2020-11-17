(defpackage :fwoar.cl-git.git-objects
  (:use :cl )
  (:export ))
(in-package :fwoar.cl-git.git-objects)

(defparameter *fake-repo* :fwoar.cl-git.git-objects)
(fiveam:def-suite :fwoar.cl-git.git-objects
  :description "testing branch resolution"
  :in :fwoar.cl-git)
(fiveam:in-suite :fwoar.cl-git.git-objects)

(fw.lu:defclass+ fake-ref ()
  ((%repo :initarg :repo :reader repo)
   (%hash :initarg :hash :reader hash)))
(defmethod fwoar.cl-git::ref ((repo (eql *fake-repo*)) hash)
  (fake-ref repo hash))


(fiveam:def-test basic-commit ()
  (let ((fwoar.cl-git::*git-repository* *fake-repo*)
        (object (fwoar.cl-git::extract-loose-object
                 nil
                 (asdf:system-relative-pathname
                  :cl-git
                  "tests/sample-git-objects/hello-world-commit.git-obj"))))
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
                   (hash (fwoar.cl-git:component :tree object))))
    (5am:is (equal *fake-repo*
                   (repo (fwoar.cl-git:component :tree object))))
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
                  :cl-git
                  "tests/sample-git-objects/hello-world-tree.git-obj"))))
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
                        :cl-git
                        "tests/sample-git-objects/hello-world-pack.idx")
                       (asdf:system-relative-pathname
                        :cl-git
                        "tests/sample-git-objects/hello-world-pack.pack")
                       repo)))

(fiveam:def-test pack-files-commit ()
  (let* ((hash "7d7b56a6a64e090041f55293511f48aba6699f1a")
         (ref (fwoar.cl-git::packed-ref-for-object
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
                     (hash (fwoar.cl-git:component :tree object))))
      (5am:is (equal *fake-repo*
                     (repo (fwoar.cl-git:component :tree object)))))
    (5am:is (equal '(("author" "L Edgley <foo@bar.com> 1605513585 -0800")
                     ("committer" "Ed L <el-github@elangley.org> 1605513585 -0800")
                     ("tree" "1da546ab4697b719efb62f11fd785d6ad3b226d2"))
                   (coerce (sort (copy-seq (fwoar.cl-git::metadata object))
                                 'string-lessp
                                 :key 'car)
                           'list)))))

(fiveam:def-test pack-files-tree ()
  (let* ((hash "1da546ab4697b719efb62f11fd785d6ad3b226d2")
         (ref (fwoar.cl-git::packed-ref-for-object
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
         (ref (fwoar.cl-git::packed-ref-for-object
               :fwoar.cl-git.git-objects.pack
               hash))
         (object (progn (fiveam:is (not (null ref)))
                        (fiveam:is (equal hash (fwoar.cl-git::ref-hash ref)))
                        (fiveam:is (equal *fake-repo* (fwoar.cl-git::ref-repo ref)))
                        (fwoar.cl-git::extract-object ref))))
    (5am:is (typep object '(vector (unsigned-byte 8))))
    (5am:is (equal "hello, world
"
                   (babel:octets-to-string
                    (fwoar.cl-git::extract-object
                     (fwoar.cl-git::packed-ref-for-object
                      :fwoar.cl-git.git-objects.pack
                      "4b5fa63702dd96796042e92787f464e28f09f17d"))
                    :encoding :utf-8)))))
