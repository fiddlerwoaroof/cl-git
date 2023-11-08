(defpackage :fwoar.cl-git.git-objects
  (:use :cl :fwoar.cl-git.protocol)
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
    (5am:is (typep object 'fwoar.cl-git.commit::git-commit))
    (5am:is (equal "hello, git!
"
                   (component :message object)))
    (5am:is (equal ()
                   (component :parents object)))
    (5am:is (equal "L Edgley <foo@bar.com> 1605513585 -0800"
                   (component :author object)))
    (5am:is (equal "Ed L <el-github@elangley.org> 1605513585 -0800"
                   (component :committer object)))
    (5am:is (equal ()
                   (component :parents object)))
    (5am:is (equal "1da546ab4697b719efb62f11fd785d6ad3b226d2"
                   (fwoar.cl-git::ref-hash (component :tree object))))
    (5am:is (equal *fake-repo*
                   (fwoar.cl-git::ref-repo (component :tree object))))
    (5am:is (equal '(("author" "L Edgley <foo@bar.com> 1605513585 -0800")
                     ("committer" "Ed L <el-github@elangley.org> 1605513585 -0800")
                     ("tree" "1da546ab4697b719efb62f11fd785d6ad3b226d2"))
                   (coerce (sort (copy-seq (fwoar.cl-git.commit::metadata object))
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
                     (component :hash entry)))
      (5am:is (equal "a"
                     (component :name entry)))
      (5am:is (equal "100644"
                     (component :mode entry))))))

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

    (5am:is (typep object 'fwoar.cl-git.commit::git-commit))
    (5am:is (equal "hello, git!
"
                   (component :message object)))
    (5am:is (equal ()
                   (component :parents object)))
    (5am:is (equal "L Edgley <foo@bar.com> 1605513585 -0800"
                   (component :author object)))
    (5am:is (equal "Ed L <el-github@elangley.org> 1605513585 -0800"
                   (component :committer object)))
    (5am:is (equal ()
                   (component :parents object)))
    (let ((fwoar.cl-git::*git-repository* *fake-repo*))
      (5am:is (equal "1da546ab4697b719efb62f11fd785d6ad3b226d2"
                     (fwoar.cl-git::ref-hash (component :tree object))))
      (5am:is (equal *fake-repo*
                     (fwoar.cl-git::ref-repo (component :tree object)))))
    (5am:is (equal '(("author" "L Edgley <foo@bar.com> 1605513585 -0800")
                     ("committer" "Ed L <el-github@elangley.org> 1605513585 -0800")
                     ("tree" "1da546ab4697b719efb62f11fd785d6ad3b226d2"))
                   (coerce (sort (copy-seq (fwoar.cl-git.commit::metadata object))
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
                     (component :hash entry)))
      (5am:is (equal "a"
                     (component :name entry)))
      (5am:is (equal "100644"
                     (component :mode entry))))))

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
(defclass fake-ref-2 (fake-ref)
  ())
(defun fake-ref-2 (repo hash)
  (fwoar.lisputils:new 'fake-ref-2 repo hash))
(defmethod fwoar.cl-git::packed-ref-pack ((ref fake-ref-2))
  (let* ((pack-file (asdf:system-relative-pathname
                     :co.fwoar.cl-git/tests
                     "tests/sample-git-objects/pack-a0533639fdee4493fdbfc1b701872ace63b95e5f.pack"))
         (index-file (asdf:system-relative-pathname
                      :co.fwoar.cl-git/tests
                      "tests/sample-git-objects/pack-a0533639fdee4493fdbfc1b701872ace63b95e5f.idx")))
    (make-instance 'fwoar.cl-git::pack
                   :repository nil
                   :index index-file
                   :pack pack-file)))
(defmethod fwoar.cl-git::packed-ref-offset ((ref fake-ref-2))
  (nth-value 1 (fwoar.cl-git::find-sha-in-pack (fwoar.cl-git::packed-ref-pack ref)
                                               (fwoar.cl-git::ref-hash ref))))
(defmethod fwoar.cl-git::ref ((repo (eql *fake-repo-2*)) hash)
  (fake-ref-2 repo hash))
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
  (let* ((fwoar.cl-git:*want-delta* t)
         (expectations-file
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

(fiveam:def-test test-pack-roundtrip ()
  (let ((shas '("b7df27f1c873f5796462cdce8aabf46c1b3e3ff2"
                "d468a84b54e73968d9426af96c1944c80ffa3a4f"
                "4cc1ee4919056be337922f0a57e0bfe7281b8c57"
                "4d4ea31b3d349ffd06e97469743f824578555edf"
                "7df80f061ae5bf6177a1c0888d085281be2801e1"
                "846489f7ae91bfaf0c78a6939b177697a89a81d0"
                "bc7ccfbd98e684d9188b6833ec39f7d1d72edfdf"
                "6089dc804725925c30d621c3d2f72c8b1b14bc17"
                "500325f0022a9adc41929b58fbb5c2d55b60524b"
                "72870f874f3ef712d9bea352e300b9b5f6aa60ee"
                "0c24c8f931ad5c0d2e5add01710678abddd3ec03"
                "e499f64d2ead6d14d74fe0f484d06f33bbd38261"
                "efe60b9f578c4966cb2258ace1661edd080ca0dc"
                "821ddf96c37e65ccc9a0f4bfe2b8ac6e255a2cb6"
                "077088c8c359489ed1f6d8e441ec76438076542e"
                "e1f7c67a8774d65bb941eeb2b41f71f333fa1a94"
                "ff33293b415cc1907a6071650d045b3dffd8e5c0"
                "e98a5866a0148fe573197e8c48a543fc3039f1bc"
                "f09f6f1b30fd3579649f8abf23719901496accde"
                "692f03101cd8ebf6830618805217b6348ddfd3a8"
                "f710cf28a9f511911e1def85c4cb98bfbcfd9017"
                "0b3ed8597e1968306c3732f7507256694357009f"
                "88e003ecce9e9420632d0bab857270819e922674"
                "5c205fb851671ff0938c86d7c0cc742f2ca2d32f"
                "8f6a890959795d2b340615a074170ce404d7f2b9"
                "e079ee4a351de0841c09f87dffdee333ef936673"
                "347e97b1efa866e3bd00bbfb68c5b660e378f3b5"
                "ad9b8a82065f70aac3da61e845ab2cd37a71e649"
                "83674eea1c0a2f2df2886b38b9539ed1193b00c3"
                "e623be68f6fd0c36dee0145a4c95dbbf85174774"
                "a7c6e622cca243456481ffbeafaab739e4687681"
                "b0bbceded2a17389a9a6ddc765398a257199c78e"
                "82c268c1e7afe543ac14bf2748df53a729fa35cb"
                "2fb0a2fc57327dc6a533b596a0643ad991847b3b"
                "f1a12e8a19691afcd5ee08d615a1b4d14b5051f9"
                "991d0162019ac2e21592553a10ab16eb337222d8"
                "94acd859d12ae611e631cfd66b7ed164d6b5ac89"
                "f115dea85d331cb5c01e247d77886bba2690e726"
                "488cc8612e7b24a1737a260b10bff0037b55636e"
                "be4ef77fd7da17393e02ef933e8d21e67be7fbec"
                "a84a7f712398c1659f2e809d903ae51b44cf7f4a"
                "8a9fe9f77149f74fed5c05388be8e5ffd4a31678"
                "e69de29bb2d1d6434b8b29ae775ad8c2e48c5391"
                "0306819e780fa57dc3bf6b99a0a059670b605ae0"
                "a52be677adeda194bcdfdd12740f00535b6b0997"
                "fb265bb344fee602dc175d1d5eac6bdc2d013a10"
                "9db42f61f21e11529b9bc1c52ee118c03d663c04"
                "197e10755343900cfbcb7fc6d863d4b3231e74d4"
                "83324cbcb0ef5b778588cc6ba547c43c46bff8c6"
                "88988d16b44fc03054807882783ed176162228f4"
                "d2818bb88b8ec5235a8ae91309f31ba58d941d42"
                "c1b83741c4dc3104f1686c20b143300db0a0e258"
                "7e24a6a7a4349497fce06830fa132e9a8ef6fd06"
                "9567a5825bf65b7e90d6f9a02574a00b53af9171"
                "b757bb704b4c7a54622b7bd197ad5c1ea51ef2cc"
                "ccccc07814249fc7a129bfffd07f09704d0f017b"
                "a4b5b13466bb8e80d6f8015e2bf27667533ea441"
                "3d894d70b6e1036034f22654408a382b6e303335"
                "fed9d70ab2441d8c8abf19648668f885ed5a4986"
                "b50c3a28d0bdab4d922d4b363cada4c582349178"
                "4b825dc642cb6eb9a060e54bf8d69288fbee4904"
                "71da880a8be0356b67d593fac348dfe429d1e0b6"
                "cab7cafae3b61c5b101ee914cd4f5c8357e77fad"
                "f03a8d1b4cea085ee9555037d09bca2dbfb990cb")))
    (loop for commit in shas
          for obj = (fwoar.cl-git::raw-object-for-ref
                     (fwoar.cl-git::ref :fwoar.cl-git.git-objects.pack-2 commit))
          do (5am:is (equal (crypto:byte-array-to-hex-string
                             (crypto:digest-sequence :sha1 obj))
                            commit)))))

(fiveam:def-test pack-file-apply-delta-commands ()
  (flet ((test-ref (ref)
           (let* ((expectations-file
                    (asdf:system-relative-pathname
                     :co.fwoar.cl-git/tests
                     (format nil "tests/sample-git-objects/blob-~a-fixture"
                             (subseq ref 0 7))))
                  (expectations
                    (alexandria:read-file-into-byte-vector expectations-file)))
             (5am:is
              (serapeum:vector=
               expectations
               (fwoar.cl-git::data
                (fwoar.cl-git::extract-object
                 (fwoar.cl-git::packed-ref :fwoar.cl-git.git-objects.pack-2 ref))))))))

    (test-ref "87c2b9b2dfaa1fbf66b3fe88d3a925593886b159")

    (test-ref "9776df71b5ddf298c56e99b7291f9e68906cf049")

    (test-ref "31576396aff0fff28f69e0ef84571c0dc8cc43ec")

    (test-ref "c516dfc248544509c3ae58e3a8c2ab81c225aa9c")

    (test-ref "53d13ed284f8b57297d1b216e2bab7fb43f8db60")

    (test-ref "912d31a169ddf1fca122d4c6fe1b1e6be7cd1176")))
