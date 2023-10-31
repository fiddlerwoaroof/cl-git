;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Package: ASDF-USER -*-
(in-package :asdf-user)

(defsystem :co.fwoar.cl-git
  :description "A pure-Lisp git implementation"
  :author "Ed L <edward@elangley.org>"
  :license "MIT"
  :pathname #-fw.dev nil #+fw.dev #p"PROJECTS:cl-git;"
  :depends-on (:alexandria
               :babel
               :chipz
               :cl-dot
               :cl-ppcre
               :data-lens
               :data-lens/beta/transducers
               :flexi-streams
               :fwoar-lisputils
               :fwoar-lisputils/bin-parser
               :ironclad
               :serapeum
               :split-sequence
               :uiop)
  :in-order-to ((test-op (test-op :co.fwoar.cl-git/tests)))
  :components ((:file "package")
               (:file "types" :depends-on ("package"))
               (:file "util" :depends-on ("types" "package"))

               ;; data model
               (:file "model" :depends-on ("package"))
               (:file "protocol" :depends-on ("package" "model" "util"))
               (:file "repository" :depends-on ("package" "model"))
               (:file "tree" :depends-on ("package" "model" "protocol"))
               (:file "commit" :depends-on ("package" "model" "protocol"))
               (:file "delta" :depends-on ("package" "model" "protocol"))

               (:file "extract" :depends-on ("package" "protocol" "commit" "tree" "delta"))
               (:file "branch" :depends-on ("package" "extract"))
               (:file "git" :depends-on ("package" "types" "util" "model" "branch"))

               ;; stable programmer interface
               (:file "porcelain" :depends-on ("package" "git" "commit"))))

(defsystem :co.fwoar.cl-git/tests
  :description ""
  :author "Ed L <edward@elangley.org>"
  :license "MIT"
  :depends-on (#:alexandria
               #:uiop
               #:serapeum
               #:fiveam
               #:co.fwoar.cl-git)
  :serial t
  :perform (test-op (o c)
                    (unless (symbol-call :fiveam '#:run! :fwoar.cl-git)
                      (error "some tests failed")))
  :components ((:module "tests"
                :components ((:file "tests")
                             (:file "branch-resolution" :depends-on ("tests"))
                             (:file "git-objects" :depends-on ("tests"))))))
