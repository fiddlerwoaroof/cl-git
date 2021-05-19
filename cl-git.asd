;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Package: ASDF-USER -*-
(in-package :asdf-user)

(defsystem :cl-git
  :description "A pure-Lisp git implementation"
  :author "Ed L <edward@elangley.org>"
  :license "MIT"
  :pathname #-fw.dev nil #+fw.dev #p"PROJECTS:cl-git;"
  :depends-on (:alexandria
               :chipz
               :cl-dot
               :data-lens
               :data-lens/beta/transducers
               :fwoar-lisputils
               :cl-ppcre
               :fwoar-lisputils/bin-parser
               :babel
               :ironclad
               :serapeum
               :split-sequence
               :uiop)
  :components ((:file "package")
               (:file "util" :depends-on ("package"))

               ;; data model
               (:file "model" :depends-on ("package"))
               (:file "protocol" :depends-on ("package" "model"))
               (:file "repository" :depends-on ("package" "model"))
               (:file "tree" :depends-on ("package" "model" "protocol"))
               (:file "commit" :depends-on ("package" "model" "protocol"))
               (:file "delta" :depends-on ("package" "model" "protocol"))

               (:file "extract" :depends-on ("package" "protocol" "commit" "tree" "delta"))
               (:file "branch" :depends-on ("package" "extract"))
               (:file "git" :depends-on ("package" "util" "model" "branch"))

               ;; stable programmer interface
               (:file "porcelain" :depends-on ("package" "git" "commit"))))
(defsystem :cl-git/tests
  :description ""
  :author "Ed L <edward@elangley.org>"
  :license "MIT"
  :depends-on (#:alexandria
               #:uiop
               #:serapeum
               #:fiveam
               #:cl-git)
  :serial t
  :components ((:module "tests"
                :components ((:file "tests")
                             (:file "branch-resolution" :depends-on ("tests"))
                             (:file "git-objects" :depends-on ("tests"))))))
