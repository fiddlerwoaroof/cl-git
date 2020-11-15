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
               :fwoar-lisputils
               :cl-ppcre
               :fwoar-lisputils/bin-parser
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
               (:file "tree" :depends-on ("package" "model"))
               (:file "commit" :depends-on ("package" "model"))
               (:file "delta" :depends-on ("package" "model"))

               (:file "extract" :depends-on ("package" "commit" "tree" "delta"))
               (:file "branch" :depends-on ("package" "extract"))
               (:file "git" :depends-on ("package" "util" "model" "branch"))

               ;; stable programmer interface
               (:file "porcelain" :depends-on ("package" "git" "commit"))))
