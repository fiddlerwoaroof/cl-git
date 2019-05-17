;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Package: ASDF-USER -*-
(in-package :asdf-user)

(defsystem :cl-git 
  :description ""
  :author "Ed L <edward@elangley.org>"
  :license "MIT"
  :pathname #-fw.dev nil #+fw.dev #p"PROJECTS:cl-git;"
  :depends-on (:alexandria
               :chipz
               :cl-dot
               :data-lens
               :fwoar-lisputils
               :fwoar-lisputils/bin-parser
               :ironclad
               :serapeum
               :split-sequence
               :uiop)
  :components ((:file "package")
               (:file "util" :depends-on ("package"))
               (:file "model" :depends-on ("package"))
               (:file "extract" :depends-on ("package"))
               (:file "branch" :depends-on ("package" "extract"))
               (:file "git" :depends-on ("package" "util" "model" "branch"))
               (:file "porcelain" :depends-on ("package" "git"))))
