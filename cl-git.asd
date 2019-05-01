;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Package: ASDF-USER -*-
(in-package :asdf-user)

(defsystem :cl-git 
  :description ""
  :author "Ed L <edward@elangley.org>"
  :license "MIT"
  :pathname #-fw.dev nil #+fw.dev #p"PROJECTS:cl-git;"
  :depends-on (:alexandria
               :split-sequence
               :cl-dot
               :chipz
               :data-lens
               :fwoar-lisputils
               :fwoar-lisputils/bin-parser
               :serapeum
               :uiop)
  :components ((:file "cl-git")))
