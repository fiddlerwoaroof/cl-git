(in-package :fwoar.cl-git)

(defmacro defclass+ (name (&rest super) &body (direct-slots &rest options))
  (let ((initargs (mapcan (lambda (slot)
                            (serapeum:unsplice
                             (make-symbol
                              (symbol-name
                               (getf (cdr slot)
                                     :initarg)))))
                          direct-slots)))
    `(progn (defclass ,name ,super
              ,direct-slots
              ,@options)
            (defun ,name (,@initargs)
              (fw.lu:new ',name ,@initargs)))))

(fw.lu:defun-ct batch-4 (bytes)
  (mapcar 'fwoar.bin-parser:be->int
          (serapeum:batches bytes 4)))

(fw.lu:defun-ct batch-20 (bytes)
  (serapeum:batches bytes 20))

(defmacro sym->plist (&rest syms)
  `(list ,@(loop for sym in syms
                 append (list (alexandria:make-keyword sym)
                              sym))))

(defmacro inspect- (s form)
  `(let ((result ,form))
     (format ,s "~&~s (~{~s~^ ~})~%~4t~s~%"
             ',form
             ,(typecase form
                (list `(list ',(car form) ,@(cdr form)))
                (t `(list ,form)))
             result)
     result))

(defun inspect-* (fn)
  (lambda (&rest args)
    (declare (dynamic-extent args))
    (inspect- *trace-output*
              (apply fn args))))

(defun partition (char string &key from-end (with-offset nil wo-p))
  (let ((pos (position char string :from-end from-end)))
    (if pos
        (if wo-p
            (list (subseq string 0 (+ pos with-offset 1))
                  (subseq string (+ pos 1 with-offset)))
            (list (subseq string 0 pos)
                  (subseq string (1+ pos))))
      (list string
            nil))))

(defun partition-subseq (subseq string &key from-end)
  (let ((pos (search subseq string :from-end from-end)))
    (if pos
        (list (subseq string 0 pos)
              (subseq string (+ (length subseq) pos)))
      (list string
            nil))))

(serapeum:defalias ->sha-string
  (data-lens:<>1 (data-lens:over 'fwoar.bin-parser:byte-array-to-hex-string)
                 'batch-20))

(defun read-bytes (count format stream)
  (let ((seq (make-array count :element-type 'fwoar.cl-git.types:octet)))
    (read-sequence seq stream)
    (funcall format
             seq)))

(defun sp-ob (ob-string)
  (partition #\null
             ob-string))

(defun split-object (object-data)
  (destructuring-bind (head tail)
      (partition 0
                 object-data)
    (destructuring-bind (type length)
        (partition #\space
                   (babel:octets-to-string head :encoding :latin1))
      (values tail
              (list type
                    (parse-integer length))))))

(defmacro with-open-files* ((&rest bindings) &body body)
  `(uiop:nest ,@(mapcar (serapeum:op
                          `(with-open-file ,_1))
                        bindings)
              (progn
                ,@body)))


(defpackage :fwoar.cl-git.utils
  (:use :cl)
  (:import-from :fwoar.cl-git #:partition-subseq #:sym->plist #:read-bytes
                #:defclass+)
  (:export #:partition-subseq #:sym->plist #:read-bytes #:defclass+))
