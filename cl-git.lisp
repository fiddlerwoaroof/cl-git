(defpackage :fwoar.cl-git
  (:use :cl )
  (:export ))
(in-package :fwoar.cl-git)

(defclass repository ()
  ((%root :initarg :root :reader root)))

(defun repository (root)
  (fw.lu:new 'repository root))

(defun get-local-branches (root)
  (mapcar (data-lens:juxt #'pathname-name
                          (alexandria:compose #'serapeum:trim-whitespace
                                              #'alexandria:read-file-into-string))
          (uiop:directory*
           (merge-pathnames ".git/refs/heads/*"
                            root))))

(defun loose-object-path (sha)
  (let ((obj-path (fwoar.string-utils:insert-at 2 #\/ sha)))
    (merge-pathnames obj-path ".git/objects/")))

(defclass pack ()
  ((%pack :initarg :pack :reader pack-file)
   (%index :initarg :index :reader index-file)))

(defun pack (index pack)
  (fw.lu:new 'pack index pack))

(defun pack-files (repo)
  (mapcar 'pack
          (uiop:directory*
           (merge-pathnames ".git/objects/pack/*.idx"
                            repo))
          (uiop:directory*
           (merge-pathnames ".git/objects/pack/*.pack"
                            repo))))

(defun find-object-in-pack-files (repo id)
  (dolist (pack-file (pack-files repo))
    (multiple-value-bind (pack mid) (find-pack-containing pack-file id)
      (when pack
        (return-from find-object-in-pack-files
          (values pack mid))))))

(defun edges-in-fanout (toc s sha)
  (let* ((fanout-offset (getf toc :fanout)))
    (file-position s (+ fanout-offset (* 4 (1- (elt sha 0)))))
    (destructuring-bind ((_ . cur) (__ . next)) (fwoar.bin-parser:extract '((cur 4 fwoar.bin-parser:be->int)
                                                                            (next 4 fwoar.bin-parser:be->int))
                                                                          s)
      (declare (ignore _ __))
      (values cur next))))

(defun find-sha-between-terms (toc s start end sha)
  (unless (>= start end)
    (let* ((sha-offset (getf toc :shas))
           (mid (floor (+ start end)
                       2)))
      (file-position s (+ sha-offset (* 20 mid)))
      (let ((sha-at-mid (read-bytes 20 'fwoar.bin-parser:byte-array-to-hex-string s)))
        (cond ((string< sha sha-at-mid)
               (find-sha-between-terms toc s start mid sha))
              ((string> sha sha-at-mid)
               (find-sha-between-terms toc s (1+ mid) end sha))
              (t mid))))))

(defun find-pack-containing (pack-file id)
  (with-open-file (s (index-file pack-file) :element-type '(unsigned-byte 8))
    (let ((binary-sha (ironclad:hex-string-to-byte-array id))
          (toc (idx-toc s)))
      (multiple-value-bind (_ end) (edges-in-fanout toc s binary-sha)
        (declare (ignore _))
        (let ((midpoint (find-sha-between-terms toc s 0 end id)))
          (and midpoint
               (values pack-file
                       midpoint)))))))

(defun extract-object-from-pack (pack obj-number)
  (with-open-file (s (index-file pack) :element-type '(unsigned-byte 8))
    (with-open-file (p (pack-file pack) :element-type '(unsigned-byte 8))
      (let* ((toc (idx-toc s))
             (offset-offset (getf toc :4-byte-offsets)))
        (file-position s (+ offset-offset (* 4 obj-number)))
        (let ((object-offset-in-pack (read-bytes 4 'fwoar.bin-parser:be->int s)))
          (file-position p object-offset-in-pack)
          (read-object-from-pack p))))))

(defun extract-loose-object (repo id)
  (with-open-file (s (object repo id)
                     :element-type '(unsigned-byte 8))
    (chipz:decompress nil (chipz:make-dstate 'chipz:zlib)
                      s)))

(defun extract-object (repo id)
  (if (object repo id)
      (extract-loose-object repo id)
      (data-lens.lenses:view *object-data-lens*
                             (multiple-value-call 'extract-object-from-pack 
                               (find-object-in-pack-files (root repo) id)))))

(defparameter *object-data-lens*
  (data-lens.lenses:make-alist-lens :object-data))

(defun turn-read-object-to-string (object)
  (data-lens.lenses:over *object-data-lens* 'babel:octets-to-string object))

(defgeneric branches (repository)
  (:method ((repository repository))
    (get-local-branches (root repository))))

(defgeneric branch (repository name)
  (:method ((repository repository) name)
    (second
     (find name (get-local-branches (root repository))
           :test 'equal
           :key 'car))))

(defgeneric object (repository id)
  (:method ((repository repository) id)
    (car
     (uiop:directory*
      (merge-pathnames (loose-object-path (serapeum:concat id "*"))
                       (root repository))))))

(defun fanout-table (s)
  (coerce (alexandria:assoc-value
           (fwoar.bin-parser:extract '((head 4)
                                       (version 4)
                                       (fanout-table #.(* 4 256) batch-4))
                                     s)
           'fanout-table)
          'vector))

(defun batch-4 (bytes)
  (mapcar 'fwoar.bin-parser:be->int
          (serapeum:batches bytes 4)))

(defun batch-20 (bytes)
  (serapeum:batches bytes 20))

(defun get-object-size (bytes)
  (let ((first (elt bytes 0))
        (rest (subseq bytes 1)))
    (logior (ash (fwoar.bin-parser:be->int rest) 4)
            (logand first 15))))

(defun get-object-type (bytes)
  (let ((first (elt bytes 0)))
    (ldb (byte 3 4)
         first)))

(serapeum:defalias ->sha-string
  (<>1 (data-lens:over 'fwoar.bin-parser:byte-array-to-hex-string)
       batch-20))

(defun get-shas-before (fanout-table first-sha-byte s)
  (let ((num-before (elt fanout-table first-sha-byte))
        (num-total (alexandria:last-elt fanout-table)))
    (values (fwoar.bin-parser:extract (list (list 'shas (* 20 num-before) '->sha-string))
                                      s)
            (- num-total num-before))))

(defun advance-past-crcs (obj-count s)
  (file-position s
                 (+ (file-position s)
                     (* 4 obj-count))))

(defun object-offset (object-number s)
  (file-position s
                 (+ (file-position s)
                     (* (1- object-number)
                        4)))
  (fwoar.bin-parser:extract '((offset 4 fwoar.bin-parser:be->int))
                            s))

(defmacro sym->plist (&rest syms)
  `(list ,@(loop for sym in syms
                 append (list (alexandria:make-keyword sym)
                              sym))))

(defun idx-toc (idx-stream)
  (let* ((object-count (progn (file-position idx-stream 1028)
                              (let ((buf (make-array 4)))
                                (read-sequence buf idx-stream)
                                (fwoar.bin-parser:be->int buf))))
         (signature 0)
         (version 4)
         (fanout 8)
         (shas (+ fanout
                   (* 4 256)))
         (packed-crcs (+ shas
                          (* 20 object-count)))
         (4-byte-offsets (+ packed-crcs
                             (* 4 object-count)))
         (8-byte-offsets-pro (+ 4-byte-offsets
                                 (* object-count 4)))  
         (pack-sha (- (file-length idx-stream)
                       40))
         (8-byte-offsets (when (/= 8-byte-offsets-pro pack-sha)
                           8-byte-offsets-pro))
         (idx-sha (- (file-length idx-stream)
                      20)))
    (values (sym->plist signature
                        version
                        fanout
                        shas
                        packed-crcs
                        4-byte-offsets
                        8-byte-offsets
                        pack-sha
                        idx-sha)
            object-count)))

(defun read-bytes (count format stream)
  (let ((seq (make-array count)))
    (read-sequence seq stream)
    (funcall format
             seq)))

(defun collect-data (idx-toc s num)
  (let ((sha-idx (getf idx-toc :shas))
        (crc-idx (getf idx-toc :packed-crcs))
        (4-byte-offsets-idx (getf idx-toc :4-byte-offsets))
        (8-byte-offsets-idx (getf idx-toc :8-byte-offsets)))
    (values num
            (progn
              (file-position s (+ sha-idx (* num 20)))
              (read-bytes 20 'fwoar.bin-parser:byte-array-to-hex-string s))
            (progn
              (file-position s (+ crc-idx (* num 4)))
              (read-bytes 4 'identity s))
            (progn
              (file-position s (+ 4-byte-offsets-idx (* num 4)))
              (read-bytes 4 'fwoar.bin-parser:be->int s)))))

(defun object-type->sym (object-type)
  (ecase object-type
    (1 :commit)
    (2 :tree)
    (3 :blob)
    (4 :tag)
    (6 :ofs-delta)
    (7 :ref-delta)))

(defun read-object-metadata-from-pack (s)
  (let* ((metadata (fwoar.bin-parser:extract-high s))
         (type (get-object-type metadata))
         (size (get-object-size metadata)))
    (values (cons :type (object-type->sym type))
            (cons :decompressed-size size))))

(defun read-object-from-pack (s)
  (let* ((metadata (fwoar.bin-parser:extract-high s))
         (type (get-object-type metadata))
         (size (get-object-size metadata))
         (object-data (chipz:decompress nil (chipz:make-dstate 'chipz:zlib) s)))
    (list (cons :type (object-type->sym type))
          (cons :decompressed-size size)
          (cons :object-data object-data)
          (cons :raw-data object-data))))

(defun get-first-commits-from-pack (idx pack n)
  (let ((toc (idx-toc idx))
        (result ()))
    (dotimes (i n (reverse result))
      (multiple-value-bind (_ sha __ offset) (collect-data toc idx i)
        (declare (ignore _ __))
        (file-position pack offset)
        (push `((:sha . ,sha)
                ,@(multiple-value-list
                   (read-object-metadata-from-pack pack))
                (:offset . ,offset))
              result))
      )))

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

(defun partition (char string &key from-end)
  (let ((pos (position char string :from-end from-end)))
    (if pos
	      (list (subseq string 0 pos)
	            (subseq string (1+ pos)))
	      (list string
	            nil))))

(defun partition-subseq (subseq string &key from-end)
  (let ((pos (search subseq string :from-end from-end)))
    (if pos
	      (list (subseq string 0 pos)
	            (subseq string (+ (length subseq) pos)))
	      (list string
	            nil))))

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

(defclass git-object ()
  ())
(defclass commit (git-object)
  ())

(defun parse-commit (commit)
  (destructuring-bind (metadata message)
      (partition-subseq #(#\newline #\newline)
                        commit #+(or)(babel:octets-to-string commit :encoding :latin1))
    (values message
            (fwoar.string-utils:split #\newline metadata))))

(defclass git-graph ()
  ((%repo :initarg :repo :reader repo)
   (%depth :initarg :depth :reader depth)
   (%branches :reader branches)
   (%node-cache :reader node-cache :initform (make-hash-table :test 'equal))
   (%edge-cache :reader edge-cache :initform (make-hash-table :test 'equal))))

(defmethod initialize-instance :after ((object git-graph) &key)
  (setf (slot-value object '%branches)
        (fw.lu:alist-string-hash-table
         (funcall (data-lens:over
                   (<>1 (data-lens:applying #'cons)
                        (data-lens:transform-head
                         (serapeum:op (subseq _1 0 (min (length _1) 7))))
                        #'reverse))
                  (branches (repo object))))))

(defun git-graph (repo)
  (fw.lu:new 'git-graph repo))

(defun get-commit-parents (repository commit)
  (map 'list 
       (serapeum:op (second (partition #\space _)))
       (remove-if-not (lambda (it)
                        (serapeum:string-prefix-p "parent" it))
                      (nth-value 1 (parse-commit
                                    (split-object
                                     (chipz:decompress nil (chipz:make-dstate 'chipz:zlib)
                                                       (object repository
                                                               commit))))))))

(defmethod cl-dot:graph-object-node ((graph git-graph) (commit string))
  (alexandria:ensure-gethash commit
                             (node-cache graph)
                             (make-instance 'cl-dot:node
                                            :attributes `(:label ,(gethash #1=(subseq commit 0 7)
                                                                           (branches graph)
                                                                           #1#)))))

(defmethod cl-dot:graph-object-points-to ((graph git-graph) (commit string))
  (mapcar (lambda (c)
            (setf (gethash (list commit c)
                           (edge-cache graph))
                  t)
            c)
          (remove-if (lambda (it)
                       (gethash (list commit it)
                                (edge-cache graph)))
                     (mapcar (serapeum:op (subseq _ 0 7))
                             (get-commit-parents (repo graph) commit)
                             #+nil
                             (loop
                               for cur = (list commit) then parents
                               for parents = (let ((f (get-commit-parents (repo graph) (car cur))))
                                               f)
                               until (or (not parents)
                                         (cdr parents))
                               finally (return (or parents
                                                   (when (not (equal commit (car cur)))
                                                     cur))))))))

