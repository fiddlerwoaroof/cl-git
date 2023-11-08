(in-package :fwoar.cl-git.pack)

(defclass pack ()
  ((%pack :initarg :pack :reader pack-file)
   (%index :initarg :index :reader index-file)
   (%repository :initarg :repository :reader fwoar.cl-git:repository)))
(defun pack (index pack repository)
  (fw.lu:new 'pack index pack repository))

(defclass packed-ref (fwoar.cl-git::git-ref)
  ((%pack :initarg :pack :reader packed-ref-pack)
   (%offset :initarg :offset :reader packed-ref-offset)))

(defmacro with-pack-streams ((idx-sym pack-sym) pack &body body)
  (alexandria:once-only (pack)
    `(with-open-file (,idx-sym (index-file ,pack) :element-type 'fwoar.cl-git.types:octet)
       (with-open-file (,pack-sym (pack-file ,pack) :element-type 'fwoar.cl-git.types:octet)
         ,@body))))

(defgeneric idx-toc (pack)
  (:method ((pack pack))
    (with-pack-streams (idx-stream _) pack
      (let* ((object-count (progn (file-position idx-stream 1028)
                                  (let ((buf (make-array 4)))
                                    (read-sequence buf idx-stream)
                                    (fwoar.bin-parser:be->int buf))))
             (signature 0)
             (version 4)
             (fanout 8)
             (shas (+ fanout
                      #.(* 4 256)))
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
        (values (fwoar.cl-git.utils:sym->plist signature
                                               version
                                               fanout
                                               shas
                                               packed-crcs
                                               4-byte-offsets
                                               8-byte-offsets
                                               pack-sha
                                               idx-sha)
                object-count)))))

(defun edges-in-fanout (toc s sha)
  (let* ((fanout-offset (getf toc :fanout)))
    (file-position s (+ fanout-offset (* 4 (1- (elt sha 0)))))
    (destructuring-bind ((_ . cur) (__ . next))
        (fwoar.bin-parser:extract '((cur 4 fwoar.bin-parser:be->int)
                                    (next 4 fwoar.bin-parser:be->int))
                                  s)
      (declare (ignore _ __))
      (values cur next))))

(defun extract-object-at-pos (pack pos ref)
  (with-open-file (p (fwoar.cl-git.pack:pack-file pack) :element-type '(unsigned-byte 8))
    (file-position p pos)
    (read-object-from-pack p
                           (fwoar.cl-git:repository pack)
                           ref)))

(defun extract-object-from-pack (pack obj-number ref)
  (let ((object-offset-in-pack (read-4-byte-offset pack obj-number)))
    (extract-object-at-pos pack
                           object-offset-in-pack
                           ref)))

(defun find-object-in-pack-files (repo id)
  (dolist (pack-file (fwoar.cl-git::pack-files repo))
    (multiple-value-bind (pack mid sha) (find-sha-in-pack pack-file id)
      (when pack
        (return-from find-object-in-pack-files
          (values pack mid sha))))))

(defun find-sha-between-terms (toc s start end sha)
  (unless (>= start end)
    (let* ((sha-offset (getf toc :shas))
           (mid (floor (+ start end)
                       2)))
      (file-position s (+ sha-offset (* 20 mid)))
      (let ((sha-at-mid (fwoar.cl-git.utils:read-bytes
                         20 'fwoar.bin-parser:byte-array-to-hex-string s)))
        (cond ((serapeum:string-prefix-p sha sha-at-mid)
               (values mid sha-at-mid))
              ((string< sha sha-at-mid)
               (find-sha-between-terms toc s start mid sha))
              ((string> sha sha-at-mid)
               (find-sha-between-terms toc s (1+ mid) end sha))
              (t (values mid sha-at-mid)))))))

(defun find-sha-in-pack (pack-file id)
  (with-open-file (s (fwoar.cl-git.pack:index-file pack-file)
                     :element-type '(unsigned-byte 8))
    (let ((binary-sha (ironclad:hex-string-to-byte-array id))
          (toc (fwoar.cl-git.pack:idx-toc pack-file)))
      (multiple-value-bind (_ end) (edges-in-fanout toc s binary-sha)
        (declare (ignore _))
        (multiple-value-bind (midpoint sha)
            (find-sha-between-terms toc s 0 end id)
          (and midpoint
               (values pack-file
                       midpoint
                       sha)))))))

(defun get-object-from-pack (s)
  (let* ((metadata (fwoar.bin-parser:extract-high s))
         (type (fwoar.cl-git::object-type->sym (fwoar.cl-git::get-object-type metadata)))
         (size (fwoar.cl-git::get-object-size metadata)))
    (case type
      (:ref-delta (error ":ref-delta not implemented yet"))
      (:ofs-delta (get-ofs-delta-offset-streaming s)))
    (let ((decompressed (chipz:decompress nil (chipz:make-dstate 'chipz:zlib) s)))
      (values (concatenate
               '(vector fwoar.cl-git.types:octet)
               (ecase type
                 (:commit #.(babel:string-to-octets "commit" :encoding :ascii))
                 (:blob #.(babel:string-to-octets "blob" :encoding :ascii))
                 (:tree #.(babel:string-to-octets "tree" :encoding :ascii)))
               #(32)
               (babel:string-to-octets (prin1-to-string size ):encoding :ascii)
               #(0)
               decompressed)
              size
              (length decompressed)))))

(defun get-ofs-delta-offset-streaming (buf)
  (let* ((idx 0))
    (flet ((advance ()
             (read-byte buf)))
      (loop
        for c = (advance)
        for ofs = (logand c 127) then (+ (ash (1+ ofs)
                                              7)
                                         (logand c 127))
        while (> (logand c 128) 0)
        finally
           (return (values (- ofs) idx))))))

(defun pack-offset-for-object (index-file obj-number)
  (let ((offset-offset (getf index-file
                             :4-byte-offsets)))
    (+ offset-offset
       (* 4 obj-number))))

(defun packed-ref (repo id)
  (multiple-value-bind (pack offset sha) (find-object-in-pack-files repo id)
    (when pack
      (make-instance 'packed-ref
                     :hash sha
                     :repo repo
                     :offset offset
                     :pack pack))))

(defun raw-object-for-ref (packed-ref)
  (let ((pack (packed-ref-pack packed-ref)))
    (fwoar.cl-git.pack:with-pack-streams (i p) pack
      (file-position p (read-4-byte-offset pack
                                           (packed-ref-offset packed-ref)))
      (get-object-from-pack p))))

(defun read-4-byte-offset (pack obj-number)
  (fwoar.cl-git.pack:with-pack-streams (s _) pack
    (file-position s
                   (pack-offset-for-object (fwoar.cl-git.pack:idx-toc pack)
                                           obj-number))
    (fwoar.cl-git.utils:read-bytes 4 'fwoar.bin-parser:be->int s)))

(defun read-object-from-pack (s repository ref)
  (let* ((pos (file-position s))
         (metadata (fwoar.bin-parser:extract-high s))
         (type (fwoar.cl-git::object-type->sym (fwoar.cl-git::get-object-type metadata)))
         (size (fwoar.cl-git::get-object-size metadata))
         (delta-base (case type
                       (:ref-delta (error ":ref-delta not implemented yet"))
                       (:ofs-delta (fwoar.cl-git::get-ofs-delta-offset-streaming s))))
         (decompressed (chipz:decompress nil (chipz:make-dstate 'chipz:zlib) s))
         (object-data (fwoar.cl-git::extract-object-of-type type decompressed repository pos (pathname s) ref delta-base)))
    (list (cons :type (fwoar.cl-git::object-type->sym type))
          (cons :decompressed-size size)
          (cons :object-data object-data)
          (cons :raw-data decompressed))))

(defun seek-to-object-in-pack (pack idx-stream pack-stream obj-number)
  (let* ((toc (idx-toc pack))
         (offset-offset (getf toc :4-byte-offsets)))
    (file-position idx-stream (+ offset-offset (* 4 obj-number)))
    (let ((object-offset-in-pack (fwoar.cl-git.utils:read-bytes
                                  4 'fwoar.bin-parser:be->int idx-stream)))
      (values (file-position pack-stream object-offset-in-pack)
              object-offset-in-pack))))

(defparameter *want-delta* nil)
(defmethod fwoar.cl-git::extract-object ((object packed-ref))
  (let ((maybe-delta (data-lens.lenses:view fwoar.cl-git::*object-data-lens*
                                            (extract-object-from-pack
                                             (fwoar.cl-git.pack::packed-ref-pack object)
                                             (fwoar.cl-git.pack::packed-ref-offset object)
                                             object))))
    (if *want-delta*
        maybe-delta
        (fwoar.cl-git::resolve-delta object
                                     maybe-delta))))
