(in-package :fwoar.cl-git)

(defun edges-in-fanout (toc s sha)
  (let* ((fanout-offset (getf toc :fanout)))
    (file-position s (+ fanout-offset (* 4 (1- (elt sha 0)))))
    (destructuring-bind ((_ . cur) (__ . next))
        (fwoar.bin-parser:extract '((cur 4 fwoar.bin-parser:be->int)
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
  (with-open-file (s (index-file pack-file)
                     :element-type '(unsigned-byte 8))
    (let ((binary-sha (ironclad:hex-string-to-byte-array id))
          (toc (idx-toc s)))
      (multiple-value-bind (_ end) (edges-in-fanout toc s binary-sha)
        (declare (ignore _))
        (let ((midpoint (find-sha-between-terms toc s 0 end id)))
          (and midpoint
               (values pack-file
                       midpoint)))))))

(defun find-object-in-pack-files (repo id)
  (dolist (pack-file (pack-files repo))
    (multiple-value-bind (pack mid) (find-pack-containing pack-file id)
      (when pack
        (return-from find-object-in-pack-files
          (values pack mid))))))

(defun read-object-from-pack (s)
  (let* ((pos (file-position s))
         (metadata (fwoar.bin-parser:extract-high s))
         (type (object-type->sym (get-object-type metadata)))
         (size (get-object-size metadata))
         (decompressed (if (member type '(:ofs-delta :ref-delta))
                           (let ((buffer (make-array size :element-type '(unsigned-byte 8))))
                             (read-sequence buffer s)
                             buffer)
                           (chipz:decompress nil (chipz:make-dstate 'chipz:zlib) s))))
    (values type
            decompressed
            pos)))

(defun extract-object-of-type (type s pos repository hash)
  (with-simple-restart (continue "Skip object of type ~s" type)
    (-extract-object-of-type (object-type->sym type)
                             s
                             repository
                             :hash hash
                             :offset-from pos)))

(defun extract-object-from-pack (pack obj-number)
  (with-open-file (s (index-file pack) :element-type '(unsigned-byte 8))
    (with-open-file (p (pack-file pack) :element-type '(unsigned-byte 8))
      (let* ((toc (idx-toc s))
             (offset-offset (getf toc :4-byte-offsets)))
        (file-position s (+ offset-offset (* 4 obj-number)))
        (let ((object-offset-in-pack (read-bytes 4 'fwoar.bin-parser:be->int s)))
          (file-position p object-offset-in-pack)
          (read-object-from-pack p))))))

(defun extract-loose-object (file)
  (with-open-file (s file :element-type '(unsigned-byte 8))
    (alexandria:when-let ((result (chipz:decompress nil (chipz:make-dstate 'chipz:zlib)
                                                    s)))
      (destructuring-bind (type rest)
          (partition (char-code #\space) result)
        (values (object-type->sym (babel:octets-to-string type))
                (elt (partition 0 rest)
                     1)
                0)))))

(defgeneric extract-object (object)
  (:method :around ((object git-ref))
    (fw.lu:with-accessors* (ref-repo ref-hash) object
      (multiple-value-call 'extract-object-of-type
        (call-next-method)
        ref-repo
        ref-hash)))
  (:method ((object loose-ref))
    (extract-loose-object (loose-ref-file object)))
  (:method ((object packed-ref))
    (extract-object-from-pack (packed-ref-pack object)
                              (packed-ref-offset object))))
