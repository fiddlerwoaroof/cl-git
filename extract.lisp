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
        (cond ((serapeum:string-prefix-p sha sha-at-mid)
               (values mid sha-at-mid))
              ((string< sha sha-at-mid)
               (find-sha-between-terms toc s start mid sha))
              ((string> sha sha-at-mid)
               (find-sha-between-terms toc s (1+ mid) end sha))
              (t (values mid sha-at-mid)))))))

(defun find-pack-containing (pack-file id)
  (with-open-file (s (index-file pack-file)
                     :element-type '(unsigned-byte 8))
    (let ((binary-sha (ironclad:hex-string-to-byte-array id))
          (toc (idx-toc s)))
      (multiple-value-bind (_ end) (edges-in-fanout toc s binary-sha)
        (declare (ignore _))
        (multiple-value-bind (midpoint sha)
            (find-sha-between-terms toc s 0 end id)
          (and midpoint
               (values pack-file
                       midpoint
                       sha)))))))

(defun find-object-in-pack-files (repo id)
  (dolist (pack-file (pack-files repo))
    (multiple-value-bind (pack mid sha) (find-pack-containing pack-file id)
      (when pack
        (return-from find-object-in-pack-files
          (values pack mid sha))))))

(defun read-object-from-pack (s repository ref)
  (let* ((pos (file-position s))
         (metadata (fwoar.bin-parser:extract-high s))
         (type (object-type->sym (get-object-type metadata)))
         (size (get-object-size metadata))
         (decompressed (if (member type '(:ofs-delta :ref-delta))
                           (let ((buffer (make-array size :element-type '(unsigned-byte 8))))
                             (read-sequence buffer s)
                             buffer)
                           (chipz:decompress nil (chipz:make-dstate 'chipz:zlib) s)))
         (object-data (extract-object-of-type type decompressed repository pos (pathname s) ref)))
    (list (cons :type (object-type->sym type))
          (cons :decompressed-size size)
          (cons :object-data object-data)
          (cons :raw-data decompressed))))

(defun extract-object-of-type (type s repository pos packfile ref)
  (with-simple-restart (continue "Skip object of type ~s at position ~d"
                                 type
                                 pos)
    (-extract-object-of-type (object-type->sym type)
                             s
                             repository
                             :offset-from pos
                             :packfile packfile
                             :hash (ref-hash ref))))

(defun pack-offset-for-object (index-file obj-number)
  (let ((offset-offset (getf index-file
                             :4-byte-offsets)))
    (+ offset-offset
       (* 4 obj-number))))

(defun extract-object-from-pack (pack obj-number ref)
  (with-open-file (s (index-file pack) :element-type '(unsigned-byte 8))
    (with-open-file (p (pack-file pack) :element-type '(unsigned-byte 8))
      (file-position s (pack-offset-for-object (idx-toc s)
                                               obj-number))
      (let ((object-offset-in-pack (read-bytes 4 'fwoar.bin-parser:be->int s)))
        (file-position p object-offset-in-pack)
        (read-object-from-pack p
                               (repository pack)
                               ref)))))

(defun extract-loose-object (repo file ref)
  (with-open-file (s file :element-type '(unsigned-byte 8))
    (alexandria:when-let ((result (chipz:decompress nil (chipz:make-dstate 'chipz:zlib)
                                                    s)))
      (destructuring-bind (type rest)
          (partition (char-code #\space) result)
        (extract-object-of-type (object-type->sym (babel:octets-to-string type))
                                (elt (partition 0 rest)
                                     1)
                                repo
                                0
                                nil
                                ref)))))

(defgeneric extract-object (object)
  (:method ((object loose-ref))
    (extract-loose-object (ref-repo object)
                          (loose-ref-file object)
                          object))
  (:method ((object packed-ref))
    (data-lens.lenses:view *object-data-lens*
                           (extract-object-from-pack (packed-ref-pack object)
                                                     (packed-ref-offset object)
                                                     object))))
