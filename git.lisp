(in-package :fwoar.cl-git)

(defun find-object-in-pack-files (repo id)
  (dolist (pack-file (pack-files repo))
    (multiple-value-bind (pack mid) (find-pack-containing pack-file id)
      (when pack
        (return-from find-object-in-pack-files
          (values pack mid))))))

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

(defun extract-object-from-pack (pack obj-number)
  (with-open-file (s (index-file pack) :element-type '(unsigned-byte 8))
    (with-open-file (p (pack-file pack) :element-type '(unsigned-byte 8))
      (let* ((toc (idx-toc s))
             (offset-offset (getf toc :4-byte-offsets)))
        (file-position s (+ offset-offset (* 4 obj-number)))
        (let ((object-offset-in-pack (read-bytes 4 'fwoar.bin-parser:be->int s)))
          (file-position p object-offset-in-pack)
          (read-object-from-pack p))))))

(defun seek-to-object-in-pack (idx-stream pack-stream obj-number)
  (let* ((toc (idx-toc idx-stream))
         (offset-offset (getf toc :4-byte-offsets)))
    (file-position idx-stream (+ offset-offset (* 4 obj-number)))
    (let ((object-offset-in-pack (read-bytes 4 'fwoar.bin-parser:be->int idx-stream)))
      (file-position pack-stream object-offset-in-pack))))

(defun extract-object-metadata-from-pack (pack obj-number)
  (with-open-file (s (index-file pack) :element-type '(unsigned-byte 8))
    (with-open-file (p (pack-file pack) :element-type '(unsigned-byte 8))
      (seek-to-object-in-pack s p obj-number)
      (read-object-metadata-from-pack p))))

(defun extract-loose-object (repo id)
  (with-open-file (s (object repo id)
                     :element-type '(unsigned-byte 8))
    (alexandria:when-let ((result (chipz:decompress nil (chipz:make-dstate 'chipz:zlib)
                                                    s)))
      (elt (partition 0 result)
           1))))

(defun extract-object (repo id)
  (if (object repo id)
      (extract-loose-object repo id)
      (data-lens.lenses:view *object-data-lens*
                             (multiple-value-call 'extract-object-from-pack 
                               (find-object-in-pack-files (root repo) id)))))


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

(defun get-object-size (bytes)
  (let ((first (elt bytes 0))
        (rest (subseq bytes 1)))
    (logior (ash (fwoar.bin-parser:be->int rest) 4)
            (logand first 15))))

(defun get-object-type (bytes)
  (let ((first (elt bytes 0)))
    (ldb (byte 3 4)
         first)))

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

(defun collect-data (idx-toc s num)
  (let ((sha-idx (getf idx-toc :shas))
        (crc-idx (getf idx-toc :packed-crcs))
        (4-byte-offsets-idx (getf idx-toc :4-byte-offsets))
        (8-byte-offsets-idx (getf idx-toc :8-byte-offsets)))
    (declare (ignore 8-byte-offsets-idx))
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
              result)))))

