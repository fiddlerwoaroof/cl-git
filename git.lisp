(in-package :fwoar.cl-git)

(defun extract-object-metadata-from-pack (pack obj-number)
  (with-pack-streams (s p) pack
    (seek-to-object-in-pack pack s p obj-number)
    (read-object-metadata-from-pack p)))

(defun turn-read-object-to-string (object)
  (data-lens.lenses:over *object-data-lens*
                         'babel:octets-to-string object))

(defun fanout-table (s)
  (coerce (alexandria:assoc-value
           (fwoar.bin-parser:extract '((head 4)
                                       (version 4)
                                       (fanout-table #.(* 4 256) batch-4))
                                     s)
           'fanout-table)
          'vector))

(defun get-object-size (bytes)
  (loop for c across bytes
        for next = (logand c 15) then (logand c #x7f)
        for shift = 0 then (if (= shift 0) 4 (+ shift 7))
        for size = next then (+ size (ash next shift))
        while (> (logand c #x80) 0)
        finally (return size)))

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
         (type-raw (get-object-type metadata))
         (size (get-object-size metadata))
         (type (object-type->sym type-raw)))
    (values (cons :type type)
            (cons :decompressed-size size))))

(defun get-first-commits-from-pack (pack n)
  (let ((toc (idx-toc pack))
        (result ()))
    (with-pack-streams (idx pack-s) pack
      (dotimes (i n (reverse result))
        (multiple-value-bind (_ sha __ offset) (collect-data toc idx i)
          (declare (ignore _ __))
          (file-position pack-s offset)
          (push `((:sha . ,sha)
                  ,@(multiple-value-list
                     (read-object-metadata-from-pack pack-s))
                  (:offset . ,offset))
                result))))))
