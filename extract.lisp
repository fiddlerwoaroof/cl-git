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

(defun behead (data)
  (elt (partition 0 data)
       1))

(defun tree-entry (data)
  (values-list (partition 0 data :with-offset 20)))

(defun format-tree-entry (entry)
  (destructuring-bind (info sha) (partition 0 entry)
    (concatenate 'vector
                 (apply #'concatenate 'vector
                        (serapeum:intersperse (vector (char-code #\tab))
                                              (reverse
                                               (partition (char-code #\space)
                                                          info))))
                 (list (char-code #\tab))
                 (babel:string-to-octets (elt (->sha-string sha) 0) :encoding *git-encoding*))))

(defun tree-entries (data &optional accum)
  (if (<= (length data) 0)
      (apply #'concatenate 'vector
             (serapeum:intersperse (vector (char-code #\newline))
                                   (nreverse accum)))
      (multiple-value-bind (next rest) (tree-entry data) 
        (tree-entries rest
                      (list* (format-tree-entry next)
                             accum)))))

(defun extract-object-of-type (type s repository)
  (with-simple-restart (continue "Skip object of type ~s" type)
    (%extract-object-of-type type s repository)))

(defgeneric %extract-object-of-type (type s repository)
  (:method ((type integer) s repository)
    (extract-object-of-type (object-type->sym type)
                            s
                            repository))

  (:method ((type (eql :commit)) s repository)
    s)

  (:method ((type (eql :blob)) s repository)
    s)

  (:method ((type (eql :tag)) s repository)
    s)

  (:method ((type (eql :tree)) s repository)
    (tree-entries s)))

(defun read-object-from-pack (s repository)
  (let* ((metadata (fwoar.bin-parser:extract-high s))
         (type (object-type->sym (get-object-type metadata)))
         (size (get-object-size metadata))
         (decompressed (if (member type '(:ofs-delta :ref-delta))
                           s
                           (chipz:decompress nil (chipz:make-dstate 'chipz:zlib) s)))
         (object-data (extract-object-of-type type decompressed repository)))
    (list (cons :type (object-type->sym type))
          (cons :decompressed-size size)
          (cons :object-data object-data)
          (cons :raw-data object-data))))

(defun extract-object-from-pack (pack obj-number)
  (with-open-file (s (index-file pack) :element-type '(unsigned-byte 8))
    (with-open-file (p (pack-file pack) :element-type '(unsigned-byte 8))
      (let* ((toc (idx-toc s))
             (offset-offset (getf toc :4-byte-offsets)))
        (file-position s (+ offset-offset (* 4 obj-number)))
        (let ((object-offset-in-pack (read-bytes 4 'fwoar.bin-parser:be->int s)))
          (file-position p object-offset-in-pack)
          (read-object-from-pack p (repository pack)))))))

(defun extract-loose-object (repo id)
  (with-open-file (s (loose-object repo id)
                     :element-type '(unsigned-byte 8))
    (alexandria:when-let ((result (chipz:decompress nil (chipz:make-dstate 'chipz:zlib)
                                                    s)))
      (destructuring-bind (type rest)
          (partition (char-code #\space) result)
        (extract-object-of-type (object-type->sym (babel:octets-to-string type))
                                (elt (partition 0 rest)
                                     1)
                                repo)))))

(defun extract-object (repo id)
  (if (loose-object-p repo id)
      (extract-loose-object repo id)
      (data-lens.lenses:view *object-data-lens*
                             (multiple-value-call 'extract-object-from-pack 
                               (find-object-in-pack-files (root repo) id)))))
