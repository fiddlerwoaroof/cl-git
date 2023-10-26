(in-package :fwoar.cl-git)

(defclass delta (git-object)
  ((%repository :initarg :repository :reader repository)
   (%base :initarg :base :reader base)
   (%commands :initarg :commands :reader commands)
   (%src-size :initarg :src-size :reader src-size)
   (%delta-size :initarg :delta-size :reader delta-size))
  (:documentation
   "The base type for deltified git objects"))

(defclass+ ofs-delta (delta)
  ())

(defclass+ ref-delta (delta)
  ()
  (:documentation "TODO: mostly unimplemented/untested"))

(defun make-ofs-delta (base commands repository src-size delta-size)
  (fw.lu:new 'ofs-delta base commands repository src-size delta-size))
(defun make-ref-delta (base commands repository)
  (fw.lu:new 'ofs-delta base commands repository))

(defun int->bit-vector (n)
  (let* ((integer-length (integer-length n))
         (bv-size (* 8 (ceiling integer-length 8)))
         (bv (make-array bv-size :element-type 'bit)))
    (loop :for ix :below integer-length
          :do (setf (aref bv (- bv-size 1 ix))
                    (if (logbitp ix n)
                        1
                        0)))
    bv))

(defun bit-vector->int (bv)
  (let ((bv-size (array-total-size bv)))
    (loop :for ix :from (1- bv-size) :downto 0
          :for n :from 0
          :unless (zerop (aref bv ix))
            :sum (expt 2 n))))

(defun trace-bases (pack delta)
  (if (typep delta 'delta)
      (let* ((offset (second (base delta)))
             (o (extract-object-at-pos pack
                                       offset
                                       (make-instance 'git-ref
                                                      :hash "00000000"
                                                      :repo nil)))
             (obj (serapeum:assocdr :object-data o))
             (raw (serapeum:assocdr :raw-data o)))
        (if (typep obj 'delta)
            (apply-commands (trace-bases pack obj)
                            (commands delta))
            (apply-commands (trace-bases pack raw)
                            (commands delta))))
      delta))

(defun partition-commands (data)
  (let ((idx 0))
    (labels ((advance ()
               (if (>= idx (length data))
                   (progn (incf idx)
                          0)
                   (prog1 (elt data idx)
                     (incf idx))))
             (get-command ()
               (let* ((bv (int->bit-vector (elt data idx)))
                      (discriminator (elt bv 0))
                      (insts (subseq bv 1)))
                 (incf idx)
                 (if (= 1 discriminator)
                     (expand-copy
                      (list :copy
                            insts
                            (coerce (loop repeat (count 1 insts) collect (advance))
                                    '(vector (unsigned-byte 8)))))
                     (list :add
                           (coerce (loop repeat (bit-vector->int insts)
                                         collect (advance))
                                   '(vector (unsigned-byte 8)))))))
             (expand-copy (copy)
               (destructuring-bind (command layout numbers) copy
                 (let* ((next-idx 0)
                        (parts (map '(vector (unsigned-byte 8))
                                    (lambda (layout-bit)
                                      (if (= layout-bit 1)
                                          (prog1 (elt numbers next-idx)
                                            (incf next-idx))
                                          0))
                                    (reverse layout))))
                   (list command
                         (fwoar.bin-parser:le->int (subseq parts 0 4))
                         (fwoar.bin-parser:le->int (subseq parts 4)))))))
      (loop while (< idx (length data))
            collect (get-command)))))

(defun apply-commands (base commands)
  (flexi-streams:with-output-to-sequence (s)
    (flet ((do-copy (offset cnt)
             (write-sequence (subseq base offset (+ offset cnt))
                             s))
           (do-add (data)
             (write-sequence data s)))
      (loop for (command . args) in commands
            when (eql command :copy) do
              (apply #'do-copy args)
            when (eql command :add) do
              (apply #'do-add args)))))

(defun get-ofs-delta-offset (buf)
  (let* ((idx 0))
    (flet ((advance ()
             (prog1 (elt buf idx)
               (incf idx))))
      (loop
        for c = (advance)
        for ofs = (logand c 127) then (+ (ash (1+ ofs)
                                              7)
                                         (logand c 127))
        while (> (logand c 128) 0)
        finally
           (return (values (- ofs) idx))))))

(defun decode-size (buf)
  (let ((parts ()))
    (loop for raw across buf
          for bits = (int->bit-vector raw)
          for morep = (= (elt bits 0) 1)
          do (push (subseq bits 1) parts)
          while morep)
    (let ((result (make-array (* 7 (length parts))
                              :element-type 'bit)))
      (loop for x from 0 by 7
            for part in parts
            do
               (replace result part :start1 x))
      (values (bit-vector->int result)
              (length parts)))))

(defmethod -extract-object-of-type ((type (eql :ofs-delta)) s repository &key offset-from packfile)
  (multiple-value-bind (offset consumed) (get-ofs-delta-offset s)
    (let ((compressed-data (chipz:decompress
                            nil
                            (chipz:make-dstate 'chipz:zlib)
                            (subseq s consumed))))
      (multiple-value-bind (src-size consumed-1) (decode-size compressed-data)
        (multiple-value-bind (delta-size consumed-2) (decode-size (subseq compressed-data
                                                                          consumed-1))
          (make-ofs-delta (list packfile
                                (+ offset-from offset))
                          (partition-commands (subseq compressed-data
                                                      (+ consumed-1
                                                         consumed-2)))
                          repository
                          src-size
                          delta-size))))))
(defmethod -extract-object-of-type ((type (eql :ref-delta)) s repository &key offset-from)
  (make-ref-delta offset-from
                  (partition-commands s)
                  repository))
