(in-package :fwoar.cl-git)

(defclass delta (git-object)
  ((%repository :initarg :repository :reader repository)
   (%base :initarg :base :reader base)
   (%commands :initarg :commands :reader commands)))

(defclass+ ofs-delta (delta)
  ())

(defclass+ ref-delta (delta)
  ())

(defun make-ofs-delta (base commands repository)
  (fw.lu:new 'ofs-delta base commands repository))
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

(defun expand-copy (copy)
  ;; TODO: implement this
  copy)

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
                                   '(vector (unsigned-byte 8))))))))
      (loop while (< idx (length data))
            collect (get-command)))))


(defun get-ofs-delta-offset (buf)
  (let ((idx 0))
    (flet ((advance ()
             (prog1 (elt buf idx)
               (incf idx))))
      (let* ((c (advance))
             (ofs (logand c 127)))
        (loop
          do (format t "~&~s ~s ~s" idx c ofs)
          while (> (logand c 128) 0)
          do
             (setf c (advance))
             (setf ofs (+ (ash (1+ ofs)
                               7)
                          (logand c 127))))
        (values (- ofs) idx)))))

(defmethod -extract-object-of-type ((type (eql :ofs-delta)) s repository &key offset-from packfile)
  (multiple-value-bind (offset consumed) (get-ofs-delta-offset s)
    (make-ofs-delta (list packfile
                          (+ offset-from offset))
                    (partition-commands (chipz:decompress
                                         nil
                                         (chipz:make-dstate 'chipz:zlib)
                                         (subseq s consumed)))
                    repository)))
(defmethod -extract-object-of-type ((type (eql :ref-delta)) s repository &key offset-from)
  (make-ref-delta offset-from
                  (partition-commands s)
                  repository))


#+(or) #+(or) #+(or)

(defmethod component ((component (eql :tree)) (object git-commit))
  (ensure-ref
   (cadr
    (fw.lu:v-assoc :tree (metadata object)
                   :test 'string-equal))))
(defmethod component ((component (eql :parents)) (object git-commit))
  (coerce (remove-if-not (serapeum:op
                           (string= "parent" _))
                         (metadata object)
                         :key #'car)
          'list))
(defmethod component ((component (eql :message)) (object git-commit))
  (data object))
