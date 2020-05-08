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

(defclass delta ()
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

(defun partition-commands (data)
  (let ((idx 0))
    (labels ((advance ()
               (prog1 (elt data idx)
                 (incf idx)))
             (get-command ()
               (let* ((bv (bit-smasher:int->bits (elt data idx)))
                      (discriminator (elt bv 0))
                      (insts (subseq bv 1)))
                 (incf idx)
                 (if (= 1 discriminator)
                     (list :copy
                           insts
                           (coerce (loop repeat (count 1 insts) collect (advance))
                                   '(vector (unsigned-byte 8))))
                     (list :add
                           (coerce (loop repeat (1- (bit-smasher:bits->int (reverse insts)))
                                         collect (advance))
                                   '(vector (unsigned-byte 8))))))))
      (loop while (< idx (length data))
            collect (get-command)))))

(defmethod -extract-object-of-type ((type (eql :ofs-delta)) s repository &key offset-from)
  (format t "~&data: ~s~%" s)
  (make-ofs-delta offset-from
                  (partition-commands s)
                  repository))
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
