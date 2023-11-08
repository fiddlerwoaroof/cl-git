(in-package :fwoar.cl-git)

(defun extract-object-of-type (type s repository pos packfile ref delta-base)
  (with-simple-restart (continue "Skip object of type ~s at position ~d"
                                 type
                                 pos)
    (-extract-object-of-type (object-type->sym type)
                             s
                             repository
                             :offset-from pos
                             :packfile packfile
                             :hash (ref-hash ref)
                             :base delta-base)))

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
                                ref
                                nil)))))

(defgeneric extract-object (object)
  (:method ((object loose-ref))
    (extract-loose-object (ref-repo object)
                          (loose-ref-file object)
                          object)))
