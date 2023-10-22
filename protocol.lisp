(in-package :fwoar.cl-git)

(defclass+ blob (git-object)
  ((%data :reader data :initarg :data)))

(defgeneric -extract-object-of-type (type s repository &key  &allow-other-keys)
  (:method :around (type s repository &key hash)
    (let ((result (call-next-method)))
      (prog1 result
        (when (typep result 'git-object)
          (setf (hash result) hash)))))

  (:method ((type (eql :blob)) s repository &key)
    (blob s))

  (:method ((type (eql :tag)) s repository &key)
    s))

(defgeneric component (component object)
  (:argument-precedence-order object component)
  (:method (component (object git-ref))
    (component component (extract-object object)))
  (:method ((component sequence) object)
    (reduce (lambda (cur next)
              (component next cur))
            component
            :initial-value object)))
