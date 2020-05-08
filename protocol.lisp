(in-package :fwoar.cl-git)

(defgeneric -extract-object-of-type (type s repository &key &allow-other-keys)
  (:method ((type (eql :blob)) s repository &key)
    s)

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
