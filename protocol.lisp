(in-package :fwoar.cl-git.protocol)

(defgeneric -extract-object-of-type (type s repository &key  &allow-other-keys)
  (:method :around (type s repository &key hash)
    (let ((result (call-next-method)))
      (prog1 result
        (when (typep result 'fwoar.cl-git:git-object)
          (setf (fwoar.cl-git:hash result) hash)))))


  (:method ((type (eql :tag)) s repository &key)
    s))

(defgeneric component (component object)
  (:argument-precedence-order object component)
  (:method (component (object fwoar.cl-git:git-ref))
    (component component (fwoar.cl-git:extract-object object)))
  (:method ((component sequence) object)
    (reduce (lambda (cur next)
              (component next cur))
            component
            :initial-value object)))


(defmacro defcomponent (component &body body)
  (declare (ignore component body))
  (error "defcomponent not available on its own"))
(defmacro defcomponents (class (object-sym component-sym) &body clauses)
  `(macrolet ((defcomponent (component &body component-body)
                `(defmethod component ((,',component-sym ,component)
                                       (,',object-sym ,',class))
                   ,@component-body)))
     ,@(loop for (component . component-body) in clauses
             collect `(defcomponent ,component
                        ,@component-body))))
