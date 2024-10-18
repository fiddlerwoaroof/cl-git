(in-package :fwoar.cl-git)

;; TODO: Update the code so this uses an object instead of a path.
(defvar *git-repository*)
(setf (documentation '*git-repository* 'variable)
      "The git repository path for porcelain commands to operate on.")

(defvar *git-encoding* :utf-8
  "The encoding to use when parsing git objects")

(defun co.fwoar.git:in-repository (root)
  (setf *git-repository*
        (ensure-repository
         (truename root))))

(defun co.fwoar.git:repository ()
  *git-repository*)

(defmacro co.fwoar.git:with-repository ((root) &body body)
  `(let ((*git-repository* (ensure-repository ,root)))
     ,@body))

(defun co.fwoar.git:show-repository ()
  *git-repository*)

(defun in-git-package (symbol)
  (intern (symbol-name symbol)
          :co.fwoar.git))

(defun handle-list (_1)
  (case (in-git-package (car _1))
    (co.fwoar.git::unwrap `(uiop:nest (car)
                                      (mapcar ,@(cdr _1))))
    (t (cons (in-git-package (car _1))
             (cdr _1)))))

(defun co.fwoar.git::resolve-refish (it)
  (flet ((hash-p (it)
           (and (> (length it) 32)
                (every (serapeum:op
                         (digit-char-p _1 16))
                       it))))
    (cond
      ((block is-branch
         (mapc (fw.lu:destructuring-lambda ((name hash))
                 (when (equal it name)
                   (return-from is-branch
                     (ensure-ref hash))))
               (branches *git-repository*))
         nil))
      ((hash-p it) (ensure-ref it)))))

(defmacro co.fwoar.git:git (&rest commands)
  `(uiop:nest ,@(reverse
                 (funcall (data-lens:<>1
                           (data-lens:over (serapeum:op
                                             (typecase _1
                                               (string `(identity ,_1))
                                               (list (handle-list _1)))))
                           (data-lens:transform-head (serapeum:op
                                                       (etypecase _1
                                                         (string `(co.fwoar.git::resolve-refish ,_1))
                                                         (t _1)))))
                          commands))))

(defun co.fwoar.git::ensure-ref (it)
  (ensure-ref it))

(defun co.fwoar.git::decode (it)
  (babel:octets-to-string it :encoding *git-encoding*))

(defun co.fwoar.git::<<= (fun &rest args)
  (apply #'mapcan fun args))

(defmacro co.fwoar.git::map (fun list)
  (alexandria:once-only (list)
    (alexandria:with-gensyms (it)
      `(mapcar ,(if (consp fun)
                    `(lambda (,it)
                       (,(in-git-package (car fun))
                        ,@(cdr fun)
                        ,it))
                    `',(in-git-package fun))
               ,list))))

(defmacro co.fwoar.git::juxt (&rest args)
  (let ((funs (butlast args))
        (arg (car (last args))))
    (alexandria:once-only (arg)
      `(list ,@(mapcar (lambda (f)
                         `(,@(alexandria:ensure-list f) ,arg))
                       funs)))))

(defmacro co.fwoar.git::pipe (&rest funs)
  (let ((funs (reverse (butlast funs)))
        (var (car (last funs))))
    `(uiop:nest ,@(mapcar (lambda (it)
                            (if (consp it)
                                `(,(in-git-package (car it)) ,@(cdr it))
                                `(,(in-git-package it))))
                          funs)
                ,var)))

(defun co.fwoar.git::filter (fun &rest args)
  (apply #'remove-if-not fun args))

(defun co.fwoar.git::object (thing)
  (extract-object thing))

(defun co.fwoar.git:show (object)
  (extract-object object))

(defun co.fwoar.git:contents (object)
  (co.fwoar.git:show object))

(defun co.fwoar.git:component (&rest args)
  (let ((component-list (butlast args))
        (target (car (last args))))
    (fwoar.cl-git::component component-list target)))

(defun co.fwoar.git:tree (commit-object)
  (component :tree
             commit-object))

(defun co.fwoar.git::filter-tree (name-pattern tree)
  #+lispworks
  (declare (notinline serapeum:string-prefix-p))
  (let* ((tree-entries (component :entries tree))
         (scanner (cl-ppcre:create-scanner name-pattern)))
    (remove-if-not (serapeum:op
                     (cl-ppcre:scan scanner _))
                   tree-entries
                   :key #'te-name)))

(defun co.fwoar.git:branch (&optional (branch nil branch-p))
  #+lispworks
  (declare (notinline serapeum:assocadr))
  (let* ((branches (branches *git-repository*))
         (branch-hash (if branch-p
                          (serapeum:assocadr (etypecase branch
                                               (string branch)
                                               (keyword (string-downcase branch)))
                                             branches
                                             :test 'equal)
                          (or (serapeum:assocadr "master" branches :test 'equal)
                              (serapeum:assocadr "main" branches :test 'equal)))))
    (if branch-hash
        (ref *git-repository*
             branch-hash)
        (error "branch ~s not found" branch))))

(defun co.fwoar.git:branches ()
  (branches *git-repository*))

(defun co.fwoar.git::parents (commit)
  (mapcar 'ensure-ref
          (component :parents commit)))
(defun co.fwoar.git:commit-parents (commit)
  (co.fwoar.git::parents commit))

(defun co.fwoar.git:rev-list (ref-id &optional (limit nil limit-p))
  "Return the commits reachable from the ref."
  (when limit-p
    (rotatef ref-id limit))
  (let ((seen (make-hash-table)))
    (labels ((iterate (queue accum &optional (count 0))
               (if (or (when limit-p
                         (= limit count))
                       (null queue))
                   accum
                   (destructuring-bind (next . rest) queue
                     (let ((parents (co.fwoar.git::parents next)))
                       (iterate (append rest parents)
                         (if (gethash next seen)
                             accum
                             (progn
                               (setf (gethash next seen) t)
                               (cons next accum)))
                         (1+ count)))))))
      (iterate (list (ensure-ref ref-id))
        ()))))
