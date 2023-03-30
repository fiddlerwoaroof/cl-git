(in-package :fwoar.cl-git)

;; TODO: Update the code so this uses an object instead of a path.
(defvar *git-repository*)
(setf (documentation '*git-repository* 'variable)
      "The git repository path for porcelain commands to operate on.")

(defvar *git-encoding* :utf-8
  "The encoding to use when parsing git objects")

(defun git:in-repository (root)
  (setf *git-repository*
        (ensure-repository
         (truename root))))

(defun git:repository ()
  *git-repository*)

(defmacro git:with-repository ((root) &body body)
  `(let ((*git-repository* (ensure-repository ,root)))
     ,@body))

(defun git:show-repository ()
  *git-repository*)

(defun in-git-package (symbol)
  (intern (symbol-name symbol)
          :git))

(defun handle-list (_1)
  (case (in-git-package (car _1))
    (git::unwrap `(uiop:nest (car)
                             (mapcar ,@(cdr _1))))
    (t (cons (in-git-package (car _1))
             (cdr _1)))))

(defun git::resolve-refish (it)
  (flet ((hash-p (it)
           (and (> (length it) 32)
                (every (serapeum:op
                         (digit-char-p _1 16))
                       it))))
    (cond
      ((let ((it "master"))
         (block is-branch
           (mapc (fw.lu:destructuring-lambda ((name hash))
                   (when (equal it name)
                     (return-from is-branch
                       (ensure-ref hash))))
                 (branches *git-repository*))
           nil)))
      ((hash-p it) (ensure-ref it)))))

(defmacro git:git (&rest commands)
  `(uiop:nest ,@(reverse
                 (funcall (data-lens:<>1
                           (data-lens:over (serapeum:op
                                             (typecase _1
                                               (string `(identity ,_1))
                                               (list (handle-list _1)))))
                           (data-lens:transform-head (serapeum:op
                                                       (etypecase _1
                                                         (string `(resolve-refish ,_1))
                                                         (t _1)))))
                          commands))))

(defun git::ensure-ref (it)
  (ensure-ref it))

(defun git::decode (it)
  (babel:octets-to-string it :encoding *git-encoding*))

(defun git::<<= (fun &rest args)
  (apply #'mapcan fun args))

(defmacro git::map (fun list)
  (alexandria:once-only (list)
    (alexandria:with-gensyms (it)
      `(mapcar ,(if (consp fun)
                    `(lambda (,it)
                       (,(in-git-package (car fun))
                        ,@(cdr fun)
                        ,it))
                    `',(in-git-package fun))
               ,list))))

(defmacro git::juxt (&rest args)
  (let ((funs (butlast args))
        (arg (car (last args))))
    (alexandria:once-only (arg)
      `(list ,@(mapcar (lambda (f)
                         `(,@(alexandria:ensure-list f) ,arg))
                       funs)))))

(defmacro git::pipe (&rest funs)
  (let ((funs (reverse (butlast funs)))
        (var (car (last funs))))
    `(uiop:nest ,@(mapcar (lambda (it)
                            (if (consp it)
                                `(,(in-git-package (car it)) ,@(cdr it))
                                `(,(in-git-package it))))
                          funs)
                ,var)))

(defun git::filter (fun &rest args)
  (apply #'remove-if-not fun args))

(defun git::object (thing)
  (extract-object thing))

(defun git:show (object)
  (extract-object object))

(defun git:contents (object)
  (git:show object))

(defun git:component (&rest args)
  (let ((component-list (butlast args))
        (target (car (last args))))
    (fwoar.cl-git::component component-list target)))

(defun git:tree (commit-object)
  (component :tree
             commit-object))

(defun git::filter-tree (name-pattern tree)
  #+lispworks
  (declare (notinline serapeum:string-prefix-p))
  (let* ((tree-entries (component :entries tree))
         (scanner (cl-ppcre:create-scanner name-pattern)))
    (remove-if-not (serapeum:op
                     (cl-ppcre:scan scanner _))
                   tree-entries
                   :key #'te-name)))

(defun git:branch (&optional (branch :master))
  #+lispworks
  (declare (notinline serapeum:assocadr))
  (let ((branches (branches *git-repository*)))
    (ref *git-repository*
         (serapeum:assocadr (etypecase branch
                              (string branch)
                              (keyword (string-downcase branch)))
                            branches
                            :test 'equal))))

(defun git:branches ()
  (branches *git-repository*))

(defun git::parents (commit)
  (alexandria:mappend (data-lens:over 'ensure-ref)
                      (component :parents commit)))
(defun git:commit-parents (commit)
  (git::parents commit))

(defun git:rev-list (ref-id &optional (limit nil limit-p))
  "Return the commits reachable from the ref."
  (when limit-p
    (rotatef ref-id limit))
  (labels ((iterate (queue accum &optional (count 0))
             (if (or (when limit-p
                       (= limit count))
                     (null queue))
                 accum
                 (destructuring-bind (next . rest) queue
                   (iterate (append rest
                                    (git::parents next))
                     (cons next accum)
                     (1+ count))))))
    (remove-duplicates (iterate (list (ensure-ref ref-id))
                         ())
                       :test 'equal
                       :key 'ref-hash)))
