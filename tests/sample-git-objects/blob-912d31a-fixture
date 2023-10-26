(in-package :fwoar.cl-git)

(fw.lu:defun-ct batch-4 (bytes)
  (mapcar 'fwoar.bin-parser:be->int
          (serapeum:batches bytes 4)))

(fw.lu:defun-ct batch-20 (bytes)
  (serapeum:batches bytes 20))

(defmacro sym->plist (&rest syms)
  `(list ,@(loop for sym in syms
                 append (list (alexandria:make-keyword sym)
                              sym))))

(defmacro inspect- (s form)
  `(let ((result ,form))
     (format ,s "~&~s (~{~s~^ ~})~%~4t~s~%"
             ',form
             ,(typecase form
                (list `(list ',(car form) ,@(cdr form)))
                (t `(list ,form)))
             result)
     result))

(defun inspect-* (fn)
  (lambda (&rest args)
    (declare (dynamic-extent args))
    (inspect- *trace-output*
              (apply fn args))))

(defun partition (char string &key from-end)
  (let ((pos (position char string :from-end from-end)))
    (if pos
	      (list (subseq string 0 pos)
	            (subseq string (1+ pos)))
	      (list string
	            nil))))

(defun partition-subseq (subseq string &key from-end)
  (let ((pos (search subseq string :from-end from-end)))
    (if pos
	      (list (subseq string 0 pos)
	            (subseq string (+ (length subseq) pos)))
	      (list string
	            nil))))

(serapeum:defalias ->sha-string
  (data-lens:<>1 (data-lens:over 'fwoar.bin-parser:byte-array-to-hex-string)
                 'batch-20))
