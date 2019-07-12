(in-package :fwoar.cl-git)

(defun offset-distance (vec)
  (+ (loop for v across vec
           for sum = (logand 127 v)
             then (+ (ash sum 7)
                     (logand 127 v))
           finally (return sum))
     (loop for x from 1 below 2
           sum (expt 2 (* 7 x)))))

(defun extract-offset-to-base (s)
  (offset-distance
   (fwoar.bin-parser:extract-high s)))

(defun object-metadata-at-offset (base-position offset s)
  (file-position s (- base-position offset))
  (read-object-metadata-from-pack s))
