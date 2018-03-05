;; Bonus problem: Convert RLE to work with streams instead.
;; (I elected not to solve this version because I don't know anything about
;; custom stream types in CL, and CLHS wasn't very helpful in the instant.)

(defun rle (ell)
  "RLE converts a list of inputs ELL into a list of run-line encoded outputs, written as pairs (OBJ, COUNT)."
  (when (null ell)
    (return-from rle nil))
  (nreverse
   (reduce
    (lambda (acc fresh)
      (destructuring-bind (last-obj last-count) (first acc)
        (cond
          ((eql last-obj fresh)
           (list* (list last-obj (1+ last-count)) (rest acc)))
          (t
           (list* (list fresh 1) acc)))))
    (list* (list (list (first ell) 1)) (rest ell)))))
