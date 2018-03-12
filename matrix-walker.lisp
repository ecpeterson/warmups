;;;; Problem #37 on watrophy.com

(defun f (tensor)
  (let* ((dims (array-dimensions tensor))
         (path-tensor (make-array dims :initial-element ':unvisited))
         (bottom-corner (make-list (length dims) :initial-element 0)))
    (labels
        ((set-up-location (&rest indices)
           (let ((memoized-value (apply #'aref path-tensor indices)))
             (unless (eql ':unvisited memoized-value)
               (return-from set-up-location memoized-value)))
           (setf (apply #'aref path-tensor indices)
                 (* (apply #'aref tensor indices)
                    (loop :for i :below (length indices)
                          :sum (let ((new-index (copy-seq indices)))
                                 (incf (nth i new-index))
                                 (if (<= (nth i dims) (nth i new-index))
                                     0
                                     (apply #'set-up-location new-index))))))))
      
      (apply #'(setf aref)
             (apply #'aref tensor (mapcar #'1- dims))
             path-tensor
             (mapcar #'1- dims))
      (apply #'set-up-location
             bottom-corner)
      (let ((z (apply #'aref path-tensor bottom-corner)))
        (cond
          ((zerop z) z)
          (t (/ z (abs z))))))))
