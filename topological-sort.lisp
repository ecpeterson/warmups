(defun f (G H T-list)
  "F defines a function (G H T-list) --> (G H' T'-list) where the inputs are subject to the following constraints:
 * G, the AMBIENT DAG, is a list of (ordered) edges.
 * H is a sub-DAG of G, represented as a sublist.
 * T-list is a topologically sorted list of vertices in G (possibly intersecting H).

The limiting behavior of F is then determined by the following properties:
 * H' <= H gives a subgraph of H.
 * T' >= T gives a superset of T, and it remains topologically sorted against G.
 * The *only* fixed points of F are the triples (G, nil, T)."
  ;; first deal with nontrivially intersecting H and T-list
  (setf H (remove-if (lambda (e)
                       (or (member (first e) T-list)
                           (member (second e) T-list)))
                     H))
  ;; if H is empty, just kick back & relax
  (when (null H)
    (return-from f
      (values G H T-list)))
  ;; find a source vertex in H
  (let (target-vertex
        (vertex-list (union (mapcar #'first H)
                            (mapcar #'second H))))
    (dolist (edge H)
      (setf vertex-list
            (remove-if (lambda (v) (eql v (second edge)))
                       vertex-list)))
    (setf target-vertex (first vertex-list))
    ;; remove all exiting edges from target-vertex
    (let ((Hprime (remove-if (lambda (e) (eql target-vertex (first e))) H)))
      ;; find a place to insert target-vertex in T-list
      (labels ((Tprime-producer (out-list in-list)
                 ;; check to see if the top element of T-list points to us
                 (cond
                   ((member (list (first in-list) target-vertex)
                            G
                            :test #'equal)
                    (append (nreverse out-list)
                            (list target-vertex)
                            in-list))
                   ((null in-list)
                    (nreverse (cons target-vertex out-list)))
                   (t
                    (Tprime-producer (cons (first in-list) out-list)
                                     (rest out-list))))))
        (values G Hprime (Tprime-producer nil T-list))))))
