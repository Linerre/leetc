(ns backtracking.subset-sum)

;;; Implementation of the algorithms descriped in Algorithms by Jeff Erickson
;;; see https://jeffe.cs.illinois.edu/teaching/algorithms/ (chapter 2.3)

;;; Suppose the source set is an array


(defn subset-sum?
  "Check if any subset of X[0..n] of source S sums to the given target T.
  Return true if there is at least one such X and false otherwise."
  [s t]
  (cond
    ;; empty set is subset of any set and it sums to 0
    (zero? t) true
    ;; empty source set can only sum to 0 but t != 0
    (or (< t 0) (empty? s)) false

    :else
    (let [x (peek s)
          ns (pop s)
          wt (- t x)
          wtx (subset-sum? ns wt)       ; without x
          wx  (subset-sum? ns t)]       ; with x
      (or wtx wx))))

(assert (true? (subset-sum? [1 2 3] 3)))
(assert (true? (subset-sum? [8, 6, 7, 5, 3, 10, 9] 15)))
(assert (false? (subset-sum? [11, 6, 5, 1, 7, 13, 12] 15)))
