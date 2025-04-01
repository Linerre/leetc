(ns backtracking.subset-sum)

;;; Implementation of the algorithms descriped in Algorithms by Jeff Erickson
;;; see https://jeffe.cs.illinois.edu/teaching/algorithms/ (chapter 2.3)

;;; Suppose the source set is an array
;;; This version creates and passes many arrays, which is less efficient
;;; and is subject to StackOverflow once the array size grows to a certain level
;;; For more detail about this algorithm, see chapter 2.3, pp. 77
(defn subset-sum?
  "Check if any subset of X[1..n] of source S sums to the given target T.
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
          wtx (subset-sum? ns wt)       ; without x == sum includes x
          wx  (subset-sum? ns t)]       ; with x    == sum excludes x
      (or wtx wx))))

(assert (true? (subset-sum? [1 2 3] 3)))
(assert (true? (subset-sum? [8, 6, 7, 5, 3, 10, 9] 15)))
(assert (false? (subset-sum? [11, 6, 5, 1, 7, 13, 12] 15)))

;;; Improved subset-sum? that relies on only two values:
;;; 1. a reference to the array (or its starting address)
;;; 2. the length of the prefix.
;;; There are still 2 recursive calls and thus it's not free from stack overflow
;;; The subset sum problem in its natural recursive form requires exploring two
;;; paths (include/exclude) and combining their results with `or`, making it
;;; fundamentally incompatible with simple tail recursion.
(defn subsetsum?
  "Check if any subset of X[1..n] of source S sums to the given target T.
  Return true if there is at least one such X and false otherwise."
  [s n t]
  (cond
    ;; empty set is subset of any set and it sums to 0
    (zero? t) true
    ;; empty source set can only sum to 0 but t != 0
    (or (< t 0) (zero? n)) false

    :else
    (let [x (get s (dec n))
          wt (- t x)
          wtx (subsetsum? s (dec n) wt) ; without x
          wx  (subsetsum? s (dec n) t)] ; with x
      (or wtx wx))))

(assert (true? (subset-sum? [1 2 3] 3)))
(assert (true? (subset-sum? [8, 6, 7, 5, 3, 10, 9] 15)))
(assert (false? (subset-sum? [11, 6, 5, 1, 7, 13, 12] 15)))

(defn build-subset
  "Return a subset of X[1..i] `s` that sums to `t`,
  or nil if no such subset exists."
  [s i t]
  (cond
    (zero? t) []
    (or (< t 0) (zero? i)) nil

    :else
    (let [x (get s (dec i))
          include-x (build-subset s (dec i) t)
          exclude-x (build-subset s (dec i) (- t x))]
      (or include-x
          (some-> exclude-x (conj x))))))

(assert (= [8 7] (build-subset [8, 6, 7, 5, 3, 10, 9] 7 15)))
(assert (= [1 2] (build-subset [1 2 3] 3 3)))
