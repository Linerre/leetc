(ns backtracking.longest-subseq)

;;; Implementation of the LISBigger in chapter 2.6 of Algorithms by Jeff Eirckson
;;; see: https://jeffe.cs.illinois.edu/teaching/algorithms/book/02-backtracking.pdf
;;; It is hard for this algorithm to be tail recursive without an auxilary function
;;; The below version is not tail optiomized recursion
(defn lisbigger
  "Find the longest increasing subsequence in the ginven vector `nums`.
  `prev` must be smaller than any numbers in `nums`. ##-Inf might be a good choice.
  Return the length of the such a sequence."
  [prev nums]
  (let [next (first nums)
        rm (rest nums)]
    (cond
      (empty? nums) 0
      (nil? next) 0
      (empty? rm) (if (< prev next) 1 0)
      (<= next prev) (lisbigger prev rm)
      :else
      (let [skip (lisbigger prev rm)
            take (lisbigger next rm)]
        (max skip (inc take))))))

(assert (= 2 (lisbigger 0 [4 3 1 9])))
(assert (= 4 (lisbigger 0 [3, 1, 5, 2, 7, 4, 8])))
(assert (= 4 (lisbigger ##-Inf [3, 1, 5, 2, 7, 4, 8])))
(assert (= 5 (lisbigger 0 [1 2 3 4 5])))

;;; Suggested by Claude
(defn lisbigger2
  "Find the longest increasing subsequence where all elements are larger than A[i]."
  [nums i j]
  (cond
    (>= j (count nums))
    0

    (>= (get nums i) (get nums j))
    (lisbigger2 nums i (inc j))

    :else
    (let [skip (lisbigger2 nums i (inc j))
          take (+ 1 (lisbigger2 nums j (inc j)))]
      (max skip take))))

;; To find LIS starting from index 0, the number indexed at 0
;; must be the smallest in the array. Simplest approach is to use neg infinity
(defn lis
  "Find the longest increasing subsequence in the given vector nums."
  [nums]
  (if (empty? nums)
    0
    ;; Create a new array with a sentinel value at the beginning
    (let [sentinel-value ##-Inf  ;; Clojure's representation of negative infinity
          nums-with-sentinel (into [sentinel-value] nums)]
      (lisbigger2 nums-with-sentinel 0 1))))

#_(defn lis [nums]
  (if (empty? nums)
    0
    (lisbigger2 nums 0 1)))

(assert (= 4 (lis [3 1 5 2 7 4 8])))
