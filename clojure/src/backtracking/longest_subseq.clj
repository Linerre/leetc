(ns backtracking.longest-subseq)

;;; Implementation of the LISBigger in chapter 2.6 of Algorithms by Jeff Eirckson
;;; see: https://jeffe.cs.illinois.edu/teaching/algorithms/book/02-backtracking.pdf
;;; It is hard for this algorithm to be tail recursive without an auxilary function
;;; The below version is not tail optiomized recursion
(defn lisbigger
  "Find the longest increasing subsequence in the ginven vector `nums`.
  `prev` is the starting point and must be the number at index 0.
  Return the length of the such a sequence."
  [prev nums]
  (let [next (first nums)
        rm (rest nums)]
    (cond
      (empty? nums) 0
      (nil? next) 1
      (empty? rm) (if (< prev next) 2 1)
      (<= next prev) (lisbigger prev rm)
      :else
      (let [skip (lisbigger prev rm)
            take (lisbigger next rm)]
        (max skip (inc take))))))

(assert (= 2 (lisbigger 4 [4 3 1 9])))
(assert (= 4 (lisbigger 3 [3, 1, 5, 2, 7, 4, 8])))
(assert (= 5 (lisbigger 1 [1 2 3 4 5])))

;;; Suggested by Claude which is buggy
#_(defn lisbigger
  "Find the longest increasing subsequence where all elements are larger than A[i]."
  [nums i j]
  (cond
    (>= j (count nums))
    0

    (>= (get nums i) (get nums j))
    (lisbigger nums i (inc j))

    :else
    (let [skip (lisbigger nums i (inc j))
          take (+ 1 (lisbigger nums j (inc j)))]
      (max skip take))))

;; To find LIS starting from index 0
#_(defn lis [nums]
  (if (empty? nums)
    0
    (lisbigger nums 0 1)))

#_(comment (lis [3 1 5 2 7 4 8]))
