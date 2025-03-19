(ns binary-search.max-count-pos-neg)

;;; Leetcode 2529: https://leetcode.com/problems/maximum-count-of-positive-integer-and-negative-integer
;;; Simple
;;; 1 <= nums.length <= 1000

;;; FIXME:
(defn maximum-count [nums]
  (loop [left -1
         right (dec (count nums))
         neg 0
         pos 0]
    (if (<= right (inc left))
      (max neg pos)
      (let [mid (quot (+ left right) 2)
            mid-ele (nth nums mid)]
        (println "mid: " mid)
        (println "mide: " mid-ele)
        (cond
          (<= 0 mid-ele)
          (recur mid right mid 0)

          (< 0 mid-ele)
          (recur left mid neg (- right mid))

          ;; default mid-ele < 0
          :else
          (recur left (inc mid) 0 0))))))
