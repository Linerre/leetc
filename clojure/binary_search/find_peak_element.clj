(ns binary-search.find-peak-element)

;;; Leetcode 162: https://leetcode.cn/problems/find-peak-element/
;;; 1 <= nums.length <= 1000
(defn find-peak-element [nums]
  (loop [left -1
         right (dec (count nums))]
    (if (<= right (inc left))
      right
      (let [mid (quot (+ left right) 2)
            mid-ele (nth nums mid)
            mid-right (nth nums (inc mid))]
        (if (< mid-right mid-ele)
          ;; mid is new peak, update right
          (recur left mid)
          (recur mid right))))))

(assert (= 2 (find-peak-element [1,2,3,1])))
(assert (= 5 (find-peak-element [1,2,1,3,5,6,4])))
