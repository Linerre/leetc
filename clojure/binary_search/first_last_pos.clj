(ns binary-search.first-last-pos)

;;; Leetcode 34: https://leetcode.cn/problems/find-first-and-last-position-of-element-in-sorted-array/description/

(defn lower-bound [nums target]
  (loop [left 0
         right (dec (count nums))]
    (if (< right left)
      left
      (let [mid (quot (+ left right) 2)]
        (if (< (nth nums mid) target)
          (recur (inc mid) right)
          (recur left (dec mid)))))))


(defn solution [nums target]
  (let [start (lower-bound nums target)]
    (if (or (= start (count nums))
            (not= target (get nums start)))
      ;; element does not exist
      [-1, -1]
      (let [end (lower-bound nums (inc target))]
        [start (dec end)]))))

(assert (= [3,4] (solution [5,7,7,8,8,10] 8)))
