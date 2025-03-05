(ns binary-search.first-last-pos)

;;; Leetcode 34: https://leetcode.cn/problems/find-first-and-last-position-of-element-in-sorted-array/description/

;; [left, right]
(defn lower-bound [nums target]
  (loop [left 0
         right (dec (count nums))]
    (if (< right left)
      left
      (let [mid (quot (+ left right) 2)]
        (if (< (nth nums mid) target)
          (recur (inc mid) right)
          (recur left (dec mid)))))))


(defn solution1 [nums target]
  (let [start (lower-bound nums target)]
    (if (or (= start (count nums))
            (not= target (get nums start)))
      ;; element does not exist
      [-1, -1]
      (let [end (dec (lower-bound nums (inc target)))]
        [start end]))))

(assert (= [3,4] (solution1 [5,7,7,8,8,10] 8)))
(assert (= [-1,-1] (solution1 [] 8)))

;; [left, right)
(defn lower-bound-2 [nums target]
  (loop [left 0
         right (count nums)]
    (if (<= right left)
      left
      (let [mid (quot (+ left right) 2)]
        (if (<= target (nth nums mid))
          (recur left mid)
          (recur (inc mid) right))))))

;; TODO: (left, right)

(defn solution2 [nums target]
  (let [start (lower-bound-2 nums target)]
    (if (or (= start (count nums))
            (not= target (get nums start)))
      ;; element does not exist
      [-1, -1]
      (let [end (dec (lower-bound-2 nums (inc target)))]
        [start end]))))

(assert (= [3,4] (solution2 [5,7,7,8,8,10] 8)))
(assert (= [-1,-1] (solution2 [] 8)))
