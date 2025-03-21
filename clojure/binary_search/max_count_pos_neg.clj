(ns binary-search.max-count-pos-neg)

;;; Leetcode 2529: https://leetcode.com/problems/maximum-count-of-positive-integer-and-negative-integer
;;; Simple
;;; 1 <= nums.length <= 1000

(defn bisec-left-1
  "Find the first number >= TARGET in an non-decreasing array NUMS
  via binary search."
  [target nums]
  (loop [left 0
         right (count nums)]
    (if (>= left right)
      left
      (let [mid (quot (+ right left) 2)
            mid-ele (nth nums mid)]
        (if (< mid-ele target)
          (recur (inc mid) right)
          (recur left mid))))))

(defn bisec-left-2
  "Find the first number >= TARGET in an non-decreasing array NUMS
  via binary search."
  [target nums]
  (loop [left -1
         right (count nums)]
    (if (>= left (dec right))
      right
      (let [mid (quot (+ right left) 2)
            mid-ele (nth nums mid)]
        (if (< mid-ele target)
          (recur mid right)
          (recur left mid))))))

(defn bisec-left
  "Find the first number >= TARGET in an non-decreasing array NUMS
  via binary search."
  [target nums]
  (loop [lo 0
         hi (count nums)]
    (if (< lo hi)
      (let [mid (quot (+ lo hi) 2)]
        (if (< (nth nums mid) target)
          (recur (inc mid) hi)
          (recur lo mid)))
      lo)))

(defn bisec-right
  "Find the first index where an element > TARGET would be inserted
   in an non-decreasing array NUMS via binary search."
  [target nums]
  (loop [lo 0
         hi (count nums)]
    (if (< lo  hi)
      (let [mid (quot (+ lo hi) 2)]
        (if (<= (nth nums mid) target)
          (recur (inc mid) hi)
          (recur lo mid)))
      lo)))

(defn maximum-count [nums]
  (let [neg (bisec-left 0 nums)         ; number of negs: [neg 0* pos]
        pos (- (count nums) (bisec-right 0 nums))]
    (max neg pos)))

(assert (= 3 (maximum-count [-2,-1,-1,1,2,3])))
(assert (= 3 (maximum-count [-3,-2,-1,0,0,1,2])))
(assert (= 4 (maximum-count [5,20,66,1314])))
