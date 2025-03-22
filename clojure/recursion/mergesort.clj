(ns recursion.mergesort)

;;; Implementing according to Algorithms by Jeff Erickson
;;; https://jeffe.cs.illinois.edu/teaching/algorithms/book/01-recursion.pdf

;; `arr` is divied into two groups: [0..m] and [m+1,n] where n = len(arr)
(defn merge-arr [arr m]
  (loop [n (dec (count arr))
         k 0                           ; index for inserting element
         i 0                           ; start index of left subarray
         j (inc m)                     ; start index of right subarray
         B (vec (repeat n nil))]
    (if (< n k)
      B
      (cond
        ;; right subarray is empty, pick the smallest from sorted left subarray
        (< n j)
        (recur n (inc k) (inc i) j (assoc B k (nth arr i)))

        ;; left subarry is empty, pick the smallest from the sorted right subarry
        (< m i)
        (recur n (inc k) i (inc j) (assoc B k (nth arr j)))

        ;; pick the smallest among pos i of left and pos j of right
        (< (compare (nth arr i) (nth arr j)) 0)
        (recur n (inc k) (inc i) j (assoc B k (nth arr i)))

        ;; pick the smallest at pos j of right
        :else
        (recur n (inc k) i (inc j) (assoc B k (nth arr j)))))))

(defn merge-sort
  "Merge sort ELEMENTS in ascending order."
  [elements]
  (if (<= (count elements) 1)
    elements
    (let [mid  (quot (count elements) 2)
          lvec (merge-sort (subvec elements 0 mid))
          rvec (merge-sort (subvec elements mid))]
      (-> (into lvec rvec)
          (merge-arr (dec mid))))))
