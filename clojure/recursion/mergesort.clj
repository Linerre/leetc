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
      (merge-arr (into lvec rvec) (dec mid)))))

(assert (= [1 2 3 5 7 8] (merge-sort [3 8 1 7 2 5])))
(assert (= [\a \b \c \d \e \f] (merge-sort [\c \e \d \a \f \b])))


;;; This version is suggested by Claude
(defn merge-arrays
  "Merge two sorted sequences into a single sorted sequence"
  [left right]
  (loop [result []
         left-remaining left
         right-remaining right]
    (cond
      (empty? left-remaining) (into result right-remaining)
      (empty? right-remaining) (into result left-remaining)

      :else
      (let [l (first left-remaining)
            r (first right-remaining)]
        (if (<= (compare l r) 0)
          (recur (conj result l) (rest left-remaining) right-remaining)
          (recur (conj result r) left-remaining (rest right-remaining)))))))

(defn merge-sort-1
  "Merge sort ELEMENTS in ascending order."
  [elements]
  (if (<= (count elements) 1)
    elements
    (let [mid (quot (count elements) 2)
          left (merge-sort (subvec elements 0 mid))
          right (merge-sort (subvec elements mid))]
      (merge-arrays left right))))

(assert (= [1 2 3 5 7 8] (merge-sort-1 [3 8 1 7 2 5])))
(assert (= [\a \b \c \d \e \f] (merge-sort-1 [\c \e \d \a \f \b])))
