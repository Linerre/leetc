(ns radix-sort
  (:require [clojure.math :as math]))

(defn digits
  "Check the digits of `n` according to `base`. For example, in base 10,
  number 1234 has 4 digits; while in base 2, number 1023 has 10
  digits."
  [number base]
  (loop [d 0
         n number]
    (if (pos? n)
      (recur (inc d) (quot n base))
      d)))

(comment (digits 1234 10))              ;4
(comment (digits 1023 2))               ;10

(defn extract-digit
  "Extract a specific digit from a number based on current offset,
  starting with 1. Offset moves base-wise. For example, for base 10,
  offset will be 1, 10, 100, and so on."
  [number base offset]
  (rem (quot number offset) base))

(comment (extract-digit 2r101 2 1))     ;1

(defn init-vec
  "Initiate a vector of length `n` and filled up with 0s."
  [n]
  (into [] (take n (repeat 0))))

(comment (init-vec 7))

;;; See CLRS  8.2 Counting sort
(defn counting-sort
  "Stable, used by radix-sort. `nums` i a vector of single-digit
  numbers. `n` is vector length and `k` is the number of digits of the
  max number in original vectors.  For example, in base 10, `k` = 9.
  Return a new vector where numbers in `nums` are sorted"
  [nums n k]
  (let [ ;; lines 2-8 on pp.209 in CLRS 8.2
        ;; c[i] now contains number of elements <= i
        C (->> nums
               (reduce (fn [cnt i] (update cnt i (fnil inc 0))) (init-vec k))
               (reduce (fn [cnt i] (conj cnt (+ (or (peek cnt) 0) i))) []))]
    ;; copy nums (A) to B (return value), starting from end of A, according to C
    (->> nums
         (reduce-kv (fn [{:keys [B C]} i _]
                      (let [m  (dec (count B))
                            j  (- m i)
                            aj (nums j)
                            cj (dec (C aj))]
                        {:B (assoc B cj aj) :C (update C aj dec)}))
                    {:B (init-vec n) :C C})
         (:B))))

(comment (counting-sort [2 5 3 0 2 3 0 3] 8 5)) ;=> [0 0 2 2 3 3 3 5]

(defn radix-sort
  "Sort vector/array nums using radix approach. `digits` comes from the
  max number in nums. In base 10, offsets look like [1, 10, 100, 1000, ...]."
  [nums digits base]
  (let [d (digits (apply max nums) base)
        offsets (mapv #(int (math/pow base %)) (take digits (range 0 base)))]))
