(ns radix-sort
  (:require [clojure.math :as math]))

(def BASE 10)

(defn digits
  "Check the digits of `n` according to `base`. For example, in base 10,
  number 1234 has 4 digits; while in base 2, number 1023 has 10
  digits."
  ([number]
   (digits number BASE))

  ([number base]
   (loop [d 0
          n number]
     (if (pos? n)
       (recur (inc d) (quot n base))
       d))))

(comment (digits 1234))                 ;4
(comment (digits 1023 2))               ;10

(defn extract-digit
  "Extract a specific digit from a number based on current offset,
  starting with 1. Offset moves base-wise. For example, for base 10,
  offset will be 1, 10, 100, and so on."
  ([number offset]
   (extract-digit number offset BASE))

  ([number offset base]
   (rem (quot number offset) base)))

(comment (extract-digit 2r101 2 1))     ;1

(defn init-vec
  "Initiate a vector of length `n` and filled up with 0s."
  [n]
  (into [] (take n (repeat 0))))

(comment (init-vec 7))

;;; See CLRS  8.2 Counting sort
(defn counting-sort
  "Stable, used by radix-sort. `ds` is a vector of single-digit
  numbers. `n` is vector length and `k` is the number of digits of the
  max number in original vectors.  For example, in base 10, `k` = 9.
  Return a new vector where numbers in `ds` are sorted"
  [ds n k]
  (let [ ;; lines 2-9 on pp.209 in CLRS 8.2
        ;; c[i] now contains number of elements <= i
        C (->> ds
               (reduce (fn [cnt i] (update cnt i (fnil inc 0))) (init-vec k))
               (reduce (fn [cnt i] (conj cnt (+ (or (peek cnt) 0) i))) []))]
    ;; copy ds (A) to B (return value), starting from end of A, according to C
    ;; lines 10-14
    (->> ds
         (reduce-kv (fn [{:keys [B C m]} i _]
                      (let [j  (- m i)
                            aj (ds j)
                            cj (dec (C aj))]
                        {:B (assoc B cj aj) :C (update C aj dec) :m m}))
                    {:B (init-vec n) :C C :m (dec n)})
         (:B))))

(comment (counting-sort [2 5 3 0 2 3 0 3] 8 5)) ;=> [0 0 2 2 3 3 3 5]

;; offsets (mapv #(int (math/pow base %)) (take digits (range 0 base)))

;;; See CLRS 9.3 Radix sort
(defn radix-sort
  "Sort vector/array of `n` d-digit numbers using radix approach, based on
  counting sort.  In base 10, offsets look like [1,10,100,1000,...]."
  [nums n d]
  (let [offests (mapv #(long (math/pow )))
        ]))
