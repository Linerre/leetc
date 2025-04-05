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
;;; The Ocaml version is provided by Plragde in Functional Data Structures
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

;;; Inspired by OCaml version from Plragde in FDS
(defn split-lst
  "Split a given vector into halves recusively
  Return a pair of the splitted halves"
  [l]
  (cond
    (empty? l)      [[] []]
    (= 1 (count l)) [l []]
    :else
    (let [a  (first l)
          b  (first (rest l))
          ls (rest (rest l))
          [f s] (split-lst ls)]
      [(into [a] f) (into [b] s)])))

(comment (split-lst [1 5 9 10 8 7]))
(comment (split-lst [1 5 9 10 8]))

;;; tail recusion version
(defn split-lst-tr
  "Split a given vector into halves recusively.
  Return a pair of the splitted halves"
  [l]
  (cond
    (empty? l)      [[] []]
    (= 1 (count l)) [l []]
    (= 2 (count l)) [[(first l)] [(last l)]]
    :else
    (loop [a  (conj [] (first l))
           b  (conj [] (first (rest l)))
           ls (rest (rest l))]
      (cond
        (empty? ls) [a b]
        (= 1 (count ls)) [(conj a (first ls)) b]
        (= 2 (count ls)) [(conj a (first ls)) (conj b (last ls))]
        :else
        (let [na  (conj a (first ls))
              nb  (conj b (first (rest ls)))
              nls (rest (rest ls))]
          (recur na nb nls))))))

(comment (split-lst-tr [1 5 9 10 8 7]))
(comment (split-lst-tr [1 5 9 10 8]))

(defn msort [l]
  (cond
    (empty? l) l
    (= 1 (count l)) l
    :else
    (let [[ls rs] (split-lst-tr l)]
      (merge-arrays (msort ls) (msort rs)))))

(comment (msort [1 5 9 10 8 7]))
(comment (msort [99 5 9 10 2]))
