(ns heap-sort)

(defn swap [v i1 i2]
  (assoc v i1 (v i2) i2 (v i1)))

;; The name is misleading in that it does not insert any new items.
;; Instead it adjusts the subtree with root node at ith by raising this root node
;; until its parent is larger than it or it becomes the root of the entire tree
(defn heap-insert
  "Maintain a heap structure where each substree has the largest number at top.
  Return the array that represents a larget-root heap"
  [nums i]
  (loop [heap nums
         ii i]
    (let [pi (quot (- ii 1) 2)
          p (heap pi)
          c (heap ii)]
      (if (< p c)
        (recur (swap heap ii pi) (quot (- ii 1) 2))
        heap))))

(comment
  (heap-insert [1 2 3 4 6] 4))


(defn heapify
  "Push down item at i as deep as possible so that each subtree maintains a
 large-root heap structure."
  [v i size]
  (loop [li (+ (* i 2) 1)
         heap v]
    (if (< li size)                     ; has left child
      (let [c (heap i)
            l (heap li)
            r (if (< (inc li) size) (heap (inc li)) nil) ; get right child
            best (if r (max c l r) (max c l))
            besti (cond (= best r) (inc li) (= best l) li :else i)]
        (if (= besti i)
          heap
          (recur (+ (* besti 2) 1) (swap heap besti i))))
      heap)))

(comment
  (assert (= (heapify [1,2,4,3,6,5] 2 6) [1, 2, 5, 3, 6, 4])))


(defn heap-sort-1 [nums]
  (let [heap (reduce (fn [h i] (heap-insert h i))
                     nums
                     (mapv first (map-indexed vector nums)))]
    ;; It's hard to use reduce here because there are 2 states to carry
    ;; 1. an array on which numbers are added from large to small
    ;; 2. the array that represents the remaining heap
    (loop [h heap
           size (count nums)]
      (if (< 1 size)
        (recur (heapify (swap h 0 (dec size)) 0 (dec size)) (dec size))
        h))))

(comment
  (assert (= (heap-sort-1 [1,2,4,3,6,5]) [1,2,3,4,5,6]))
  (assert (= (heap-sort-1 [5,2,3,1]) [1,2,3,5]))
  (assert (= (heap-sort-1 [5,1,1,2,0,0]) [0,0,1,1,2,5])))

(comment
  (assert (= (heap-sort-1 [19 7 3 20 1 15 8 8]) [1 3 7 8 8 15 19 20])))
