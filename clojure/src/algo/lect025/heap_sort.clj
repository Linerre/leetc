(ns heap-sort)

(defn swap [v i1 i2]
  (assoc v i1 (v i2) i2 (v i1)))

;; The name is misleading in that it does not insert any new items.
;; Instead it adjusts the subtree with root node at ith by raising this root node
;; until its parent is larger than it or it becomes the root of the entire tree
(defn heap-insert
  "Maintain a heap structure where each substree has the largest number at top.
  Return the array that represents a large-root heap"
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
  (heap-insert [14 2 19 7 11 20 1 16 5 18 3 9 12 6 17 4 10 15 8 13] 10))

(defn heapify
  [v i size]
  (loop [i i
         heap v]
    (let [l (+ (* i 2) 1)]
      (if (< l size)
        (let [r (inc l)
              best-child (if (and (< r size) (> (heap r) (heap l))) r l)
              best (if (> (heap best-child) (heap i)) best-child i)]
          (if (= best i)
            heap
            (recur best (swap heap best i))))
        heap))))

(comment
  (heapify [14 2 19 7 11 20 1 16 5 18 3 9 12 6 17 4 10 15 8 13] 10 20))

(comment
  (heapify [5 8 7 15 16 14 17 10 11 13 3 9 12 1 6 2 4] 0 17))

(defn heap-sort-1 [nums]
  ;; heap is built up top down
  (let [heap (reduce (fn [h i]
                       (heap-insert h i))
                     nums
                     (mapv first (map-indexed vector nums)))]
    ;; It's hard to use reduce here because there are 2 states to carry
    ;; 1. an array on which numbers are added from large to small
    ;; 2. the array that represents the remaining heap
    (loop [h heap
           size (count nums)]
      ;; (println "size:" size)
      ;; (println (swap h 0 (dec size)))
      ;; (println (heapify (swap h 0 (dec size)) 0 (dec size)))
      (if (< 1 size)
        (recur (heapify (swap h 0 (dec size)) 0 (dec size)) (dec size))
        h))))

(comment
  (assert (= (heap-sort-1 [1,2,4,3,6,5]) [1,2,3,4,5,6]))
  (assert (= (heap-sort-1 [5,2,3,1]) [1,2,3,5]))
  (assert (= (heap-sort-1 [5,1,1,2,0,0]) [0,0,1,1,2,5])))

(comment
  (assert (= (heap-sort-1 [19 7 3 20 1 15 8 8]) [1 3 7 8 8 15 19 20])))

(comment
  (heap-sort-1 [14 2 19 7 11 20 1 16 5 18 3 9 12 6 17 4 10 15 8 13]))

(comment
  (heap-sort-1 [11 2 18 7 15 1 20 9 4 16 12 3 17 6 14 8 19 5 10 13]))

(defn heap-sort-2
  "Build the heap bottom up so that majority of nodes travel fewer layers.
  The general time complexity will remain O(n * logn)."
  [nums]
  (let [[heap _] (reduce (fn [[h size] i]
                           [(heapify h i size) size])
                         [nums (count nums)]
                         (reverse (mapv first (map-indexed vector nums))))]
    ;; It's hard to use reduce in the body because there are 2 states to carry
    ;; 1. an array on which numbers are added from large to small
    ;; 2. the array that represents the remaining heap
    (loop [h heap
           size (count nums)]
      (if (< 1 size)
        (recur (heapify (swap h 0 (dec size)) 0 (dec size)) (dec size))
        h))))

(comment
  (heap-sort-2 [19 7 3 12 20 1 15 8 8]))

(comment
  (heap-sort-2 [11 2 18 7 15 1 20 9 4 16 12 3 17 6 14 8 19 5 10 13]))
