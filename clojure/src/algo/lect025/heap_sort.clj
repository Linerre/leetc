(ns heap-sort)


(defn swap [v i1 i2]
  (assoc v i1 (v i2) i2 (v i1)))

;; The name is misleading in that it does not insert any new items.
;; Instead it adjust the subtree with root node at ith by raising this root node
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
