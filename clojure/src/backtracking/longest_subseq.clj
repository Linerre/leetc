(ns backtracking.longest-subseq)

(defn lisbigger [nums]
  (loop [ret []
         prev (first nums)
         next (first (rest nums))
         rm (rest (rest nums))]
    (println prev)
    (println next)
    (println ret)

    (cond
      (empty? nums) ret
      (nil? next) (conj ret prev)
      (empty? rm) (if (< prev next) (conj ret next) (conj ret prev))
      (< next prev) (recur ret prev (first rm) (rest rm))
      :else                             ; prev <= next
      (let [nnext (first rm)]
        (cond
          (empty? (rest rm))
          (if (< next nnext) (conj ret prev next nnext) (conj ret prev next))

          (< next nnext) (recur (conj ret prev next nnext) next nnext (rest rm))

          :else
          (recur (conj ret prev next) next (first (rest rm)) (rest rm)))))))

(comment (lisbigger []))
(comment (lisbigger [1]))
(comment (lisbigger [1 2]))
(comment (lisbigger [4 3 1 9]))
;; prev 4 > next 3
(comment (lisbigger [4 3 1 9 8 6 7 5 9]))
(comment (lisbigger [4 3 1 10 8 9 6 7 5 9]))
