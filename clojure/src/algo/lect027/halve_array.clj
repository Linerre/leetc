(ns algo.lect027.halve-array)

(defn halve-array [v]
  (let [v20 (mapv #(bit-shift-left % 20) v)
        goal (/ (reduce + v20) 2)]
    (loop [vv v20
           cnt 0
           amt 0]
      (println "amt=" amt "cnt=" cnt "goal=" goal)
      (if (< amt goal)
        (let [m (apply max vv)
              i (.indexOf vv m)
              top (/ m 2)]
          (recur (assoc vv i top) (inc cnt) (+ amt top)))
        cnt))))

(comment
  (halve-array [6,58,10,84,35,8,22,64,1,78,86,71,77]))
;; => 9

(comment
  (halve-array [32,98,23,14,67,40,26,9,96,96,91,76,4,40,42,2,31,13,16,37,62,2,27,25,100,94,14,3,48,56,64,59,33,10,74,47,73,72,89,69,15,79,22,18,53,62,20,9,76,64]))
;; => 36


(defn hconj
  "A more restricted heapify because of recursion and unncessary comparison."
  [v n]
  (cond
    (empty? v) [n]
    (< (first v) n) (into [n] v)
    :else (into [(first v)] (hconj (rest v) n))))

(comment
  (reduce hconj [] [8 1 3 2 4 7 5 6]))

(comment
  (reduce hconj [] [6,58,10,84,35,8,22,64,1,78,86,71,77]))
