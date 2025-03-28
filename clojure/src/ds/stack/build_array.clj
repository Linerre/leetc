(ns ds.stack.build-array)

;;; Leetcode 1441
;;; https://leetcode.cn/problems/build-an-array-with-stack-operations/description/

;;; time: O(n)
;;; space: O(n) due to the required return type (a vector)
(defn build-array [target n]
  (->> (range 1 (inc n))                ; replace `n' with `(last target)'?
       (reduce
         (fn [ret e]
           (cond
             (some #{e} target)   (conj ret "Push")
             (>= (last target) e) (into ret ["Push" "Pop"])
             ;; default
             :else                ret))
         [])))

(assert (= ["Push" "Push" "Pop" "Push"] (build-array [1,3] 3)))

(assert (= ["Push" "Push" "Push"] (build-array [1,2,3] 3)))

(assert (= ["Push" "Push"] (build-array [1,2] 4)))

(assert (= ["Push" "Push" "Pop" "Push" "Pop" "Push"] (build-array [1,4] 6)))

(assert (= ["Push" "Push" "Push" "Pop" "Push" "Pop" "Push"] (build-array [1,2,5] 6)))
