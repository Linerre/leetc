(ns dp.fibonacci)

;; O(n^2): top to bottom
(defn fib1 [n]
  (if (<= n 1)
    n
    (+ (fib1 (- n 1)) (fib1 (- n 2)))))

(comment (assert (= 233 (fib1 13))))


;; O(n^2): top to bottom
(defn fib2 [n]
  (if (<= n 1)
    n
    (loop [dp (vec (repeat (inc n) nil))
           m n]
      (if (< m 2)
        (peek dp)
        (recur (assoc dp m (+ (fib2 (- m 1)) (fib2 (- m 2))))
               (dec m))))))

(comment (fib2 18))

;; O(n): bottom to top
(defn fib3 [n]
  (if (<= n 1)
    n
    (loop [dp [0 1]
           i 2]
      (if (> i n)
        (peek dp)
        (recur (conj dp (+ (dp (- i 1)) (dp (- i 2))))
               (inc i))))))

(comment (assert (= 34 (fib3 9))))

;; O(n): bottom to top
(defn fib4 [n]
  (if (<= n 1)
    n
    (loop [m   2
           a   0
           b   1
           ret (+ a b)]
      (if (= m n)
        ret
        (recur (inc m) b (+ a b) (+ ret b))))))

(comment (fib4 12))
