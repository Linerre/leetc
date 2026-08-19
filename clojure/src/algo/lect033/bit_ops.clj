(ns algo.lect033.bit-ops)

(defn add [a b]
  (loop [sum 0
         a'  (int a)
         b'  (int b)]
    (if (not= 0 b')
      (recur (bit-xor a' b') (bit-xor a' b') (-> b' (bit-and a') (bit-shift-left 1)))
      sum)))

(comment (add 230 3))

(defn neg [a]
  (-> a
    (int)
    (bit-not)
    (add (int 1))))

(comment (neg 10))

(defn sub [a b]
  (add (int a) (neg b)))

(comment (sub 10 20))

(defn div [a b]
  (loop [x (if (< a 0) (neg a) (int a))
         y (if (< b 0) (neg b) (int b))
         quot 0
         i 30]
    (if (>= i 0)
      (if (>= (bit-shift-right x i) y)
        (recur (sub x (bit-shift-left y i)) y (bit-or quot (bit-shift-left 1 i)) (dec i))
        (recur x y quot (dec i)))
      (if (or (< a 0 b) (< b 0 a))
        (neg quot)
        (int quot)))))

(comment (div 1024 2))

(def MIN (int -2147483648))
(def MAX (int 2147483647))

(defn divide
  "Implement division without using arithmatic operations. The number
  range is strictly limited within [-2^31, 2^31-1] (inclusive)."
  [dividend divisor]
  (let [a (int dividend)
        b (int divisor)]
    (cond
      (and (= a MIN) (= b MIN)) 1
      (and (not= a MIN) (not= b MIN)) (div a b)
      (= b MIN) 0
      (= b (neg 1)) MAX
      ;; (and (= a MIN), (not= b MIN), (not= b (neg 1)))
      :else
      (let [a' (add a (if (< 0 b) b (neg b)))
            q (div a' b)
            offset (if (< 0 b) (neg 1) (int 1))]
        (add q offset)))))

(comment (divide MIN MIN))
(comment (divide MIN MAX))
(comment (divide 321 MIN))
(comment (divide MAX (int -1)))
(comment (divide 333 111))
