(ns bit-ops)

(def MIN (int -2147483648))
(def MAX (int 2147483647))

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
