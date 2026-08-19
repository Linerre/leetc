(ns bit-ops)

(def MIN (int -2147483648))
(def MAX (int 2147483647))

(defn add [a b]

  (loop [sum 0
         a' (int a)
         b' (int b)]
    (if (not= 0 b')
      (recur (bit-xor a' b') (bit-xor a' b') (-> b' (bit-and a') (bit-shift-left 1)))
      sum)))

(comment (add 230 3))

(defn neg [a]
  (-> a
    (int)
    (bit-not )
    (add (int 1))))

(comment (neg 10))

(defn sub [a b]
  (add (int a) (neg b)))

(comment (sub 10 20))
