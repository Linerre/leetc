(ns algo.lect041.gcm)

(defn gcd [a b]
  (if (zero? b)
    a
    (gcd b (rem a b))))

(defn gcd2 [a b]
  (cond
    (< a b) (recur b (- b a))
    (> a b) (recur (- a b) b)
    :else a))

(comment (gcd 20 30))
(comment (gcd 5 6))
(comment (gcd2 5 6))
(comment (gcd2 20 30))

(defn lcm [a b]
  (->>
    (gcd a b)
    (/ a )
    (* b)))

(comment (lcm 50 30))
