(ns algo.lec041.gcm)

(defn gcd [a b]
  (if (zero? b)
    a
    (gcd b (rem a b))))

(comment (gcd 20 30))


(defn lcm [a b]
  (->>
    (gcd a b)
    (/ a )
    (* b)))

(comment (lcm 50 30))
