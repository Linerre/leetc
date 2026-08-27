(ns algo.lect035.setallmap
  "Naive implementation that assumes correct input.")

(defrecord SetAllMap [map set-all-value set-all-time cnt])

(defprotocol IMap
  (mput [this k v])
  (mset-all [this v])
  (mget [this k]))

(extend-type SetAllMap
  IMap
  (mput [this k v]
    (-> this
        (assoc-in [:map k] [v (:cnt this)])
        (update this :cnt inc)))

  (mset-all [this v]
    (-> this
        (assoc :set-all-value v)
        (assoc :set-all-time (:cnt this))
        (update :cnt inc)))

  (mget [this k]
    (cond
      (nil? (get-in this [:map k] k)) -1
      (> (peek (get-in this [:map k] k)) (:set-all-time this)) (first (get-in this [:map k] k))
      :else (:set-all-value this))))


(comment
  (let [map (SetAllMap. {} 0 -1 0)]
    (-> map
        (mput 5 17)
        (mput 6 100)
        (mset-all 9)
        (mget 5))))
