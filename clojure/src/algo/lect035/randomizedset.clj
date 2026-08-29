(ns algo.lect035.randomizedset
  (:require [clojure.math :as math]))

(def randomized-set (atom {:rmap {}, :rarr []}))

(defn rs-insert! [val]
  (let [{:keys [rmap rarr]} @randomized-set
        index               (count rarr)]
    (if (get rmap val)
      false
      (do
        (swap! randomized-set assoc :rmap (assoc rmap val index))
        (swap! randomized-set assoc :rarr (conj rarr val))
        true))))

(defn rs-remove! [val]
  (let [{:keys [rmap rarr]} @randomized-set
        val-index (get rmap val)
        last-val (peek rarr)]
    (if (nil? val-index)
      false
      (do
        (swap! randomized-set assoc :rmap (dissoc rmap val))
        (swap! randomized-set assoc :rarr (-> rarr (assoc val-index last-val) (pop)))
        true))))

(defn rs-get-random! []
  (let [{:keys [rarr]} @randomized-set
        random-index (long (* (math/random) (count rarr)))]
    (get rarr random-index)))

(comment
  (do
    (rs-insert! 1)
    (rs-insert! 1)
    (rs-insert! 2)
    (rs-remove! 1)
    (rs-get-random!)))
