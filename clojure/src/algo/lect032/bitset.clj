(ns bitset
  (:require [clojure.string :as string]))

(defrecord Bitset [bits size zeros ones reverse])

(defn- init-bits [n]
  (into [] (take (quot (+ n 63) 64) (repeat 0))))

(defn- init-bitset [n]
  (map->Bitset {:bits (init-bits n) :size n :zeros n :ones 0 :reverse false}))


(defprotocol IBitset
  (fix [this i])
  (unfix [this i])
  (flip [this])
  (all [this])
  (one [this])
  (count-ones ^Long [this])
  (to-string [this]))

(extend-type Bitset
  IBitset
  (fix [this i]
    (let [{:keys [ones zeros bits reverse]} this
          slot (quot i 64)
          bit (rem i 64)
          ts  (get bits slot)           ; target slot number
          mask (bit-shift-left 1 bit)
          nm (if (not reverse)          ; new bitmap at slot
               (when (= 0 (bit-and ts mask)) (bit-set ts bit))
               (when (not= 0 (bit-and ts mask)) (bit-clear ts bit)))]
      (assoc this
             :ones (inc ones)
             :zeros (dec zeros)
             :bits (assoc bits slot nm))))
  (unfix [this i]
    (let [{:keys [ones zeros bits reverse]} this
          slot (quot i 64)
          bit (rem i 64)
          ts  (get bits slot)           ; target slot number
          mask (bit-shift-left 1 bit)
          nm (if (not reverse)          ; new bitmap at slot
               (when (not= 0 (bit-and ts mask)) (bit-clear ts bit))
               (when (= 0 (bit-and ts mask)) (bit-set ts bit)))]
      (assoc this
             :ones (dec ones)
             :zeros (inc zeros)
             :bits (assoc bits slot nm))))
  (flip [this]
    (assoc this :zeros (:ones this) :ones (:zeros this) :reverse (not (:reverse this))))
  (all [this]
    (= (:ones this) (:size this)))
  (one [this]
    (< 0 (:ones this)))
  (count-ones ^Long [this]
    (:ones this))
  (to-string [this]
    (->> (:bits this)
         (reduce (fn [{:keys [bits checked] :as state} b]
                   (let [rflag (:reverse this)
                         size  (:size this)]
                     (loop [j     0
                            cnt   checked
                            bits1 bits]
                       (let [status (bit-xor (bit-and (bit-shift-right b j) 1) (if rflag 1 0))]
                         (if (and (< j 64) (< cnt size))
                           (recur (inc j) (inc cnt) (conj bits1 status))
                           (assoc state :bits bits1 :checked cnt))))))
                 {:bits [] :checked 0})
         (:bits)
         (string/join))))


(comment
  (let [bitset (init-bitset 5)]
    (-> bitset
        (fix 3)
        (fix 1)
        (flip)
        (unfix 0)
        (flip)
        (unfix 0)
        (to-string))))

(comment
  (let [bitset (init-bitset 5)]
    (-> bitset
        (fix 3)
        (fix 1)
        (flip)
        (unfix 0)
        (flip)
        (unfix 0)
        (count-ones))))
