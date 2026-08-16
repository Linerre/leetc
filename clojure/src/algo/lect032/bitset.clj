(ns bitset)

(defrecord Bitset
    [^longs bits
     ^long size
     ^long zeros
     ^long ones
     ^boolean reverse])

(defn- init-bits [n]
  (into [] (take (quot (+ n 63) 64) (repeat 0))))

(defn- init-bitset [n]
  (map->Bitset {:bits (init-bits n) :size n :zeros n :ones 0 :reverse false}))


;; // void fix(int i) : 将下标i的位上的值更新为1
;; // void unfix(int i) : 将下标i的位上的值更新为0
;; // void flip() : 翻转所有位的值
;; // boolean all() : 是否所有位都是1
;; // boolean one() : 是否至少有一位是1
;; // int count() : 返回所有位中1的数量
;; // String toString() : 返回所有位的状态
(defprotocol IBitset
  (fix [this i])
  (unfix [this i])
  (flip [this])
  (all ^boolean [this])
  (one ^boolean [this])
  (count-ones ^long [this])
  (to-string ^String [this]))







(comment (init-bitset 5))
