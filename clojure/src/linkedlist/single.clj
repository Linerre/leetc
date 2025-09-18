(ns linkedlist.single
  "Single is simple"
  (:require [clojure.string :as string]))

(defprotocol ISinglyList
  (set-next [this next-node])
  (count-nodes [this]))

(deftype ListNode [value next]
  ISinglyList
  (set-next [this next-node]
    (if (identical? (.next this) next-node)
      this
      (ListNode. (.value this) next-node)))
  (count-nodes [this] (loop [cnt 1
                             cur this]
                        (if (nil? (.next cur))
                          cnt
                          (recur (inc cnt) (.next cur))))))

(comment
  (let [a (ListNode. 1 nil)
        b (ListNode. 2 nil)]
    (-> (set-next a b)
        (.next)
        (.value))))

(defn make-linked-list
  "Create a head-only singly linked list.  Return the head."
  [value]
  (ListNode. value nil))

(defn prepend-node [current value]
  (ListNode. value current))

(defn print-list
  [head]
  (loop [cur head
         vals [(.value cur)]]
    (if (nil? (.next cur))
      (println (string/join " -> " vals))
      (recur (.next cur) (conj vals (-> cur .next .value))))))

;; Leetcode 206
;; 1 -> 2 -> 3 -> 4 -> 5
;; 5 -> 4 -> 3 -> 2 -> 1
(defn reverse-list [head]
  (loop [prev nil
         cur head]
    (if (nil? cur)
      prev
      (recur (prepend-node (.value cur) prev) (.next cur)))))

(defn find-prev
  "Position is 1-indexed and is guaranteed to be <= list length.
  This is equivalent to return previous node of the given positoin.
  If the given position is 1 (head), return the dummy node."
  [dummy position]
  (loop [curn dummy
         curp 0]
    (if (= curp (dec position))
      curn
      (recur (.next curn) (inc curp)))))

(comment
  (-> (ListNode. 1 (ListNode. 2 (ListNode. 3 (ListNode. 4 (ListNode. 5 nil)))))
      (prepend-node "dummy")
      (find-prev 1)
      (.value)))

(defn find-next
  "Position is 1-indexed is guaranteed to be <= list length.
  No need for dummy in this search"
  [head position]
  (loop [curn head
         curp 1]
    (if (= curp position)
      (.next curn)
      (recur (.next curn) (inc curp)))))

(defn find-node
  [head position]
  (loop [curn head
         curp 1]
    (if (= curp position)
      curn
      (recur (.next curn) (inc curp)))))

(comment
  (-> (ListNode. 1 (ListNode. 2 (ListNode. 3 (ListNode. 4 (ListNode. 5 nil)))))
      (find-node 4)
      (.value)))

;; Leetcode 92
;; 1 -> 2 -> 3 -> 4 -> 5
;; 1 -> 5 -> 4 -> 3 -> 2 (left = 2, right = 5)
;; 1 -> 4 -> 3 -> 2 -> 5 (left = 2, right = 4)
(defn reverse-between
  "Reverse the part (1 <= left <= right) of the given list starting with head."
  [head left right]
  (let [dummy    (prepend-node head "dummy")
        nl       (find-prev dummy left)
        ;; FIXME: better to not run this search
        ;; get the rest list immediately after right
        nr       (find-next head right)]
    (loop [prev nr                 ; start prepending onto to the rest
           curn (.next nl)
           cnt  0]
      (if (= cnt (+ 1 (- right left)))
        (loop [oprev nl
               ret prev
               pos (dec left)]
          (if (= pos 0)
            ret
            (recur (find-prev dummy pos)
                   (prepend-node ret (.value oprev))
                   (dec pos))))
        (recur (prepend-node prev (.value curn)) (.next curn) (inc cnt))))))

(comment
  (-> (ListNode. 1 (ListNode. 2 (ListNode. 3 (ListNode. 4 (ListNode. 5 nil)))))
      (reverse-between 1 2)
      (print-list)))

;; Leetcode 25
#_(defn reverse-k-nodes
    "Split the linked list into len / k groups and reverse each group."
    [head k]
    (let [n (count-nodes head)
          dummy (insert-node (ListNode. -1 nil) head)]
      (loop [m n
             step k
             prev nil
             curn (.next dummy)]
        (if (< m k)
          prev
          (recur (- m k)
                 (+ k step)
                 (reverse-list-partial curn (dec step) k)
                 (-> curn .next .next))))))

#_(comment
  (-> (ListNode. 1 (ListNode. 2 (ListNode. 3 (ListNode. 4 (ListNode. 5 nil)))))
      (reverse-list-partial 1 2)
      (print-list)))

#_(comment
  (-> (ListNode. 1 (ListNode. 2 (ListNode. 3 (ListNode. 4 (ListNode. 5 nil)))))
      (reverse-k-nodes 2)
      (print-list)))
