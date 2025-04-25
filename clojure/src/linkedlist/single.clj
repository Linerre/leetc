(ns linkedlist.single
  "Single is simple"
  (:require [clojure.string :as string]))

(defprotocol ISinglyList
  (set-next [this next-node])
  (count-nodes [this]))

(deftype ListNode [value next]
  ISinglyList
  (set-next [this next-node] (ListNode. (.value this) next-node))
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

(defn insert-node
  "Insert a new node with the given value at the node.
  This is equivalent to making the new node's next point to node.
  If the given node is not head, the inserted new node becomes the new head."
  [current new-node]
  (ListNode. (.value current) new-node))

(defn list-len
  "Always assume the list contains at least one node."
  [head]
  (loop [cnt 1
         cur head]
    (if (nil? (.next cur))
      cnt
      (recur (inc cnt) (.next cur)))))

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
      (recur (insert-node cur prev) (.next cur)))))

(comment
  (-> (ListNode. 1 (ListNode. 2 (ListNode. 3 (ListNode. 4 (ListNode. 5 nil)))))
      reverse-list
      print-list))

(defn find-prev
  "Position is 1-indexed and is guaranteed to be <= list length."
  [head position]
  (loop [curn head
         curp (dec position)]
    (if (= 0 curp)
      curn
      (recur (.next curn) (dec curp)))))

(comment
  (-> (ListNode. 1 nil)
      (insert-node (ListNode. 2 (ListNode. 3 (ListNode. 4 (ListNode. 5 nil)))))
      (find-prev 4)
      (.value)))

(defn find-next
  "position is guaranteed to be <= list length.
  If position points to the last node, return last node intead."
  [head position]
  (loop [curn head
         curp 1]
    (if (= curp position)
      (.next curn)
      (recur (.next curn) (inc curp)))))

(comment
  (-> (ListNode. 1 nil)
      (insert-node (ListNode. 2 (ListNode. 3 (ListNode. 4 (ListNode. 5 nil)))))
      (find-next 5)))

;; Leetcode 92
;; 1 -> 2 -> 3 -> 4 -> 5
;; 1 -> 5 -> 4 -> 3 -> 2 (left = 2, right = 5)
;; 1 -> 4 -> 3 -> 2 -> 5 (left = 2, right = 4)
(defn reverse-list-partial
  "Reverse the part (1 <= left <= right) of the given list starting with head."
  [head left right]
  (let [dummy    (insert-node (ListNode. -1 nil) head)
        nl       (find-prev dummy left)
        ;; FIXME: better to not run this search
        nr       (find-next head right)]
    (loop [prev nr
           curn (.next nl)
           cnt  0]
      (if (= cnt (+ 1 (- right left)))
        (cond-> nl
          (< 1 left)
          (insert-node prev))
        (recur (insert-node curn prev) (.next curn) (inc cnt))))))

(comment
  (-> (ListNode. 1 (ListNode. 2 (ListNode. 3 (ListNode. 4 (ListNode. 5 nil)))))
      (reverse-list-partial 2 4)
      (print-list)))
