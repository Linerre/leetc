(ns linkedlist.single
  "Single is simple"
  (:require [clojure.string :as string]))

;; Leetcode 206

(deftype ListNode [value next])

(defn make-linked-list
  "Create a head-only singly linked list.  Return the head."
  [value]
  (ListNode. value nil))

(defn insert-node
  "Insert a new node with the given value at the head."
  [head value]
  (ListNode. value head))

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
      (string/join " -> " vals)
      (recur (.next cur) (conj vals (-> cur .next .value))))))

;; 1 -> 2 -> 3 -> 4 -> 5
;; 5 -> 4 -> 3 -> 2 -> 1
(defn reverse-list [head]
  (loop [prev nil
         cur head]
    (if (nil? cur)
      prev
      (recur (insert-node prev (.value cur)) (.next cur)))))

(comment
  (-> (ListNode. 1 (ListNode. 2 (ListNode. 3 (ListNode. 4 (ListNode. 5 nil)))))
      reverse-list
      print-list))

(comment
  (let [head (make-linked-list 1)]
    (-> head
        (insert-node 2)
        (insert-node 3)
        (list-len))))

(comment
  (let [head (make-linked-list 3)]
    (-> head
        (insert-node 2)
        (insert-node 1)
        (print-list))))
