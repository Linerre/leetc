(ns linkedlist.single
  "Single is simple"
  (:require [clojure.string :as string]))

(deftype ListNode [value next])

(defn make-linked-list
  "Create a head-only singly linked list.  Return the head."
  [value]
  (ListNode. value nil))

(defn count-nodes [head]
  (loop [cnt 1
         cur head]
    (if (nil? (.next cur))
      cnt
      (recur (inc cnt) (.next cur)))))

;; O(1)
(defn prepend-node [current value]
  (ListNode. value current))

(defn print-list
  [head]
  (if (nil? head)
    (println "Empty list")
    (loop [cur  head
           vals [(.value cur)]]
      (if (nil? (.next cur))
        (println (string/join " -> " vals))
        (recur (.next cur) (conj vals (-> cur .next .value)))))))

;; Leetcode 206
;; 1 -> 2 -> 3 -> 4 -> 5
;; 5 -> 4 -> 3 -> 2 -> 1
(defn reverse-list [head]
  (loop [prev nil
         curn head]
    (if (nil? curn)
      prev
      (recur (prepend-node prev (.value curn)) (.next curn)))))

(comment
  (-> (ListNode. 1 (ListNode. 2 (ListNode. 3 (ListNode. 4 (ListNode. 5 nil)))))
      (reverse-list)
      (print-list)))

;; O(n)
(defn take-n-nodes-reverse
  "Take n nodes starting with head. The resulted linked list is a perfectly
  reversed one. If n is larger than the list length, equivalent to reversing
  the entire list."
  [head n]
  (if (nil? head)
    head
    (loop [lst nil
           curn head
           cnt 1]
      (if (or (nil? (.next curn)) (= cnt n))
        (prepend-node lst (.value curn))
      (recur (prepend-node lst (.value curn)) (.next curn) (inc cnt))))))

(comment
  (-> (ListNode. 1 (ListNode. 2 (ListNode. 3 (ListNode. 4 (ListNode. 5 nil)))))
      (take-n-nodes-reverse 3)
      (print-list)))

(defn take-n-nodes [head n]
  (if (nil? head)
    nil
    (-> head
      (take-n-nodes-reverse n)
      (reverse-list))))

(comment
  (-> (ListNode. 1 (ListNode. 2 (ListNode. 3 (ListNode. 4 (ListNode. 5 nil)))))
      (take-n-nodes 3)
      (print-list)))

;; O(n)
(defn prepend-list [node lst]
  (loop [n (count-nodes lst)
         head node
         curn (reverse-list lst)]
    (if (zero? n)
      head
      (recur (dec n) (prepend-node head (.value curn)) (.next curn)))))

(comment
  (->> (ListNode. 1 (ListNode. 2 (ListNode. 3 (ListNode. 4 (ListNode. 5 nil)))))
       (prepend-list (ListNode. 6 nil))
       (print-list)))

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
(defn reverse-k-group
  [head k]
  (let [l     (count-nodes head)
        m     (rem l k)                    ; number of out-of-k-group nodes
        rhead (reverse-list head)]
    (loop [rcurn  (if (zero? m) rhead (find-next rhead m)) ; skip m nodes in reverse
           group  (take-n-nodes rcurn k) ; get each group in reverse
           cnt    0                        ; count number of nodes reversed
           mnodes (if (zero? m) nil (take-n-nodes-reverse rhead m))]
      ;; (if (nil? rcurn) (println "end") (println "curn-val=" (.value rcurn)))
      ;; (print-list group)
      ;; (print-list mnodes)
      (if (or (nil? rcurn) (= l (+ m cnt)))
        mnodes
        (recur (find-next rcurn k)
               (take-n-nodes (find-next rcurn k) k)
               (+ k cnt)
               (prepend-list mnodes group))))))

(comment
  (-> (ListNode. 1 (ListNode. 2 (ListNode. 3 (ListNode. 4 (ListNode. 5 nil)))))
      (reverse-k-group 2)
      (print-list)))

(comment
  (-> (ListNode. 1 (ListNode. 2 (ListNode. 3 (ListNode. 4 (ListNode. 5 (ListNode. 6 nil))))))
      (reverse-k-group 3)
      (print-list)))
