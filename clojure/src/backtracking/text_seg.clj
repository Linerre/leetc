(ns backtracking.text-seg
  (:require clojure.string as cstr))

;;; Implement algorithm in chapter 2.5 of Algorithms by Jeff Erickson
;;; see: https://jeffe.cs.illinois.edu/teaching/algorithms/

;;; In the worst case, `splittable` explores each of 2^(n-1) possiblities of
;;; a string of length n. (pp. 85)
;;; A set of words considered acceptable and a function that checks if an
;;; input string is indeed an acceptable word.  Note the definition of `word`
;;; can vary a lot. In this case, a `word` is just an English word, but it
;;; can also be a palindrome, 5-letter word, noun, verb, etc.
(def words #{"HE"
             "HEAR"
             "HEART"
             "HEARTH"
             "ART"
             "HANDS"
             "HAND"
             "SATURN"
             "SPIN"
             "TURN"})

(defn word? [w] (contains? words w))

;; My buggy version (deadloop!)
#_(defn splittable
  "Return true if the argument string can be split into separate words,
  otherwise false. An empty string is split into zero words and thus splittable."
  [s]
  (println "s: " s)
  (if (empty? s)
    true
    (loop [i 1
           w (subs s 0 i)]
      (if (word? w)
        (splittable (subs s (inc i)))
        (recur (inc i) s)))))

;; Claude suggested this working version
(defn splittable [s]
  #_(println "s: " s)
  (if (empty? s)
    true
    (loop [i 1]
      (cond
        (> i (count s)) false
        (word? (subs s 0 i)) (and (splittable (subs s i)) true)
        :else (recur (inc i))))))

;;; Index formualtion
;;; or purposes of designing the algorithm, it’s incredibly useful
;;; to treat the original input array as a global variable,
;;; a global string to be used together with the gloabl set of words
(def input-string "HEARTHANDSATURNSPIN")

(defn is-word?
  "For any indices i and j , let IsWord(i, j) = T iff
  the substring A[i .. j] is a word"
  [i, j]
  (contains? words (subs input-string i j)))

(defn splittable-idx
  "For any index i , let Splittable(i) = T iff
  the sux A[i .. n] can be split into words."
  [i]
  #_(println "s: " s)
  (if (<= (count input-string) i)       ; i starts at 0
    true
    (loop [j (inc i)]                   ; j starts at 1
      (cond
        (> j (count input-string)) false
        (is-word? i j) (and (splittable-idx j) true)
        :else (recur (inc j))))))
