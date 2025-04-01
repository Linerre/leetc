(ns backtracking.text-seg
  (:require clojure.string as cstr))

;;;
;;;
;;;

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

;; My deadloop version
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
  (if (empty? s)
    true
    (loop [i 1]
      (cond
        (> i (count s)) false
        (word? (subs s 0 i)) (and (splittable (subs s i)) true)
        :else (recur (inc i))))))
