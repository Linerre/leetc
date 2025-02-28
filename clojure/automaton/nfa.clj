(ns automaton.nfa
  (:require [clojure.set :refer [union intersection]]))

(defn trans
  "Transition function that takes a state in Q and an input symbol in S
  as arguments and returns a subset of Q.  This is the extended transition
  function which accepts epsilon (empty) input"
  ;; same as that when input is an empty
  ([state] #{state})

  ([state input]
   (if (empty? input)
     #{state}
     (case [state input]
       [:q0 "0"] #{:q0 :q1}
       [:q0 "1"] #{:q0}
       [:q1 "0"] #{}
       [:q1 "1"] #{:q2}
       [:q2 "0"] #{}
       [:q2 "1"] #{}))))

;;; NFA = (Q,S,d,qs,A)
(def nfa1
  {:name "string-ends-with-01"
   :Q    #{:q0 :q1 :q2},
   :S    #{"0", "1"},
   :d    trans,
   :qs   :q0,
   :A    #{:q2}})

(assert (= #{:q2} (apply (:d nfa1) [:q1 "1"])))
(assert (= #{:q0} (apply (:d nfa1) [:q0])))

;;; Extended transition function for the above NFA.
;;; The algorithm is described in chapter 2.3.3 (Hopcroft, Motwani, et al.)
(defn process-input
  "Start with the given `state' to process a string input
  that consists of only `0' and `1' and has an arbitrary length.

  After processing the entire input, Return the last set of states,
  which is the set of states that the NFA will be in."
  [state input]
  (if (empty? input)
    (apply (:d nfa1) [:q0])
    (reduce (fn [states char]
              (reduce (fn [acc s]
                        (union acc (apply (:d nfa1) [s (str char)])))
                      #{}
                      states))
            ;; using set of states is necessary cuz the inner fn
            ;; loops through the set of states got from previous
            ;; computation of d(p,c)
            #{state}
            input)))

(assert (= #{:q0} (process-input :q0 "")))
(assert (= #{:q0 :q1} (process-input :q0 "00")))
(assert (= #{:q0 :q2} (process-input :q0 "00101")))

;;; Accept if ture and reject otherwise
(defn accept? [state input]
  (let [halt-states (process-input state input)]
    (not= #{} (intersection (:A nfa1) halt-states))))

(assert (not (accept? :q0 "00111")))
(assert (accept? :q0 "00101"))
