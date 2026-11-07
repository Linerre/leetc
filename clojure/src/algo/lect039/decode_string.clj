(ns algo.lect039.decode-string
  (:require [clojure.string :as string]))

(defn parse [s i]
  (let [n (count s)]
    (loop [ii i
           cnt  0
           p (str)
           c (get s ii)]
      ;; (println c)
      (if (and (< ii n) c (not (re-matches #"\]" c)))
        (cond
          (re-matches #"[a-zA-Z]" c)
          (recur (inc ii) cnt (str p c) (get s (inc ii)))

          (re-matches #"[0-9]" c)
          (recur (inc ii) (-> cnt (* 10) (+ (parse-long (re-matches #"[0-9]" c)))) p (get s (inc ii)))

          ;; left bracket
          :else
          (let [{:keys [pos, path]} (parse s (inc ii))]
            (recur (inc pos) 0 (str p (string/join "" (repeat cnt path))) (get s (inc pos)))))
        {:pos ii, :path p}))))

(defn decode-string [s]
  (let [chars (string/split s #"")]
    (-> chars
        (parse 0)
        (:path))))

(comment
  (decode-string "3[a]")) ;=> "aaa"
(comment
  (decode-string "3[a2[c]]")) ;=> "accaccacc"
(comment
  (decode-string "2[abc]3[cd]ef")) ;=> "abcabccdcdcdef"


