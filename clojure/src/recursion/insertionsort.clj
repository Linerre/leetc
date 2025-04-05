(ns recursion.insertionsort)

;;; Ported here from the OCaml version by Plragde in Functional Data Structures
;;; https://cs.uwaterloo.ca/~plragde/flaneries/FDS/Tools_and_Techniques.html
(defn insert
  "Insert the given element `e` into the list `l` recursively
  to make sure the order of the elements in the list is reserved.
  Return a new list containing the given elenment."
  [e l]
  (if (empty? l)
    [e]
    (let [hd (first l)
          tl (rest l)]
      (if (< e hd)
        (into [e] l)
        (into [hd] (insert e tl))))))

(defn isort
  "Sort the given vector `l` using insertion sorting, recursively.
  Return a new sorted list where elements are placed in increasing order."
  [l]
  (if (empty? l)
    []
    (let [hd (first l)
          tl (rest l)]
      (insert hd (isort tl)))))

(assert (= [0 1 2 3 5 8 9 14] (isort [3 9 0 8 5 14 2 1])))
