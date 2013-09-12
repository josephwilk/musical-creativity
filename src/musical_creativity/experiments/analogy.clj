(ns musical-creativity.experiments.analogy
  (:require
   [musical-creativity.util :refer :all]))

(def fact-store (atom ()))

(defn llast [thing]
  (last (last thing)))

(defn reverse-the-database [database]
  (map #(reverse %) database))

(defn lookup-data [pointer database]
  (first (filter #(= pointer (first %)) database)))

(defn lookup [pointer]
  (first (filter #(= pointer (first %)) @fact-store)))

(defn infer-for-analogy
  "infers pointer with each successive pointer."
  [pointer database]
  (let [test (lookup-data pointer database)]
    (if (empty? test)
      ()
      (cons test (infer-for-analogy (last test) database)))))

(defn remove-all [lists other-lists]
  (if (empty? lists)
    other-lists
    (remove-all (rest lists)
                (remove #(= (first lists) %)  other-lists))))

(defn infer
  "returns all of the pointers of what's pointed towards - implicit information."
  [pointer]
  (let [test (lookup pointer)]
    (if (empty? test)
      ()
      (concat test (rest (infer (last test)))))))

(defn add-to-database [statement]
  (reset! fact-store (concat statement @fact-store)))

(defn reset-the-database [] (reset! fact-store ()))

(defn derive-all-logical-predecessors
  "connects the various pointers for analogy."
  ([end-pointer] (derive-all-logical-predecessors end-pointer (reverse-the-database @fact-store)))
  ([end-pointer database]
     (let [test (infer-for-analogy end-pointer database)]
       (if (empty? test)
         ()
         (concat [(llast test)]
                 (derive-all-logical-predecessors end-pointer (remove-all test database)))))))

(defn analogy [pointer]
  "toplevel function for analogy - pretties up the results."
  (let [test (infer pointer)
        predecessors (derive-all-logical-predecessors (last test))
        choice (choose-one (remove #(= pointer %) predecessors))]
    (concat (list pointer) '(is like) (list choice))))

;;Example:
(reset! fact-store '((cat*mouse are strong*weak)
                     (strong*weak are opposite*sides)
                     (oil*water are non*mixable)
                     (non*mixable are opposite*sides)))
(analogy 'cat*mouse)
;;-> '(cat*mouse is like oil*water

(reset! fact-store '((g-b-d*c-e-g are dominant*tonic) (dominant*tonic are resolutions) (f-a-c*c-e-g are subdominant*tonic) (subdominant*tonic are resolutions)))
(analogy 'g-b-d*c-e-g)
;;-> (g-b-d*c-e-g is like f-a-c*c-e-g)
