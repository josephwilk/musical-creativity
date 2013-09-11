(ns musical-creativity.experiments.analogy)

(def *database* (atom ()))

(defn llast [thing]
  (last (last thing)))

(defn reverse-the-database
  "reverses each element of the database."
  [database]
  (map #(reverse %) database))

(defn lookup-data [pointer database]
  "associates pointer with database."
  (first (filter #(= pointer (first %)) database)))

(defn lookup
  "the basic lookup function - returns only what the pointer points towards."
  [pointer]
  (first (filter #(= pointer (first %)) @*database*)))

(defn infer-for-analogy
  "infers pointer with each successive pointer."
  [pointer database]
  (let [test (lookup-data pointer database)]
    (if (empty? test)
      ()
      (cons test (infer-for-analogy (last test) database)))))

(defn remove-all [lists other-lists]
  "removes all of the first arg from the second arg."
  (if (empty? lists)
    other-lists
    (remove-all (rest lists)
                (remove #(= (first lists) %)  other-lists))))

(defn choose-one [list]
  "chooses one its arg randomly."
  (when-not (empty? list)
    (nth list (rand (count list)))))

(defn infer
  "returns all of the pointers of what's pointed towards - implicit information."
  [pointer]
  (let [test (lookup pointer)]
    (if (empty? test)
      ()
      (concat test (rest (infer (last test)))))))

(defn add-to-database [statement]
  "adds statement to the database as explicit fact."
  (reset! *database* (concat statement @*database*)))

(defn reset-the-database
  "resets the database to nil."
  []
  (reset! *database* ()))

(defn derive-all-logical-predecessors
  "connects the various pointers for analogy."
  ([end-pointer] (derive-all-logical-predecessors end-pointer (reverse-the-database @*database*)))
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
(reset! *database* '((cat*mouse are strong*weak)
                     (strong*weak are opposite*sides)
                     (oil*water are non*mixable)
                     (non*mixable are opposite*sides)))

;;(reset! *database* '((g-b-d*c-e-g are dominant*tonic) (dominant*tonic are resolutions) (f-a-c*c-e-g are subdominant*tonic) (subdominant*tonic are resolutions)))

;;(analogy 'g-b-d*c-e-g)
;;-> (g-b-d*c-e-g is like f-a-c*c-e-g)

