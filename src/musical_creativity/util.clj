(ns musical-creativity.util)

(defn filtermap
  "Filter a list while also mapping to a new value if that value evaluates to not false.
  equivalent to (map fn1 (filter fn2 list)) but not lazy"
  [predicate list]
  (reduce (fn [new-list item]
            (let [result (predicate item)]
              (if result
                (conj new-list result)
                new-list))) [] list))

(defn member
  "returns tail of list beginning with value otherwise nil"
  [value list]
  (if (seq list)
    (if (= value (first list))
      list
      (recur value (rest list)))))

(defn position [thing list]
  "returns position of thing in list or nil"
  (let [index (.indexOf list thing)]
    (when (>= index 0) index)))

(defn choose-one [list]
  "randomly pick a value from the list"
  (when-not (empty? list)
    (nth list (rand-int (count list)))))

(defn last-first
  "First item of last element in list (first (last list))"
  [list]
  (let [last-item (last list)]
    (if (seq? last-item)
      (first last-item)
      last-item)))

(defn frequency [item list] (count (filter #(= % item) list)))

(defn fourth [list]
  (nth list 3))

(defn third [list]
  (nth list 2))
