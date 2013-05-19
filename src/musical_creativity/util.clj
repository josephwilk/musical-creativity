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

(defn position [thing list]
  (let [index (.indexOf list thing)]
    (when (>= index 0) index)))

(defn choose-one [list]
  (nth list (rand-int (count list))))

(defn last-first
  "Returns first atom of last item in the list."
  [list]
  (let [last-item (last list)]
    (if (seq? last-item)
      (first last-item)
      last-item)))

(defn fourth [list]
  (nth list 3))

(defn third [list]
  (nth list 2))
