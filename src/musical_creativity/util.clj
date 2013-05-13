(ns musical-creativity.util)

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
