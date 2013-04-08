(ns musical-creativity.cosine
  (:require [clojure.math.numeric-tower :as math]))

(defn run-cosine-fn [number x]
  (if (zero? number) []
      (cons (/ 1 (Math/cos (math/expt x 2)))
            (run-cosine-fn (- 1 number)(+ 1 x)))))

(defn normalize [low1 high1 number low2 high2]
  (math/round (+ (* (/ (- number low1)(- high1 low1))(- high2 low2)) low2)))

(defn normalize-cosines [cosines min max low high]
  (if (empty? cosines) []
      (cons (normalize min max (first cosines) low high)
            (normalize-cosines (rest cosines) min max low high))))

(defn cosine [n midi-low midi-high]
  (let [test (run-cosine-fn n 1)
        max (apply #'max test)
        min (apply #'min test)]
    (normalize-cosines test min max midi-low midi-high)))

(defn make-event [ontime pitch channel]
  {:time ontime
   :pitch (if (symbol? pitch) (eval pitch) pitch)})

(defn make-events [pitch-groupings & [random ontime]]
  (let [random (or random nil)
        ontime (or ontime 0)]
    (if (empty? pitch-groupings) []
        (let [duration (if random (+ 250 (rand-int 1750)) 1000)]
          (conj (list (make-event ontime (first pitch-groupings) (if random (+ 1 (rand-int 16)) 1)))
                (make-events (rest pitch-groupings) random (+ ontime duration)))))))
