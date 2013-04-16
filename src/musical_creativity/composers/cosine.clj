(ns musical-creativity.composers.cosine
  (:require
    [clojure.math.numeric-tower :as math]
    [musical-creativity.events :as events]))

(defn run-cosine-fn [number x]
  (if (zero? number) []
      (cons (/ 1 (Math/cos (math/expt x 2)))
            (run-cosine-fn (- number 1) (+ 1 x)))))

(defn normalize [low1 high1 number low2 high2]
  (math/round (+ (* (/ (- number low1)(- high1 low1))(- high2 low2)) low2)))

(defn normalize-cosines [cosines min-value max-value low high]
  (map #(normalize min-value max-value % low high) cosines))

(defn calculate-cosine [n midi-low midi-high]
  (let [test (run-cosine-fn n 1)
        max-value (apply max test)
        min-value (apply min test)]
    (normalize-cosines test min-value max-value midi-low midi-high)))

(defn compose []
  (events/make (calculate-cosine 20 24 108) 0 300))