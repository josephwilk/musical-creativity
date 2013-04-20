(ns musical-creativity.composers.sonify-data
  (:require [clojure.math.numeric-tower :as math]
            [musical-creativity.events :as events]))

(load-file "data/cassiopeia.clj")

(defn normalize
  "This function normalizes number into low-high-1 range to the low-high-2 range."
  [low1 high1 number low2 high2]
  (math/round (+ (* (/ (- number low1)(- high1 low1))(- high2 low2)) low2)))

(defn normalize-numbers [numbers min max low high]
  (map #(normalize min max % low high) numbers))

(defn sonify [data]
  (let [max (apply max data)
        min (apply min data)]
    (normalize-numbers data min max 24 108)))

(defn compose []
  (events/make (sonify data.cassiopeia/a)))