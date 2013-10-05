(ns musical-creativity.composers.apprentice.graph
  (:require
   [clojure.math.numeric-tower :as math]

   [loom.graph :refer :all]
   [loom.alg :refer :all]
   [loom.gen :refer :all]
   [loom.attr :refer :all]
   [loom.label :refer :all]
   [loom.io :refer :all]

   [musical-creativity.composers.apprentice :as apprentice]))

(defn network->edges [network]
  (vec (reduce (fn [edges [k v]]
                 (concat edges (distinct (map (fn [[name weight]] [(keyword k) (keyword name) (format "%.2f" weight)]) (:associations v))))) [] network)))

(defn view-graph [] (view (apply weighted-digraph (network->edges @apprentice/*words-store*))))

