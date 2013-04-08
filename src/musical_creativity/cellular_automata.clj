(ns musical-creativity.cellular-automata
  (:require [clojure.math.numeric-tower :as math]))

(def events (atom []))
(def store-rules (atom []))

(defn create-event [position timing]
  [(* timing 250) (math/round (+ (* position (/ 60 261)) 36)) 250 1 100])

(defn matching-rule [group rules]
  (some (fn [rule]
          (when (= (vec group) (first rule))
            rule)) rules))

(defn apply-the-rule [group rules position timing]
  (let [test (second (matching-rule group rules))]
    (do
      (when (= test '*)
        (swap! events concat (create-event position timing)))
      (if test
        test
        '0))))

(defn create-the-row [old-row rules timing & [position]]
  (let [position (or position 1)]
    (if (empty? old-row) []
        (cons (apply-the-rule (take 3 old-row) rules position timing)
              (create-the-row (rest old-row) rules timing (+ 1 position))))))

(defn create-lists [number start rules & [up-number]]
  (let [up-number (or up-number 1)]
    (if (= number up-number)
      (reverse @store-rules)
      (do
        (swap! store-rules concat start)
        (create-lists number
                      (cons '0 (butlast (create-the-row start rules up-number)))
                      rules
                      (+ 1 up-number))
        (reverse @events)))))