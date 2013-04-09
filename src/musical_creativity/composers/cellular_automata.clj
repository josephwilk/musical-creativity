(ns musical-creativity.composers.cellular-automata
  (:require [clojure.math.numeric-tower :as math]))

(def events (atom []))
(def store-rules (atom []))

(defn create-event [position timing]
  (let [pitch (math/round (+ (* position (/ 60 261)) 36))
        time (* timing 250)]
    {:time time :pitch pitch}))

(defn matching-rule-result [group rules]
  (some (fn [rule]
          (when (= (vec group) (first rule))
            (second rule))) rules))

(defn apply-the-rule [group rules position timing]
  (let [rule-result (matching-rule-result group rules)]
    (do
      (when (= rule-result *)
        (swap! events conj (create-event position timing)))
      (if rule-result
        rule-result
        0))))

(defn create-the-row [old-row rules timing & [position]]
  (let [position (or position 1)]
    (if (empty? old-row) []
        (cons (apply-the-rule (take 3 old-row) rules position timing)
              (create-the-row (rest old-row) rules timing (+ 1 position))))))

(defn compose [number start rules & [up-number]]
  (let [up-number (or up-number 1)]
    (if (= number up-number)
      (reverse @store-rules)
      (do
        (swap! store-rules conj start)
        (compose number
                 (cons 0 (butlast (create-the-row start rules up-number)))
                 rules
                 (+ 1 up-number))
        (reverse @events)))))