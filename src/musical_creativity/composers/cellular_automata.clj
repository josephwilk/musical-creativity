(ns musical-creativity.composers.cellular-automata
  (:require
    [clojure.math.numeric-tower :as math]
    [musical-creativity.events :as events]))

(def default-rules
    [[["*" "*" "*"] "0"]
     [["*" "*" "0"] "*"]
     [["*" "0" "*"] "0"]
     [["*" "0" "0"] "*"]
     [["0" "*" "*"] "*"]
     [["0" "*" "0"] "*"]
     [["0" "0" "*"] "0"]
     [["0" "0" "0"] "0"]])

(def default-start
  ["0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "*" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0"])

(def events (atom []))
(def store-rules (atom []))

(defn- pitch-from-position [position]
  (math/round (+ (* position (/ 60 261)) 56)))

(defn create-event [position timing]
  (events/make-event (* timing 250) (pitch-from-position position)))

(defn matching-rule-result [group rules]
  (some (fn [rule]
          (when (= (vec group) (first rule))
            (second rule))) rules))

(defn apply-the-rule [group rules position timing]
  (let [rule-result (matching-rule-result group rules)]
    (when (= rule-result "*")
      (swap! events conj (create-event position timing)))
    (if rule-result
      rule-result
      "0")))

(defn create-the-row [old-row rules timing & [position]]
  (let [position (or position 1)]
    (if (empty? old-row) []
        (cons (apply-the-rule (take 3 old-row) rules position timing)
              (create-the-row (rest old-row) rules timing (+ 1 position))))))

(defn create-rows [number start rules & [up-number]]
  (let [up-number (or up-number 1)]
    (if (= number up-number)
      (reverse @store-rules)
      (do
        (swap! store-rules conj start)
        (create-rows number
                 (cons "0" (butlast (create-the-row start rules up-number)))
                 rules
                 (+ 1 up-number))
        (reverse @events)))))

(defn compose []
  (create-rows 20 default-start default-rules))