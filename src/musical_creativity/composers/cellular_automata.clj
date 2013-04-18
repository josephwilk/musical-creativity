(ns musical-creativity.composers.cellular-automata
  (:require
    [clojure.math.numeric-tower :as math]
    [clojure.pprint :refer :all]
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

(def rule-22
   [[["*" "*" "*"] "0"] [["*" "*" "0"] "0"] [["*" "0" "*"] "0"] [["*" "0" "0"] "*"] [["0" "*" "*"] "0"] [["0" "*" "0"] "*"] [["0" "0" "*"] "*"] [["0" "0" "0"] "0"]])

(def rule-57
  [[["*" "*" "*"] "0"][["*" "*" "0"] "0"][["*" "0" "*"] "*"][["*" "0" "0"] "*"] [["0" "*" "*"] "*"][["0" "*" "0"] "0"][["0" "0" "*"] "0"][["0" "0" "0"] "*"]])

(def rule-73
  [[["*" "*" "*"] "0"][["*" "*" "0"] "*"][["*" "0" "*"] "0"][["*" "0" "0"] "0"] [["0" "*" "*"] "*"][["0" "*" "0"] "0"][["0" "0" "*"] "0"][["0" "0" "0"] "*"]])

(def rule-75
  [[["*" "*" "*"] "0"][["*" "*" "0"] "*"][["*" "0" "*"] "0"][["*" "0" "0"] "0"] [["0" "*" "*"] "*"][["0" "*" "0"] "0"][["0" "0" "*"] "*"][["0" "0" "0"] "*"]])

(def rule-121
  [[["*" "*" "*"] "0"][["*" "*" "0"] "*"][["*" "0" "*"] "*"][["*" "0" "0"] "*"] [["0" "*" "*"] "*"][["0" "*" "0"] "0"][["0" "0" "*"] "0"][["0" "0" "0"] "*"]])

(def default-start
  ["0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "*" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0"])

(def events (atom []))
(def store-rules (atom []))

(defn log-event [entry-time pitch]
  (swap! events conj (events/make-event entry-time {:pitch pitch})))

(defn- pitch-from-position [position]
  (math/round (+ (* position (/ 60 200)) 49)))

(defn create-event [position timing]
  (let [pitch (pitch-from-position position)
        entry-time (* timing 500)]
    (when (not-any? #(and (= pitch (:pitch %)) (= entry-time (:time %))) @events)
      (log-event entry-time pitch))))

(defn matching-rule-result [group rules]
  (some (fn [rule]
          (when (= (vec group) (first rule))
            (second rule))) rules))

(defn apply-rule [group rules position timing]
  (let [rule-result (matching-rule-result group rules)]
    (when (= rule-result "*") (log-event position timing))
    (if rule-result
      rule-result
      "0")))

(defn create-the-row [old-row rules timing & [position]]
  (let [position (or position 1)]
    (if (empty? old-row) []
        (cons (apply-rule (take 3 old-row) rules position timing)
              (create-the-row (rest old-row) rules (+ 10 timing) (+ 1 position))))))

(defn create-rows [number start rules & [up-number]]
  (println start)
  (let [up-number (or up-number 1)]
    (when-not (= number up-number)
      (let [row (create-the-row start rules up-number)
            new-row (cons "0" (butlast row))]
        (create-rows number new-row rules (+ 1 up-number))
        @events))))

(defn compose [& [rules]]
  (reset! store-rules [])
  (reset! events [])
  (let [rules (or rules default-rules)
        events (create-rows 25 default-start rules)]
        (remove nil? events)))