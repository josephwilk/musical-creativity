(ns musical-creativity.composers.cellular-automata
  (:require
    [clojure.math.numeric-tower :as math]
    [musical-creativity.events  :as events]))

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
   [[["*" "*" "*"] "0"]
    [["*" "*" "0"] "0"]
    [["*" "0" "*"] "0"]
    [["*" "0" "0"] "*"]
    [["0" "*" "*"] "0"]
    [["0" "*" "0"] "*"]
    [["0" "0" "*"] "*"]
    [["0" "0" "0"] "0"]])

(def rule-57
  [[["*" "*" "*"] "0"]
   [["*" "*" "0"] "0"]
   [["*" "0" "*"] "*"]
   [["*" "0" "0"] "*"]
   [["0" "*" "*"] "*"]
   [["0" "*" "0"] "0"]
   [["0" "0" "*"] "0"]
   [["0" "0" "0"] "*"]])

(def rule-73
  [[["*" "*" "*"] "0"]
   [["*" "*" "0"] "*"]
   [["*" "0" "*"] "0"]
   [["*" "0" "0"] "0"]
   [["0" "*" "*"] "*"]
   [["0" "*" "0"] "0"]
   [["0" "0" "*"] "0"]
   [["0" "0" "0"] "*"]])

(def rule-75
  [[["*" "*" "*"] "0"]
   [["*" "*" "0"] "*"]
   [["*" "0" "*"] "0"]
   [["*" "0" "0"] "0"]
   [["0" "*" "*"] "*"]
   [["0" "*" "0"] "0"]
   [["0" "0" "*"] "*"]
   [["0" "0" "0"] "*"]])

(def rule-121
  [[["*" "*" "*"] "0"]
   [["*" "*" "0"] "*"]
   [["*" "0" "*"] "*"]
   [["*" "0" "0"] "*"]
   [["0" "*" "*"] "*"]
   [["0" "*" "0"] "0"]
   [["0" "0" "*"] "0"]
   [["0" "0" "0"] "*"]])

(def default-start
  ["0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "*" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0"])

(def events (atom []))
(def store-rules (atom []))
(def _start (atom nil))

(defn log-a-event [entry-time pitch]
  (swap! events conj (events/make-event entry-time {:out @_start :pitch pitch})))

(defn- pitch-from-position [position]
  (math/round (+ (* position (/ 60 200)) 55)))

(defn- duplicate-events-fn [pitch entry-time]
  (fn [event]
    (and (= pitch (:pitch event))
         (= entry-time (:time event)))))

(defn create-event [position timing]
  (let [pitch (pitch-from-position position)
        entry-time (* timing 100)
        duplicate-events (duplicate-events-fn pitch entry-time)]
    (when (not-any? duplicate-events @events)
      (log-a-event entry-time pitch))))

(defn matching-rule-result [group rules]
  (some (fn [rule]
          (when (= (vec group) (first rule))
            (second rule))) rules))

(defn apply-rule [group rules position timing]
  (let [rule-result (matching-rule-result group rules)]
    (when (= rule-result "*") (create-event position timing))
    (or rule-result "0")))

(defn create-the-row [old-row rules timing & [position]]
  (let [position (or position 1)]
    (if (empty? old-row) []
        (cons (apply-rule (take 3 old-row) rules position timing)
              (create-the-row (rest old-row) rules (+ 30 timing) (+ 1 position))))))

(defn create-rows [number start rules & [up-number]]
  (println start)
  (reset! _start start)
  (let [up-number (or up-number 1)]
    (when-not (= number up-number)
      (let [row (create-the-row start rules up-number)
            new-row (cons "0" (butlast row))]
        (create-rows number new-row rules (+ 1 up-number))
        @events))))

(defn compose [& [rules]]
  (reset! events [])
  (let [rules (or rules default-rules)
        the-events (create-rows 25 default-start rules 0)
        the-events (map #(select-keys % [:pitch :log]) the-events)
        the-events (events/make the-events)]
         (remove nil? the-events)))
