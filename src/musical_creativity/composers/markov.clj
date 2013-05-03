(ns musical-creativity.composers.markov
  (:require
   [musical-creativity.events :as events]))

(load-file "data/bach.clj")

(def default-events
  [{:pitch 60 :time 0}
   {:pitch 60 :time 0}
   {:pitch 62 :time 1000}
   {:pitch 64 :time 2000}
   {:pitch 65 :time 3000}
   {:pitch 67 :time 4000}
   {:pitch 65 :time 5000}
   {:pitch 69 :time 6000}
   {:pitch 71 :time 7000}
   {:pitch 72 :time 8000}
   {:pitch 60 :time 9000}])

(def defaults
  {:start 60
   :events default-events
   :length 50})

(defn pick-pitches-fn [stm]
  (fn [pitches]
    (let [last-pitch (last pitches)
          candidates (stm last-pitch)
          picked (rand-nth candidates)]
      (conj pitches picked))))

(defn- compose-pitches [start length stm]
  (nth (iterate (pick-pitches-fn stm) [start]) length))

(defn probabilities-for [stm [first-pitch second-pitch]]
  (let [stm-key first-pitch
        stm-row (or (stm stm-key) [])]
    (assoc stm stm-key (conj stm-row second-pitch))))

(defn- state-transition-matrix-probabilities [pitches]
  (let [pitch-pairs (partition 2 1 pitches)]
    (reduce probabilities-for {} pitch-pairs)))

(defn- compose-markov [start-pitch length stm]
  (events/make (compose-pitches start-pitch length stm) 0 350))

(defn compose
  ([] (compose (:events defaults) (:start defaults) (:length defaults)))
  ([events start-pitch length]
     (let [pitches (map :pitch events)
           stm (state-transition-matrix-probabilities pitches)]
       (compose-markov start-pitch length stm))))
