(ns musical-creativity.composers.markov
  (:require
   [musical-creativity.events :as events]))

(load-file "data/bach.clj")

(def default-events
  [{:pitch 60 :time 0}
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
  {:start [60]
   :events default-events
   :length 50
   :phrase-length 1
   :depth 1})

(defn pick-pitches-fn [depth stm]
  (fn [pitches]
    (let [last-chunk (take-last depth pitches)
          candidates (stm last-chunk)
          picked (rand-nth candidates)]
      (if picked
        (concat pitches picked)
        pitches))))

(defn- compose-pitches [start length depth stm]
  (nth (iterate (pick-pitches-fn depth stm) start) length))

(defn probabilities-for [depth stm chunk]
  (let [prefix  (take depth chunk)
        suffix  (drop depth chunk)
        stm-row (or (stm prefix) [])]
    (assoc stm prefix (conj stm-row suffix))))

(defn- state-transition-matrix-probabilities [pitches depth phrase-length]
  (let [pitch-chunks (partition (+ depth phrase-length) 1 pitches)]
    (reduce (partial probabilities-for depth) {} pitch-chunks)))

(defn- compose-markov [start length depth stm]
  (events/make (flatten (compose-pitches start length depth stm)) 0 350))

(defn compose
  ([] (compose (:events defaults) (:start defaults) (:phrase-length defaults) (:length defaults)))
  ([events start phrase-length length]
     (assert (zero? (mod length phrase-length))
             (format "length %s is no multiple of phrase-length %s" length phrase-length))
     (let [depth (count start)]
       (if (= (count start) depth)
         (let [pitches (map :pitch events)
               stm (state-transition-matrix-probabilities pitches depth phrase-length)]
           (compose-markov start (/ length phrase-length) depth stm))
         (println (str "Error: Start sequence must be same as depth: start:" (count start) " depth:" depth))))))
