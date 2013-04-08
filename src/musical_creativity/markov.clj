(ns musical-creativity.markov)

(def events
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

(def empty-state-transition-matrix {})

(defn- compose-m [start length stm]
  (if (= length 0) []
      (let [candidates (stm start)
            picked (rand-nth candidates)]
        (cons picked
              (compose-m picked (- length 1) stm)))))

(defn- make-events [list-of-pitches & [time]]
  (let [time (or time 0)]
    (if (empty? list-of-pitches) []
        (cons {:time (or time 0)
                :pitch (first list-of-pitches)}
              (make-events (rest list-of-pitches) (+ time 250))))))

(defn- firstn [number list]
  (take number list))

(defn- get-pitches [events]
  (map #(:pitch %) events))

(defn- compose-markov [start length stm]
  (make-events (compose-m start length stm)))

(defn probabilities-for [stm [first-pitch second-pitch]]
  (let [stm-key first-pitch
        stm-row (or (stm stm-key) [])]
    (assoc stm stm-key (conj stm-row second-pitch))))

(defn- state-transition-matrix-probabilities [pitches]
  (let [pitch-pairs (partition 2 1 pitches)]
    (reduce probabilities-for {} pitch-pairs)))

(defn compose [start length events]
  (let [stm empty-state-transition-matrix
        pitches (get-pitches events)
        stm (state-transition-matrix-probabilities pitches)]
    (compose-markov start length stm)))
