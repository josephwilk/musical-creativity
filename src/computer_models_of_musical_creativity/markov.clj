(ns computer-models-of-musical-creativity.markov)

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

(def empty-state-transition-matrix
  {60 []
   62 []
   64 []
   65 []
   67 []
   69 []
   71 []
   72 []})

(def state-transition-matrix (atom empty-state-transition-matrix))

(defn- clear-matrix! []
  (reset! state-transition-matrix empty-state-transition-matrix))

(defn- choose-one [l]
  (nth (rand-int (count l)) l))

(defn- compose-m [start length]
  (if (= length 0) []
      (let [test (choose-one (@state-transition-matrix start))]
        (cons test
              (compose-m test (- length 1))))))

(defn- make-events [list-of-pitches & [time]]
  (if (nil? list-of-pitches) []
      (cons [{:time (or time 0)
              :pitch (first list-of-pitches)}]
            (make-events (rest list-of-pitches) (+ time 250)))))

(defn- firstn [number list]
  (take number list))

(defn- get-pitches [events]
  (map #(:pitch %) events))

(defn- compose-markov [start length]
  (make-events (compose-m start length)))

(defn put-probabilities [[first-pitch second-pitch] stm]
  (let [stm-key first-pitch
        stm-row (stm stm-key)]
    (assoc stm stm-key (conj stm-row second-pitch))))

(defn- put-probabilities-into-state-transition-matrix [pitches stm]
  (if (empty? (rest pitches)) stm
     (do
       (reset! state-transition-matrix (put-probabilities (firstn 2 pitches) stm))
       (put-probabilities-into-state-transition-matrix (rest pitches) stm))))

(defn compose-new-music-based-on-markovian-probabilities [start length events]
  (clear-matrix!)
  (reset! state-transition-matrix (put-probabilities-into-state-transition-matrix (get-pitches events) @state-transition-matrix))
  (compose-markov start length))