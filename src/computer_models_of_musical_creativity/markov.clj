(ns computer-models-of-musical-creativity.markov)

(def events '((0 60 1000 1 127)(0 60 1000 1 127)(1000 62 1000 1 127)
              (2000 64 1000 1 127)(3000 65 1000 1 127)(4000 67 1000 1 127)
              (5000 65 1000 1 127)(6000 69 1000 1 127)(7000 71 1000 1 127)
              (8000 72 1000 1 127)(9000 60 1000 1 127)))

(def state-transition-matrix '((60 ())
			   				   (62 ())
                               (64 ())
                               (65 ())
                               (67 ())
                               (69 ())
                               (71 ())
                               (72 ())))

(def rs [make-random-state t])

(defn compose-new-music-based-on-markovian-probabilities [start length events])

(defn put-probabilities-into-state-transition-matrix [pitches state-transition-matrix])

(defn put-probabilities [two-pitches state-transition-matrix])

(defn compose-markov [start length])

(defn compose-m [start length])

(defn make-events [list-of-pitches &optional (time 0)])

(defn choose-one [list])

(defn firstn [number list])

(defn get-pitches [events])

(defn clear-matrix [])