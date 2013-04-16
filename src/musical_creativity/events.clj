(ns musical-creativity.events)

(defn random-duration []
  (+ 250 (rand-int 800)))

(defn random-channel []
  (+ 1 (rand-int 16)))

(defn make-event [ontime pitch & [channel]]
  {:time ontime
   :pitch (:pitch pitch)
   :channel (or channel 1)})

(defn random-make [pitch-groupings & [ontime]]
  (let [ontime (or ontime 0)]
    (if (empty? pitch-groupings) 
      []
      (let [duration (random-duration)]
        (concat [(make-event ontime (first pitch-groupings) (random-channel))]
                 (random-make (rest pitch-groupings) (+ ontime duration)))))))

(defn make [pitch-groupings & [ontime]]
  (let [ontime (or ontime 0)]
    (if (empty? pitch-groupings) 
      []
      (let [duration 1000]
        (concat [(make-event ontime (first pitch-groupings))]
                 (make (rest pitch-groupings) (+ ontime duration)))))))
