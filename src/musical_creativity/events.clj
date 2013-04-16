(ns musical-creativity.events)

(defn- random-duration []
  (+ 250 (rand-int 800)))

(defn- random-channel []
  (+ 1 (rand-int 16)))

(defn- extract-pitch [pitch]
  (if (map? pitch)
    (:pitch pitch)
    pitch))

(defn make-event [ontime data & [channel]]
  {:time ontime
   :log (:out data)
   :pitch (int (extract-pitch data))
   :channel (or channel 1)})

(defn random-make [pitch-groupings & [ontime]]
  (let [ontime (or ontime 0)]
    (if (empty? pitch-groupings) 
      []
      (let [duration (random-duration)]
        (concat [(make-event ontime (first pitch-groupings) (random-channel))]
                 (random-make (rest pitch-groupings) (+ ontime duration)))))))

(defn make [pitch-groupings & [ontime interval]]
  (let [ontime (or ontime 0)]
    (if (empty? pitch-groupings) 
      []
      (let [duration (or interval 300)]
        (concat [(make-event ontime (first pitch-groupings))]
                 (make (rest pitch-groupings) (+ ontime duration) interval))))))
