(ns musical-creativity.events
  (:require [overtone.music.pitch :as opitch]))

(defn- random-duration []
  (+ 250 (rand-int 800)))

(defn- random-channel []
  (+ 1 (rand-int 16)))

(defn- extract-pitch [pitch]
  (if (map? pitch)
    (:pitch pitch)
    (opitch/note pitch)))

(defn timepoint-of [event]
  (nth event 0))

(defn pitch-of [event]
  (nth event 1))

(defn velocity-of [event]
  (nth event 2))

(defn channel-of [event]
  (nth event 3))

(defn instrument-of [event]
  (nth event 4))

(defn midi-to-event [midi]
  {:time     (timepoint-of midi)
   :pitch    (pitch-of midi)
   :velocity (velocity-of midi)
   :channel  (channel-of midi)
   :instrument (when (= 5 (count midi)) (instrument-of midi))})

(defn make-event [ontime data & [channel]]
  {:time ontime
   :log (or (:out data) (:log data))
   :pitch (int (extract-pitch data))
   :channel (or channel 1)})


(defn fill-default [ontime data]
  (merge {:time     ontime
          :log      nil
          :pitch    64
          :duration 200
          :channel  1}
         data))

(defn random-make [pitch-groupings & [ontime]]
  (let [ontime (or ontime 0)]
    (if (empty? pitch-groupings)
      []
      (let [duration (random-duration)]
        (concat [(make-event ontime (first pitch-groupings) (random-channel))]
                 (random-make (rest pitch-groupings) (+ ontime duration)))))))

(defn make [pitch-groupings & [ontime interval]]
  (loop [fixontime                  (or ontime 0)
         duration                   (or interval 300)
         [head-group & more-groups] pitch-groupings
         acc                        []]
    (if (empty? more-groups)
      acc
      (recur (+ fixontime duration) duration more-groups
             (conj acc (make-event fixontime head-group))))))

(defn make-from-maps [input-maps ontime]
  (loop [current               ontime
         [data & more-data]    input-maps
         acc                   []]
    (if (nil? data)
      acc
      (recur (+ current (:duration data))
             more-data
             (conj acc (fill-default current data))))))

(defn make-pairs
  ([pitch-groupings] (make-pairs pitch-groupings 0))
  ([pitch-groupings ontime]
     (let [interval 900
           ontimes (range 0 (* interval (count pitch-groupings)) interval)]
       (flatten
        (map (fn [[pitch1 pitch2] ontime]
               [(make-event ontime pitch1 1)
                (make-event ontime pitch2 2)])
             pitch-groupings ontimes)))))

(defn as-chords
  ([pitch-groupings] (as-chords pitch-groupings 0))
  ([pitch-groupings ontime]
     (let [interval 900
           ontimes (range 0 (* interval (count pitch-groupings)) interval)]
       (flatten
        (map (fn [chord ontime]
               (let [chord-pitches (opitch/chord (:pitch chord) :major)]
                 (map #(make-event ontime % 1) chord-pitches)))
             pitch-groupings ontimes)))))
