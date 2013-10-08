(ns musical-creativity.band
  (:require
   [overtone.live :refer :all]
   [overtone.inst.synth :refer :all]
   [overtone.inst.sampled-piano :as piano]
   [overtone.inst.drum :as drum]

   [musical-creativity.instruments :as instruments]))

(defn flute-fn [pitch & rest]
  (println :flute)
  (simple-flute (midi->hz pitch)))
(defn bass-fn  [pitch & rest]
  (println :bass)
  (bass (midi->hz pitch)))
(defn tom-fn [pitch & rest]
  (println :tom)
  (drum/tom (midi->hz pitch)))
(defn kick-fn [pitch & rest]
  (drum/kick (midi->hz pitch)))

;(10 20 25 26 41 50 52 55 56 58 59 60 61 62 63 64 65 66 67 68 69 70 71
;72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94
;95 96 97 98 99 100 101 102 103 105 106 107 108 109 110 111 113 114
;115 117 118 119 120 121 123 125 126 127)

(defn find-instrument [id]
  (case id
    81 instruments/sawtooth
    90 instruments/sawish
    nil))

(defn band-fn [event start-time]
  (when-let [play-with-instrument (find-instrument (:instrument event))]
    (println :instrument play-with-instrument)
    (play-with-instrument event start-time)))

(defn play [events]
  (let [start-time (+ 1000 (overtone.live/now))]
    (dorun (map #(band-fn % start-time) events))))
