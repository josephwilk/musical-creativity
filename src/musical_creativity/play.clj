(ns musical-creativity.play
  (:require
    [overtone.live :refer :all]
    [overtone.music.pitch :as pitch]))

(definst saw-wave [freq 440 attack 0.01 sustain 0.4 release 0.1 vol 0.4]
  (* (env-gen (lin-env attack sustain release) 1 1 0 1 FREE)
     (saw freq)
     vol))

(defn saw2 [music-note]
  (saw-wave (midi->hz (note music-note))))

(defn note->hz [music-note]
  (midi->hz (note music-note)))

(defn play-chord [a-chord]
  (doseq [note a-chord] (saw2 note)))

(defn play-event [event start-time]
  (let [pitch (:pitch event)
        note-time (+ start-time (:time event))
        note-name (find-note-name pitch)]
    (at note-time (play-chord (pitch/chord note-name :major)))))

(defn play [events]
  (let [start-time (overtone.live/now)]
    (dorun (map #(play-event % start-time) events))))