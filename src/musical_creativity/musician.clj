(ns musical-creativity.musician
  (:require
    [overtone.live :refer :all]
    [overtone.music.pitch :as pitch]
    [overtone.inst.sampled-piano :as piano]
    [overtone.inst.synth :as synth]
    [overtone.synth.stringed :as stringed]))

(definst saw-wave [freq 440 attack 0.01 sustain 0.4 release 0.1 vol 0.4]
  (* (env-gen (lin-env attack sustain release) 1 1 0 1 FREE)
     (saw freq)
     vol))

(def g (stringed/guitar))

(defn- saw2 [music-note]
  (saw-wave (midi->hz (note music-note))))

(defn- note->hz [music-note]
  (midi->hz (note music-note)))

(defn- play-chord [a-chord]
  (doseq [note a-chord] (saw2 note)))

(defn play-event [event start-time player-fn]
  (let [pitch (:pitch event)
        note-time (+ start-time (:time event))]
    (at note-time (player-fn pitch))))


(defn play-saw [event start-time]
  (let [pitch (:pitch event)
        note-time (+ start-time (:time event))
        note-name (find-note-name pitch)]
    (at note-time (play-chord (pitch/chord note-name :major)))))

(defn play-piano [event start-time]
  (play-event event start-time piano/sampled-piano))

(defn play-ping [event start-time]
  (play-event event start-time synth/ping))

(defn play-guitar [event start-time]
  (let [pitch (:pitch event)
        note-time (+ start-time (:time event))
        note-name (find-note-name pitch)]
    (at note-time (stringed/guitar-strum g note-name :down 0.25))))

(defn play-tb303 [event start-time]
  (play-event event start-time synth/tb303))

(defn play [events & [musician-fn]]
  (let [start-time (overtone.live/now)]
    (dorun (map #(musician-fn % start-time) events))))
