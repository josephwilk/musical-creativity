(ns musical-creativity.instruments
  (:require
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
  (let [pitch-to-play (:pitch event)
        note-time (+ start-time (:time event))]
    (at note-time (player-fn pitch-to-play))))

(defn play-saw [event start-time]
  (let [pitch (:pitch event)
        note-time (+ start-time (:time event))
        note-name (find-note-name pitch)]
    (at note-time (play-chord (pitch/chord note-name :major)))))

(defn piano [event start-time]
  (play-event event start-time piano/sampled-piano))

(defn ping [event start-time]
  (play-event event start-time synth/ping))

(defn guitar [event start-time]
  (let [pitch (:pitch event)
        note-time (+ start-time (:time event))
        note-name (find-note-name pitch)]
    (at note-time (stringed/guitar-strum g note-name :down 0.25))))

(defn tb303 [event start-time]
  (play-event event start-time synth/tb303))