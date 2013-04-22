(ns musical-creativity.instruments
  (:require
    [overtone.live :refer :all]
    [overtone.music.pitch :as pitch]
    [overtone.inst.sampled-piano :as piano]
    [overtone.inst.synth :as synth]
    [overtone.synth.stringed :as stringed]))

(def my-pool (overtone.at-at/mk-pool))

(definst saw-wave [freq 440 attack 0.01 sustain 0.4 release 0.1 vol 0.4]
  (* (env-gen (lin-env attack sustain release) 1 1 0 1 FREE)
     (saw freq)
     vol))

(definst kick [freq 120 dur 0.3 width 0.5]
  (let [freq-env (* freq (env-gen (perc 0 (* 0.99 dur))))
        env (env-gen (perc 0.01 dur) 1 1 0 1 FREE)
        sqr (* (env-gen (perc 0 0.01)) (pulse (* 2 freq) width))
        src (sin-osc freq-env)
        drum (+ sqr (* env src))]
    (compander drum drum 0.2 1 0.1 0.01 0.01)))

(definst c-hat [amp 0.8 t 0.04]
  (let [env (env-gen (perc 0.001 t) 1 1 0 1 FREE)
        noise (white-noise)
        sqr (* (env-gen (perc 0.01 0.04)) (pulse 880 0.2))
        filt (bpf (+ sqr noise) 9000 0.5)]
    (* amp env filt)))

(def g (stringed/guitar))

(defn- saw2 [music-note]
  (saw-wave (midi->hz (note music-note))))

(defn- note->hz [music-note]
  (midi->hz (note music-note)))

(defn- play-chord [a-chord]
  (doseq [note a-chord] (saw2 note)))

(defn play-event [event start-time player-fn]
  (let [pitch-to-play (:pitch event)
        log (:log event)
        note-time (+ start-time (:time event))]
    (when pitch-to-play
      (at note-time (player-fn pitch-to-play))
      (overtone.at-at/at (- note-time 10) #(when log (do (print log) (flush))) my-pool))))

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
        log (:log event)
        note-time (+ start-time (:time event))
        note-name (find-note-name pitch)]
    (at note-time (stringed/guitar-strum g note-name :down 0.25))))

(defn tb303 [event start-time]
  (play-event event start-time synth/tb303))