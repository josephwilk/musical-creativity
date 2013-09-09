(ns musical-creativity.instruments
  "Some instruments imported from Chris Ford's Whelmed: https://github.com/ctford/whelmed/blob/master/src/whelmed/instrument.clj"
  (:require
    [overtone.live :refer :all]
    [overtone.music.pitch :as pitch]
    [overtone.inst.sampled-piano :as piano]
    [overtone.inst.synth :as synth]
    [overtone.synth.stringed :as stringed]))

(defn piano-scale-field [pitch-field] (filter #(and (>= % 21) (<= % 108)) pitch-field))

(def my-pool (overtone.at-at/mk-pool))

(definst bell-by-freq [frequency 440 duration 1000 volume 1.0
  h0 1 h1 0.6 h2 0.4 h3 0.25 h4 0.2 h5 0.15]
  (let [harmonics   [ 1  2  3  4.2  5.4 6.8]
        proportions [h0 h1 h2   h3   h4  h5]
        proportional-partial
          (fn [harmonic proportion]
            (let [envelope
                    (* volume 1/5 (env-gen (perc 0.01 (* proportion (/ duration 1000)))))
                  overtone
                    (* harmonic frequency)]
              (* 1/2 proportion envelope (sin-osc overtone))))
        partials
          (map proportional-partial harmonics proportions)
        whole (mix partials)]
      (detect-silence whole :action FREE)
      whole))

(defn play-bell [musical-note]
  (bell-by-freq (note->hz (note musical-note))))

(definst sawish-by-freq [freq 440 duration 1500 vibrato 8 depth 1 volume 1.0]
  (let [envelope (env-gen (perc 0.2 (/ duration 1000)) :action FREE)]
    (-> (square freq)
        (* 0.7 volume)
        (* envelope)
        (rlpf (mul-add (sin-osc vibrato) (* freq depth) (* 2 freq))))))

(defn play-sawish [musical-note]
  (sawish-by-freq (note->hz (note musical-note))))

(definst organ-with-freq [freq 440 dur 1000 vol 1.0]
  (->
    (map #(sin-osc (* freq %)) (range 1 5))
    mix
    (* 1/6 vol)
    (* (env-gen (asr 0.1 1.0 0.5)
         (line:kr 1.0 0.0 (/ dur 1000))
         :action FREE))
    (lpf (mul-add (sin-osc 5) freq (* freq 5)))))

(defn play-organ [musical-note]
  (organ-with-freq (note->hz (note musical-note))))

(definst woah-by-freq [freq 440 duration 1000 volume 1.0]
  (let [fenv (* (env-gen (perc 0.1 (/ duration 1000))) freq)
        aenv (env-gen (perc 0.005 (/ duration 1000)) :action FREE)]
    (* volume (sin-osc fenv (* 0.5 Math/PI)) aenv)))

(defn play-woah [musical-note]
  (woah-by-freq (note->hz (note musical-note))))

(definst sawnoff-by-freq [freq 440 depth 10]
  (let [envelope (env-gen (perc 0.1 0.9) :action FREE)]
    (*
      envelope
      (sin-osc freq)
      (sin-osc (* 2 freq))
      (saw (+ freq (* depth (lf-saw:kr 0.1 0.2)))))))

(defn play-sawnoff [musical-note]
  (sawnoff-by-freq (note->hz (note musical-note))))

(definst groan-by-freq [freq 440 duration 10000 vibrato 8/3 volume 1.0]
  (let [length (/ duration 1000)
        envelope (* (sin-osc vibrato)
                    (env-gen (perc 0.1 length) :action FREE))]
    (*
     0.7
     volume
     envelope
     (+
      (* (sin-osc 0.5) (+ 0.1 (saw freq)))
      (* (sin-osc 0.8) (+ -0.03 (square freq)))
      (+ -0.04 (sin-osc freq))))))

(defn play-groan [musical-note]
  (groan-by-freq (note->hz (note musical-note))))

(definst shudder-by-freq [freq 440 vibrato 6]
  (let [envelope (env-gen (perc 2 1.5) :action FREE)]
    (*
      (* envelope (sin-osc vibrato))
      (square freq)
      (sin-osc freq))))

(defn play-shudder [musical-note]
  (shudder-by-freq (note->hz (note musical-note))))

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

(defn linear-map
  "given points (x0,y0), (x1,y1) calculate linear relation y given x"
  [x0 x1 y0 y1 x]
  (let [dydx (/ (- y1 y0) (- x1 x0))
        dx (- x x0)]
    (+ y0 (* dydx dx))))

(defn velocity-to-attack
  "sampled-piano uses attack & level, not velocity"
  [v]
  (linear-map 0 127 0.2 0.05 v))

(defn velocity-to-level
  "sampled-piano uses attack & level, not velocity"
  [v]
  (linear-map 0 800 0.0 0.8 v))

(defn- play-chord [a-chord]
  (doseq [note a-chord] (saw2 note)))

(defn play-event [event start-time player-fn]
  (let [pitch-to-play (:pitch event)
        log (:log event)
        attack (when (:velocity event) (velocity-to-attack (:velocity event)))
        level (when (:velocity event)  (velocity-to-level (:velocity event))  1)
        note-time (+ start-time (:time event))]
    (when pitch-to-play
      (if (and attack level)
        (let [current-instrument (at note-time (player-fn :note pitch-to-play :attack attack :level level))]
          (at (+ 1200 note-time) (ctl current-instrument :gate 0)))
        (at note-time (player-fn pitch-to-play)))
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

(defn organ [event start-time]
  (play-event event start-time play-organ))

(defn sawish [event start-time]
  (play-event event start-time play-sawish))

(defn bell [event start-time]
  (play-event event start-time play-bell))

(defn woah [event start-time]
  (play-event event start-time play-woah))

(defn sawnoff [event start-time]
  (play-event event start-time play-sawnoff))

(defn groan [event start-time]
  (play-event event start-time play-groan))

(defn shudder [event start-time]
  (play-event event start-time play-shudder))

(defn guitar [event start-time]
  (let [pitch (:pitch event)
        log (:log event)
        note-time (+ start-time (:time event))
        note-name (find-note-name pitch)]
    (at note-time (stringed/guitar-strum g note-name :down 0.25))))

(defn tb303 [event start-time]
  (play-event event start-time synth/tb303))