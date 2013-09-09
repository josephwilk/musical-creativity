(ns musical-creativity.composers.brainwaves
  (:require [cheshire.core :as json]
            [overtone.music.pitch :as pitch]
            [overtone.live :as live]
            [overtone.inst.sampled-piano :as piano]))

;;Brainwave types:
;; * Delta: deepest stages of sleep
;; * Beta:  normal waking consciousness
;; * Alpha: relaxation and meditation (creativity)
;; * Theta: REM sleep (dreams)
;; * Gamma: hyperalertness, perception, and integration of sensory input

; Example:
;; {gamma_waves 95408, high_beta_waves 205681, beta_waves 293928, mid_gamma_waves 84528, low_alpha_waves 172417,
;; delta_waves 117933, timestamp 1.375811275696894E9, low_beta_waves 382176, low_gamma_waves 106288, alpha_waves 112605, theta_waves 635628, high_alpha_waves 52793, attention 0, meditation 0}

(def brainwave-transmitter "/tmp/brain-dump")

(defn linear-map
  "given points (x0,y0), (x1,y1) calculate linear relation y given x"
  [x0 x1 y0 y1 x]
  (let [dydx (/ (- y1 y0) (- x1 x0))
        dx (- x x0)]
    (+ y0 (* dydx dx))))

(defn ->pentatonic-scale [smallest largest signal]
  (println :signal signal)
  (let [scale (pitch/scale-field :d :pentatonic)]
    (nth scale (linear-map smallest largest 0 (count scale) signal))))

(defn delta-waves->music [wave] {:pitch (->pentatonic-scale 3205 2919511 wave)})
(defn theta-waves->music [wave] (println wave) )

(defn alpha-waves->music [high low wave] {})
(defn beta-waves->music  [high low wave] {})
(defn gamma-waves->music [high low wave] {})

(defn brainwave->music [waves]
  (let [delta-music (delta-waves->music (:delta_waves waves))
        theta-music (theta-waves->music (:theta_waves waves))

        alpha-music (alpha-waves->music (:high_alpha_waves waves) (:low_alpha_waves waves) (:alpha_waves waves))
        beta-music  (beta-waves->music  (:high_beta_waves waves) (:low_beta_waves waves) (:beta_waves waves))
        gamma-music (gamma-waves->music (:high_gamma_waves waves) (:low_gamma_waves waves) (:gamma_waves waves))
        music (merge alpha-music delta-music theta-music gamma-music)]

    (piano/sampled-piano (:pitch music))
    (println music)))

(defn listen-for-brainwaves []
  (with-open [reader (clojure.java.io/reader brainwave-transmitter)]
    (brainwave->music (json/decode (first (line-seq reader)) true))))

(defn compose [] (while true (listen-for-brainwaves)))