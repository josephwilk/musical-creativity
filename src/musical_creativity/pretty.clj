(ns musical-creativity.pretty
  (:require [quil.core :as quil]))

(defn setup []
  (smooth)
  (frame-rate 1)
  (background 200))

(defn draw []
  (stroke (random 255))
  (stroke-weight (random 10))
  (fill (random 255))

  (let [diam (random 100)
        x    (random (width))
        y    (random (height))]
    (ellipse x y diam diam)))

(defsketch pretty-music
  :title "Musical creativity"
  :setup setup
  :draw draw
  :size [323 200])
