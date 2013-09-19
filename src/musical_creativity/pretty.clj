(ns musical-creativity.pretty
  (:require [quil.core :refer :all]))

(def cells (atom ["0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "*" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0"]))

(defn setup []
  (smooth)
  (frame-rate 1)
  (background 200))

(defn draw []
  (stroke 255)
  (stroke-weight 2)

  (doall
   (map (fn [[cell index]]
          (let [c (if (= cell "*") 0 255)
                diam 15
                x    (* (inc index) 21)
                y    20]
            (fill c)
            (ellipse x y diam diam)))
        (map vector @cells (range)))))

(defsketch pretty-music
  :title "Cellullar Automata"
  :setup setup
  :draw draw
  :size [580 50])
