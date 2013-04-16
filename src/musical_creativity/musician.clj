(ns musical-creativity.musician
  (:require
    [overtone.live :refer :all]))

(defn halt []
  (stop))

(defn play [events musician-fn]
  (let [start-time (+ 500 (overtone.live/now))]
    (println)
    (dorun (map #(musician-fn % start-time) events))))
