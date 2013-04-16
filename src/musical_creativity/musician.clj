(ns musical-creativity.musician
  (:require
    [overtone.live :refer :all]))

(defn play [events musician-fn]
  (let [start-time (overtone.live/now)]
    (dorun (map #(musician-fn % start-time) events))))
