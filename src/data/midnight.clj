(ns data.midnight
  (:require [midi-mash.core :refer :all]))

(def midnight (map (fn [event] [(:time event)
                                (:pitch event)
                                (:velocity event)
                                (:channel event)
                                (:instrument event)]) (csv->events "data/midnight.csv")))

(def midnight-raw (csv->events "data/midnight.csv"))
