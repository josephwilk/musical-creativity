(ns data.one-summers-day
  (:require [midi-mash.core :refer :all]))

(def one-summers-day (map (fn [event] [(:time event)
                                       (:pitch event)
                                       (:velocity event)
                                       (:channel event)
                                       (:instrument event)]) (csv->events "data/one_summers_day.csv")))

