(ns data.monsters
  (:require [midi-mash.core :refer :all]))

(def events (map (fn [event] [(:time event)
                              (:pitch event)
                              (:velocity event)
                              (:channel event)
                              (:instrument event)]) (csv->events "data/monsters.csv")))

(def monsters-raw (csv->events "data/monsters.csv"))
