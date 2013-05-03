(ns musical-creativity.integration.t-sonify-data
  (:require
    [midje.sweet :refer :all]
    [musical-creativity.composers.sonify-data :as sonify]))

(facts "it should generate events"
  (let [events (sonify/compose)
        all-keys (reduce concat [] (map keys events))]
     (distinct all-keys) => '(:time :log :pitch :channel)
     ))