(ns musical-creativity.integration.t-sonify
  (:require
    [midje.sweet :refer :all]
    [musical-creativity.composers.sonify :as sonify]))

(facts "it should generate events"
  (let [events (sonify/compose)
        all-keys (reduce concat [] (map keys events))]
     (distinct all-keys) => [:time :pitch]))