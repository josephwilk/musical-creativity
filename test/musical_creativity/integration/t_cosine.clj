(ns musical-creativity.integration.t-cosine
  (:require
    [midje.sweet :refer :all]
    [musical-creativity.composers.cosine :as cosine]))

(fact "it should generate events"
  (let [events (cosine/compose)
        all-keys (reduce concat [] (map keys events))]
    (vec (distinct all-keys)) => [:time :log :pitch :channel]))