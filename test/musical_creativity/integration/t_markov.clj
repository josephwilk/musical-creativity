(ns musical-creativity.integration.t-markov
  (:require
    [midje.sweet :refer :all]
    [musical-creativity.composers.markov :as markov]))

(fact "it should collect probabilites for pitches"
  (let [events (markov/compose markov/default-events [60] 1 50)]
    (count events) => 50))

(fact "it should support different depths"
  (let [events (markov/compose markov/default-events [60 62] 2 50)]
    (count events) => 9))
