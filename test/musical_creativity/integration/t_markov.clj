(ns musical-creativity.integration.t-markov
  (:require
    [midje.sweet :refer :all]
    [musical-creativity.composers.markov :as markov]))

(fact "it should collect probabilites for pitches"
  (let [events (markov/compose markov/default-events [60] 50 1)]
    (count events) => 51))

(future-fact "it should support different depths"
  (let [events (markov/compose markov/default-events [71 72] 50 2)]
    (count events) => 51))

;(play (markov/compose 60 100 events))