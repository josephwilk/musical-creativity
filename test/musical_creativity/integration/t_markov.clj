(ns musical-creativity.integration.t-markov
  (:require
    [midje.sweet :refer :all]
    [musical-creativity.composers.markov :as markov]))

(fact "it should collect probabilites for pitches"
  (let [events (markov/compose)]
    (count events) => 51))

;(play (markov/compose 60 100 events))