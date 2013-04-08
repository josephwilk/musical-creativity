(ns -musical-creativity.integration.t-markov
  (:require
    [midje.sweet :refer :all]
    [musical-creativity.markov :as markov]))

(fact "it should collect probabilites for pitches"
  (markov/compose 60 5 markov/events) => {})

;(play (markov/compose 60 100 events))
