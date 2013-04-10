(ns musical-creativity.integration.t-markov
  (:require
    [midje.sweet :refer :all]
    [musical-creativity.composers.markov :as markov]))

(fact "it should collect probabilites for pitches"
  (let [events (markov/compose {:pitch 60 :length 5 :events markov/default-events})]
    events => (contains {:pitch 60, :time 0})
    (count events) => 5))

;(play (markov/compose 60 100 events))