(ns musical-creativity.unit.composers.t-markov
  (:require
    [midje.sweet :refer :all]
    [musical-creativity.composers.markov :as markov]))

(facts "put-probabilities"
  (fact "it should collect probablites"
    (markov/probabilities-for {} [60 61]) => {60 [61]}))