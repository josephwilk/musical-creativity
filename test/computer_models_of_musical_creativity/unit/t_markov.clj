(ns computer-models-of-musical-creativity.unit.t-markov
  (:require
    [midje.sweet :refer :all]
    [computer-models-of-musical-creativity.markov :as markov]))
    
(facts "put-probabilities"
  (fact "it should collect probablites"
    (markov/probabilities-for [60 61] {60 []}) => {60 [61]}))