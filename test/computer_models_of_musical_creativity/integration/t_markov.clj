(ns computer-models-of-musical-creativity.integration.t-markov
  (:require
    [midje.sweet :refer :all]
    [computer-models-of-musical-creativity.markov :as markov]))
    
(fact "it should collect probabilites for pitches"
  (markov/compose-new-music-based-on-markovian-probabilities 60 5 markov/events) => {})


;(play (compose-new-music-based-on-markovian-probabilities 60 100 events))
  