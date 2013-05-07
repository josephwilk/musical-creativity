(ns musical-creativity.integration.t-network
  (:require
   [midje.sweet :refer :all]
   [musical-creativity.composers.network :refer :all]))

(fact "it should compose musical events"
  (count (run-neural-net)) => 25)