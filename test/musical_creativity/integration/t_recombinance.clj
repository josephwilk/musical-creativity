(ns musical-creativity.integration.t-recombinance
  (:require
   [midje.sweet                               :refer :all]
   [musical-creativity.composers.recombinance :refer :all]))

(fact "it composes"
  (count (compose)) => 197)