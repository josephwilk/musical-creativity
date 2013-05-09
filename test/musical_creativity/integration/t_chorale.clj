(ns musical-creativity.integation.t-chorale
  (:require
   [midje.sweet :refer :all]
   [musical-creativity.composers.chorale :refer :all]))

(fact "it composes"
  (count (compose)) => 20)