(ns musical-creativity.integration.t-improvise
  (:require
   [midje.sweet :refer :all]
   [musical-creativity.composers.improvise :refer :all]))

(fact "compose"
  (count (compose)) => #(> % 0))
