(ns musical-creativity.composers.t-chorale
  (:require
   [midje.sweet :refer :all]
   [musical-creativity.composers.chorale :refer :all]))

(fact "all together"
  (all-together '((1 1000) (1 2000) (1 2500) (1 3000)) => 1000))
