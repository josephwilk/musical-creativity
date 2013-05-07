(ns musical-creativity.composers.t-network
  (:require
   [midje.sweet :refer :all]
   [musical-creativity.composers.network :refer :all]))

(fact "find largest output"
  (find-the-largest-output @array-8) => 0
  (find-the-largest-output [1 1 0 3 2]) => 3)

