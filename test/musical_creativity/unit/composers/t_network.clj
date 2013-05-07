(ns musical-creativity.composers.t-network
  (:require
   [midje.sweet :refer :all]
   [musical-creativity.composers.network :refer :all]))

(fact "find largest output"
  (find-the-largest-output) => 2)