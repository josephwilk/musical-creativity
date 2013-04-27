(ns musical-creativity.integration.composers.t-gradus
  (:require
   [midje.sweet :refer :all]
   [musical-creativity.composers.gradus :refer :all]))

(fact "it should compose music"
  (compose))