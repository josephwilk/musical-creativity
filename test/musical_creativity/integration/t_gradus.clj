(ns musical-creativity.integration.t-gradus
  (:require
   [midje.sweet :refer :all]
   [musical-creativity.composers.gradus :refer :all]))

(fact "it should compose music"
  (gradus @*auto-goals* true))