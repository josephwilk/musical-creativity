(ns musical-creativity.integration.t-gradus
  (:require
   [midje.sweet :refer :all]
   [musical-creativity.composers.gradus :refer :all]))

(fact "it should generate pitch events"
  (count (compose)) => 20)

(fact "it should compose a canon"
  (count (compose-canon)) => 98)