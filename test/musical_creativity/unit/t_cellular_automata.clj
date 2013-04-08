(ns musical-creativity.unit.t-cellular-automata
  (:require
    [midje.sweet :refer :all]
    [musical-creativity.cellular-automata :as cellular-automata]))

(def rules
  [[[* * *] 0]
   [[* * 0] *]
   [[* 0 *] 0]
   [[* 0 0] *]
   [[0 * *] *]
   [[0 * 0] *]
   [[0 0 *] 0]
   [[0 0 0] 0]])

(def start
  [0 0 0 0 0 0 0 0 0 0 0 0 * * 0 0 0 0 0 0 0 0 0 0 0])

(facts "create-the-row"
  (fact "it should create a new row based on apply rules to start state"
    (cellular-automata/create-the-row start rules 2) => [0 0 0 0 0 0 0 0 0 0 0 * * * 0 0 0 0 0 0 0 0 0 0 0]))

(facts "matching-rule"
  (fact "it should return a matching rule"
    (cellular-automata/matching-rule-result [0 * 0] rules) => [[0 * 0] *]))