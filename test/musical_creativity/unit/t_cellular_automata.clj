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
    
(fact "it should create the row"
  (cellular-automata/create-the-row start rules 2) => [0 0 0 0 0 0 0 0 0 0 0 * * * 0 0 0 0 0 0 0 0 0 0 0])