(ns musical-creativity.integration.t-cellular-automata
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
  [[0 0 0 0 0 0 0 0 0 0 0 0 * 0 0 0 0 0 0 0 0 0 0 0 0]])


(fact "it should collect probabilites for pitches"
  (let [events (cellular-automata/create-lists 20 start rules)]
    events => (contains {:pitch 60, :time 0})))