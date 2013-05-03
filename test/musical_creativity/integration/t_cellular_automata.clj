(ns musical-creativity.integration.t-cellular-automata
  (:require
    [midje.sweet :refer :all]
    [musical-creativity.composers.cellular-automata :as cellular-automata]))

(def rules
  [[["*" "*" "*"] "0"]
   [["*" "*" "0"] "*"]
   [["*" "0" "*"] "0"]
   [["*" "0" "0"] "*"]
   [["0" "*" "*"] "*"]
   [["0" "*" "0"] "*"]
   [["0" "0" "*"] "0"]
   [["0" "0" "0"] "0"]])

(def start
  ["0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "*" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0"])

(fact "it should generate events"
  (let [events (cellular-automata/compose rules)]
    (count events) => 151))