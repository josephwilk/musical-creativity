(ns musical-creativity.unit.composers.t-cellular-automata
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
  ["0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "*" "*" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0"])

(facts "create-the-row"
  (fact "it should create a new row based on apply rules to start state"
    (cellular-automata/create-the-row start rules 2) => ["0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "*" "*" "*" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0"]))

(facts "apply-the-rule"
  (fact "it should apply the rule"
    (cellular-automata/apply-rule ["0" "0" "*"] rules 11 1) => "0"
    (cellular-automata/apply-rule ["0" "*" "*"] rules 11 1) => "*"))
