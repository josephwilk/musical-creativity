(ns musical-creativity.unit.composers.t-apprentice
  (:require [midje.sweet :refer :all]
            [musical-creativity.composers.apprentice :refer :all]))

(fact "find-no"
  (find-no '(what is your name?)) => ())

(fact "find-yes"
  (find-yes '(what is your name?)) => ())

(fact "reply"
  (put-sentence-into-database '(what is your name?))

  (println @*sentences*)
  (println @*words*)

  (reply '? '(what is your name?)) => ())