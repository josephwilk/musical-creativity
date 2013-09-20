(ns musical-creativity.unit.composers.t-apprentice
  (:require [midje.sweet :refer :all]
            [musical-creativity.composers.apprentice :refer :all]))

(fact "find-no"
  (find-no '(what is your name?)) => ())

(fact "find-yes"
  (find-yes '(what is your name?)) => ())

(fact "reply"
  (put-sentence-into-database '(what is your name?))

  (println :sentence @*sentences*)
  (println :word @*words*)

  (reply '? '(what is your name?)) => ())

(fact "establish-keywords"
  (put-sentence-into-database '(what is your name?))
  (establish-keywords '(what is your name?))  => '(name?))

(fact "choose-the-highest-rated-word"
  (choose-the-highest-rated-word
   '((name 1.6) (name? 2.49) (your 1.2) (is 1.6) (what 0.8) (my 1.1))) => 'name?)

(fact "compound-associations"
  (compound-associations '((name? 0.75) (name? 0.2) (is 0.5)))
   => '((name? 0.95) (is 0.5)))
