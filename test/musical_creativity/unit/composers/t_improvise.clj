(ns musical-creativity.unit.composers.t-improvise
  (:require
   [midje.sweet :refer :all]
   [musical-creativity.composers.improvise :refer :all]
   [data.forgray :refer :all]))

(fact "creating a database"
  (create-a-complete-database '(forgray))
  (first (:grouping-names  (@*lexicon-store* "lexicon-87-41-41"))) => #"forgray\[\d+\]")

(fact "making lexicon name"
  (make-name-of-lexicon '(63 59 37)) => "lexicon-63-59-37")