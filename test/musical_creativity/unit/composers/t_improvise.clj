(ns musical-creativity.unit.composers.t-improvise
  (:require
   [midje.sweet :refer :all]
   [musical-creativity.composers.improvise :refer :all]
   [data.forgray :refer :all]))

(fact "creating a database"
  (create-a-complete-database '(forgray))

  (first (:grouping-names (@*lexicon-store* "lexicon-87-41-41"))) => #"forgray\[\d+\]"
  (@*groupings-store* "forgray[2]-97-99-41") => {:destination "forgray[2]-41-101-103",
                                                 :events '((59000 97 99 1 71) (59000 99 99 1 109) (59000 41 99 1 93 tie)),
                                                 :lexicon "lexicon-97-99-41",
                                                 :name 'forgray
                                                 :timing '(59000 59099)})

(fact "making lexicon name"
  (make-name-of-lexicon '(63 59 37)) => "lexicon-63-59-37")

(fact "removing ends"
  (create-a-complete-database '(forgray))

  ;(reset! *lexicon-store* {})

  (remove-ends '(lexicon-77-36 lexicon-77-36-74 lexicon-74-70-34)) =>
  '(lexicon-77-36 lexicon-77-36-74 lexicon-74-70-34))

(fact "choose begining grouping"
  (choose-beginning-grouping '("fourbros [110]-62-65-68-44-41-51")) => "fourbros [110]-62-65-68-44-41-51")