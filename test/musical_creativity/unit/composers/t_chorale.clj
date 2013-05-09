(ns musical-creativity.unit.composers.t-chorale
  (:require
   [midje.sweet :refer :all]
   [data.chorale :as chorale]
   [data.chorale.jsb1 :refer :all]
   [data.chorale.jsb2 :refer :all]
   [data.chorale.jsb3 :refer :all]
   [data.chorale.jsb4 :refer :all]
   [data.chorale.jsb5 :refer :all]
   [data.chorale.jsb6 :refer :all]
   [data.chorale.jsb7 :refer :all]
   [data.chorale.jsb8 :refer :all]
   [data.chorale.jsb9 :refer :all]
   [data.chorale.jsb10 :refer :all]
   [data.chorale.jsb11 :refer :all]
   [data.chorale.jsb12 :refer :all]
   [data.chorale.jsb13 :refer :all]
   [musical-creativity.composers.chorale :refer :all]))

(fact "find alignemnt in channels"
  (find-alignment-in-all-channels 1000 '(((2 1000) (2 2000) (2 2500)))) => 1000)

(fact "find alignment"
  (find-alignment 1000 '((2 1000) (2 2000) (2 2500) (2 3000) (2 3500) (2 4000))) => true)

(fact "all together"
  (all-together '((1 1000) (1 2000) (1 2500) (1 3000)) '((1000) (2000))) => 1000)

(fact "find triad beginning"
  ;b43500b-14
  (create-complete-database chorale/bach-chorales-in-databases)
  (find-triad-beginning) => nil)

(fact "make name"
  (make-name :b206b 1) => 'b206b-1)


