(ns musical-creativity.unit.composers.t-fuzzy
  (:require
   [midje.sweet :refer :all]
   [musical-creativity.composers.fuzzy :as fuzzy]))

(facts "about make-set"
  (fact "when empty"
    (fuzzy/make-set '()) => [0 0 0 0 0 0 0 0 0 0 0 0])
  (fact "when there are pitches"
    (fuzzy/make-set '(7 11 2)) => [0 0 1 0 0 0 0 1 0 0 0 1]))

(fact "fz-union"
  (fuzzy/fz-union '(1 0 0 0 0 0 0 0 0 0 0 0) '(0 0 0 0 0 0 0 0 0 0 0 0)) => [1 0 0 0 0 0 0 0 0 0 0 0])

(fact "top-n-positions"
  (fuzzy/top-n-positions [1 2 3 4 5 6] 3) => [5 4 3])