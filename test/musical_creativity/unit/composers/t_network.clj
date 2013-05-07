(ns musical-creativity.unit.composers.t-network
  (:require
   [midje.sweet :refer :all]
   [musical-creativity.composers.network :refer :all]))

(fact "find largest output"
  (find-the-largest-output @array-8 [false false false false false]) => 0
  (find-the-largest-output [1 1 0 3 2] [false false false false false]) => 3)

(fact "update f1 stm arrays"
  (update-f1-stm-arrays) => nil)

(fact "check for f2 reset"
  (check-for-f2-reset) => nil)

(fact "competitive learning at f2"
  (competitive-learning-at-f2) => nil)

(fact "update f2 stm storage"
  (update-f2-stm-storage))

(fact "find all"
  (find-all 1 '(((0.0 0.2 0.1 0.25 0.35) 4) ((0.0 0.1 0.2 0.25 0.35) 1)  ((0.2 0.1 0.2 0.25 0.45) 1)))  =>
  '(((0.0 0.1 0.2 0.25 0.35) 1) ((0.2 0.1 0.2 0.25 0.45) 1)))

(fact "count highest"
  (count-highest '(((0.0 0.2 0.1 0.25 0.35) 4)
                  ((0.0 0.1 0.2 0.25 0.35) 1)
                  ((0.0 0.2 0.25 0.35 0.4) 1)))
  =>  '(((0.0 0.1 0.2 0.25 0.35) 1) ((0.0 0.2 0.25 0.35 0.4) 1)))