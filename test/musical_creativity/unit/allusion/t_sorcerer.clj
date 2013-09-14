(ns musical-creativity.unit.allusion.t-sorcerer
 (:require
  [midje.sweet :refer :all]
  [musical-creativity.allusion.sorcerer :refer :all]))

(fact "count-xs" (count-xs '(:x :x 5 -1 1)) => 2)

(fact "add-timing"
  (add-timing 1000 '((0 77 500 1 64) (500 82 1500 1 64) (2000 81 250 1 64)
                     (2250 82 250 1 64) (2500 84 500 1 64)
                     (3000 82 500 1 64))) =>
                     '((1000 77 500 1 64) (1500 82 1500 1 64)
                       (3000 81 250 1 64) (3250 82 250 1 64)
                       (3500 84 500 1 64) (4000 82 500 1 64)))

(fact "match"
  (match 5 5) => true)

(fact "find the patterns"
  (find-the-patterns
   '((0 71 500 1 64) (500 74 500 1 64) (1000 78 500 1 64) (1500 83 1000 1 64) (2500 81 500 1 64) (3000 83 1000 1 64))
   '(((0 77 500 1 64) (500 82 1500 1 64) (2000 81 250 1 64) (2250 82 250 1 64) (2500 84 500 1 64) (3000 82 500 1 64)))
   '(beethoven-2))=>
   '(((:x :x 5 -1 1) beethoven-2)))

(fact "pattern match"
  (pattern-match '(3 4 5 -2 2) '(5 -1 1 2 -2)) => '(:x :x 5 -1 1))