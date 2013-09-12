(ns musical-creativity.unit.experiments.analogy
  (:require
   [midje.sweet :refer :all]
   [musical-creativity.experiments.analogy :refer :all]))

(reset! fact-store '((cat*mouse are strong*weak)
                     (strong*weak are opposite*sides)
                     (oil*water are non*mixable)
                     (non*mixable are opposite*sides)))

(fact "lookup"
  (lookup-data 'opposite*sides '((non*mixable are oil*water) (opposite*sides are non*mixable))) => '(opposite*sides are non*mixable))

(fact "infer-for-analogy"

  (infer-for-analogy 'opposite*sides '((strong*weak are cat*mouse)
                                       (opposite*sides are strong*weak)
                                       (non*mixable are oil*water)
                                       (opposite*sides are non*mixable))) =>
                                       '((opposite*sides are strong*weak)
                                         (strong*weak are cat*mouse)))

(fact "derive-all-logical-predecessors"
  (derive-all-logical-predecessors 'opposite*sides) => '(cat*mouse oil*water))


(fact "remove-all"
  (remove-all '((a) (b)) '((a) (b) (a) (c))) => '((c)))

(fact "analogy"
  (analogy 'cat*mouse) => '(cat*mouse is like oil*water))
