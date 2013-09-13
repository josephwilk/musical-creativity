(ns musical-creativity.integration.t-sorcerer
  (:require
   [midje.sweet :refer :all]
   [musical-creativity.allusion.sorcerer :refer :all]))

(fact "allude"
  (sorcerer 'bach-1 '(beet-2)) => '((0 71 500 1 64)
                                    (500 74 500 1 64)
                                    (1000 78 500 1 64)
                                    (1000 77 500 0 64)
                                    (1500 83 1000 1 64)
                                    (1500 82 1500 0 64)
                                    (2500 81 500 1 64)
                                    (3000 83 1000 1 64)
                                    (3000 81 250 0 64)
                                    (3250 82 250 0 64)
                                    (3500 84 500 0 64)
                                    (4000 82 500 0 64)))
