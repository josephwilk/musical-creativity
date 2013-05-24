(ns musical-creativity.unit.composers.t-network
  (:require
   [midje.sweet :refer :all]
   [musical-creativity.composers.network :refer :all]))

(namespace-state-changes (before :facts (reset-all! 5 5)))

(fact "initialize-the-network"
  (initialize-the-network) => nil)

(fact "learn the patterns"
  (learn-the-patterns [[0 0 0 0 1]] 50))

(fact "find largest output"
  (find-the-largest-output @output-array [false false false false false]) => 0
  (find-the-largest-output [1 1 0 0 3 2] [false false false false false]) => 4)

(fact "update f1 stm arrays"
  (update-f1-stm-arrays) => nil)

(fact "check for f2 reset"
  (check-for-f2-reset) => false)

(fact "zero activations"
  (zero-activations) => nil)

(fact "competitive learning at f2"
  (competitive-learning-at-f2) => nil)

(fact "update f2 stm storage"
  (update-f2-stm-storage))

(fact "update weights"
  (update-weights) => nil)

(fact "find all"
  (find-all 1 '(((0.0 0.2 0.1 0.25 0.35) 4) ((0.0 0.1 0.2 0.25 0.35) 1)  ((0.2 0.1 0.2 0.25 0.45) 1)))  =>
  '(((0.0 0.1 0.2 0.25 0.35) 1) ((0.2 0.1 0.2 0.25 0.45) 1)))

(fact "count highest"
  (count-highest '(((0.0 0.2 0.1 0.25 0.35) 4)
                  ((0.0 0.1 0.2 0.25 0.35) 1)
                  ((0.0 0.2 0.25 0.35 0.4) 1)))
  =>  '(((0.0 0.1 0.2 0.25 0.35) 1) ((0.0 0.2 0.25 0.35 0.4) 1)))

(fact "check array value"
  (check-layer-value 0 (double-array 4 1.0) (boolean-array 4 false)) => d)

(fact "l2 norm of a vector"
  (l2-norm-of-a-vector (double-array 5 0.0)) => 0.001
  (l2-norm-of-a-vector (double-array 5 1.0)) => 2.2370679774997897
  (l2-norm-of-a-vector (double-array 5 0.2)) => 0.448213595499958)

(fact "sigmoid-threshold-function"
  (sigmoid-threshold-function 0.6302290114489392) =>  0.6302290114489392)

(fact "run-one-full-cycle"
  (run-one-full-cycle) => nil)

(fact "set learning pattern"
  (count (set-learning-pattern '(0.0 0.2 0.1 0.25 0.35))) => 5)
