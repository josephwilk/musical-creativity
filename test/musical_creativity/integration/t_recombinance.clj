(ns musical-creativity.integration.t-recombinance
  (:require
   [midje.sweet                               :refer :all]
   [data.bach                                 :as bach]
   [musical-creativity.composers.recombinance :refer :all]))

(fact "it composes"
  (create-database-from bach/chorale-140-data)
  (count (compose)) =not=> zero?)
