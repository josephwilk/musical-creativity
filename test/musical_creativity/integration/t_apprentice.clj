(ns musical-creativity.integration.t-apprentice
  (:require
   [midje.sweet :refer :all]
   [musical-creativity.composers.apprentice :as apprentice]))

(fact "apprentice"
  (apprentice/apprentice))