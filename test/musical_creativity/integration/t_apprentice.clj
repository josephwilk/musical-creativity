(ns musical-creativity.integration.t-apprentice
  (:require
   [midje.sweet :refer :all]
   [musical-creativity.composers.apprentice :as apprentice]
   [musical-creativity.composers.apprentice.music :as music]))

(namespace-state-changes (before :facts (apprentice/reset-all!)))


(future-fact "apprentice"
  (apprentice/apprentice))

(fact "musical-apprentice"
  (music/preload!)

  (apprentice/apprentice-reply '(bach-1-1!)))