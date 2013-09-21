(ns musical-creativity.unit.composers.t-apprentice
  (:require [midje.sweet :refer :all]
            [musical-creativity.composers.apprentice :refer :all]))

(namespace-state-changes (before :facts (reset-state!)))

(defn reset-state! []
  (reset! *words* {})
  (reset! *sentences* {})
  (reset! *no* ())
  (reset! *yes* ())
  (reset! *no-sentences* ())
  (reset! *yes-sentences* ()))

(fact "find-no"
  (find-no '(what is your name?)) => ()
  @*no* => ()

  (find-no '(*what is your name?)) => '(*what)
  @*no* => '(*what))

(fact "find-yes"
  (find-yes '(what is your name?)) => ()
  @*yes* => ()

  (find-yes '($what is your name?)) => '($what)
  @*yes* => '($what))


(fact "recognize-no"
  (recognize-no '(what is your name?)) => nil)

(fact "recognize-yes"
  (recognize-yes '(what is your name?)) => nil)

(fact "reply"
  (put-sentence-into-database '(what is your name?))

  (println :sentence @*sentences*)
  (println :word @*words*)

  (reply '? '(what is your name?)) => ())

(fact "establish-keywords"
  (establish-keywords '(what is your name?))

  @*keyword* => '(name?))

(fact "choose-the-highest-rated-word"
  (choose-the-highest-rated-word
   '((name 1.6) (name? 2.49) (your 1.2) (is 1.6) (what 0.8) (my 1.1))) => 'name?)

(fact "compound-associations"
  (compound-associations '((name? 0.75) (name? 0.2) (is 0.5)))
   => '((name? 0.95) (is 0.5)))

(fact "add weighting"
  (put-sentence-into-database '(what is your name?))

  (add-weighting '(no*) '(*)) =>
  '((no* (name? 0.85) (computer! 0.1) (david! 0.1)
         (name 0.1) (my 0.1) (your 0.1) (is 0.1) (what 0.1))))
