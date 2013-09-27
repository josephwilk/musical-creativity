(ns musical-creativity.unit.composers.t-apprentice
  (:require [midje.sweet :refer :all]
            [musical-creativity.composers.apprentice :refer :all]))

(namespace-state-changes (before :facts (reset-state!)))

(defn reset-state! []
  (reset! *all-words* ())
  (reset! *words* {})
  (reset! *sentences* {})
  (reset! *no* ())
  (reset! *yes* ())
  (reset! *no-sentences* ())
  (reset! *yes-sentences* ())
  (reset! *keyword* ())
  (reset! *keywords* ())
  (reset! *last-word-weight* 0.2)
  (reset! *counter* 0)

  (reset! *last-word* nil)
  (reset! *successor* nil))

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

  ;;(println :sentence @*sentences*)
  ;;(println :word @*words*)

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

(fact "get-keyword"
  (get-keyword '(hello!)) =>  'hello!
  (get-keyword '(what is your name?)) => 'name?
  (get-keyword '(who am i speaking to?)) => 'speaking)

(fact "make-weight-list"
  (make-weight-list 'name? 0.75) => '((name? 0.75)))

(fact "add-weighting"
  (put-sentence-into-database '(what is your name?)))

(fact "build-associations"
  (put-sentence-into-database '(what is your name?))

  (build-associations 'what) => '([name? 0.4]
                                    [is 0.6]
                                      [your .1]))

(fact "reduce-weight"
  (put-sentence-into-database '(what is your name?))
  (put-sentence-into-database '(my name is david!))
  (put-sentence-into-database '(your name is computer!))

  (reduce-weight 'what '(computer!)) =>
  '((computer! 0.81) (name? 5.04) (is 2.7) (david! 1.62) (name 0.9) (my 0.5) (your 1.3)))

(fact "punish"
  (punish '((name? 2.47) (is 1.0) (david! 0.1) (name 0.1) (my 0.1) (your 0.4)) '(david!))
  => '((david! 0.05) (name? 2.47) (is 1.0) (name 0.1) (my 0.1) (your 0.4)))

(fact "reward"
  (reward '((david! 2.47) (name 0.9) (up? 0.1) (is 0.5) (what 0.1) (hello! 0.1) (yes$ 0.1))
          '(what is up?))
  =>
  '((up? 0.2) (is 1.0) (what 0.2) (david! 2.47) (name 0.9) (hello! 0.1) (yes$ 0.1)))

(fact "compare-words"
  (compare-words 'a 'a) => true)

(fact "remove-object-twice"
  (remove-object-twice 'name? '((name? 0.75) (name? 0.2) (is 0.5)))
   => '((is 0.5)))

(fact "sentence-type"
  (get-sentence-type '(what is your name?)) => "?")

(fact "define-incipients"
  (define-incipients '(my name is david!) '!) => '(my))

(fact "define-cadences"
  (define-cadences '(my name is david!) '!) => '(david!))