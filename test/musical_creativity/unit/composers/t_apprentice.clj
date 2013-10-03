(ns musical-creativity.unit.composers.t-apprentice
  (:require [midje.sweet :refer :all]
            [musical-creativity.composers.apprentice :refer :all]))

(namespace-state-changes (before :facts (reset-all!)))

(fact "contains no?"
  (contains-no? '(what is your name?)) => nil
  @*no* => ()

  (contains-no? '(*what is your name?)) => '(*what)
  @*no* => '(*what)

  (contains-no? '(no*)) => '(no*))

(fact "contains yes?"
  (contains-yes? '(what is your name?)) => nil

  (contains-yes? '($what is your name?)) => '($what)
  @*yes* => '($what))

(fact "contains no"
  (contains-no? '(what is your name?)) => nil)

(fact "recognize-yes"
  (contains-yes? '(what is your name?)) => nil)

(fact "put-sentence-into-database"
  (put-sentence-into-database '(hello!))
  (put-sentence-into-database '(what is your name?))

  (:associations ('name?  @*words-store*)) => '((your 0.2) (is 0.2) (what 0.2) (hello! 0.1))
  (:associations ('your   @*words-store*)) => '((name? 2.21) (is 0.3) (what 0.3) (hello! 0.1))
  (:associations ('is     @*words-store*)) => '((name? 2.09) (your 0.8) (what 0.4) (hello! 0.1))
  (:associations ('what   @*words-store*)) => '((name? 2.47) (is 0.9) (hello! 0.1) (your 0.4))
  (:associations ('hello! @*words-store*)) => '((what 0.4) (is 0.4) (your 0.4) (name? 1.52)))

(fact "default-reply-thing"
  (put-sentence-into-database '(hello!))
  (build-a-response "!" '(hello!)) => '()

  (put-sentence-into-database '(what is your name?))
  (count (build-a-response "?" '(what is your name?))) => #(> % 1))

(fact "reply"
  (put-sentence-into-database '(hello!))
  (reply "!" '(hello!)) => ()

  (put-sentence-into-database '(what is your name?))
  (reply "?" '(what is your name?)) => '(your what is hello!))

(fact "establish-keywords"
  (establish-keywords '(what is your name?)) => {:keyword 'name? :last-word 'name?})

(fact "choose-the-highest-rated-word"
  (choose-the-highest-rated-word '((is 1.4) (hello! 0.4) (your 1.4) (what 0.9)))
   => 'your)

(fact "compound-associations"
  (compound-associations '((name? 0.75) (name? 0.2) (is 0.5)))
   => '((name? 0.95) (is 0.5)))

(fact "add weighting"
  (put-sentence-into-database '(hello!))
  (put-sentence-into-database '(what is your name?))
  (put-sentence-into-database '(my name is david!))
  (put-sentence-into-database '(your name is computer!))
  (put-sentence-into-database '(what is your name?))
  (put-sentence-into-database '(what is my name?))

  (add-weighting '(no*) '("*")) =>
  '((no* (name? 0.85) (computer! 0.1) (david! 0.1)
         (name 0.1) (my 0.1) (your 0.1) (is 0.1) (what 0.1))))

(fact "add word to word weightlists"
  (put-sentence-into-database '(what is your name?))

  (add-word-to-word-weightlists 'what 'what 'is (all-words)) => nil)

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

  (build-associations 'what {:keyword 'name? :last-word 'name? :successor 'is} (all-words)) => '((name? 1.05) (is 0.6) (your 0.1))

  (build-associations 'what {:keyword 'name? :last-word 'name? :successor 'is} (all-words) '((name 0.5))) => '([name? 1.05] [is 0.6] [your 0.1]  [name 0.5]))

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
  =>  '((up? 0.2) (is 1.0) (what 0.2) (david! 2.47) (name 0.9) (hello! 0.1) (yes$ 0.1)))

(fact "remove-object-twice"
  (remove-object-twice 'name? '((name? 0.75) (name? 0.2) (is 0.5)))
   => '((is 0.5)))

(fact "sentence-type"
  (get-sentence-type '(what is your name?)) => "?")

(fact "define-incipients"
  (define-incipients '(my name is david!) '!) => '(my))

(fact "define-cadences"
  (define-cadences '(my name is david!) '!) => '(david!))

(fact "remove-all-ffirst"
  (remove-all-by-ffirst '(name?) '((name? 6.77) (is 1.4) (hello! 0.4) (your 1.4) (what 0.9))) =>
  '((is 1.4) (hello! 0.4) (your 1.4) (what 0.9)))

(fact "remove-all-by-ffirst"
  (remove-by-ffirst 'name? '((name? 6.77) (is 1.4) (hello! 0.4) (your 1.4) (what 0.9))) =>
    '((is 1.4) (hello! 0.4) (your 1.4) (what 0.9)))

(fact "round it"
  (round-it 0.95) => (roughly 0.95))

(fact "fix-end-of-music-sentences"
  (fix-end-of-music-sentences '(improv[5]-75-66-70 "!")))