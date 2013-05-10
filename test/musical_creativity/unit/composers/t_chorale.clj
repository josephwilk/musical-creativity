(ns musical-creativity.unit.composers.t-chorale
  (:require
   [midje.sweet :refer :all]
   [data.chorale :as chorale]
   [data.chorale.jsb1 :refer :all]
   [data.chorale.jsb2 :refer :all]
   [data.chorale.jsb3 :refer :all]
   [data.chorale.jsb4 :refer :all]
   [data.chorale.jsb5 :refer :all]
   [data.chorale.jsb6 :refer :all]
   [data.chorale.jsb7 :refer :all]
   [data.chorale.jsb8 :refer :all]
   [data.chorale.jsb9 :refer :all]
   [data.chorale.jsb10 :refer :all]
   [data.chorale.jsb11 :refer :all]
   [data.chorale.jsb12 :refer :all]
   [data.chorale.jsb13 :refer :all]
   [musical-creativity.composers.chorale :refer :all]))

(namespace-state-changes (before :facts (do
                                          (reset! *beats-store* {})
                                          (reset! *lexicon-store* {})
                                          (reset! bach-start-beats [])
                                          (reset! bach-rules [])
                                          (reset! bach-compose-beats []))))

(fact "find alignemnt in channels"
  (find-alignment-in-all-channels 1000 '(((2 1000) (2 2000) (2 2500)))) => 1000)

(fact "find alignment"
  (find-alignment 1000 '((2 1000) (2 2000) (2 2500) (2 3000) (2 3500) (2 4000))) => true)

(fact "all together"
  (all-together '((1 1000) (1 2000) (1 2500) (1 3000)) '((1000) (2000))) => 1000)

(fact "triad?"
  (triad? '((111000 40 500 4 96) (111000 55 500 3 96) (111000 64 1000 2 96) (111000 72 1000 1 96))) => true)

(fact "find triad beginning"
  (create-complete-database '(b43500b))
  (find-triad-beginning) => 'b43500b-14)

(fact "make name"
  (make-name :b206b 1) => 'b206b-1)

(fact "put beat into lexicon"
  (reset! *beats-store* {'b206b-1 {:start-notes [57 60 69 76]}})

  (put-beat-into-lexicon 'b206b-1) => :bach-57-60-69-76

  @*lexicon-store* => {:bach-57-60-69-76 {:beats '(b206b-1)}})


(fact "create complete database"
  (create-complete-database ['b206b]) => true

  (keys ('b206b-1 @*beats-store*)) => '(:start-notes :destination-notes :events :voice-leading :speac)
  (count (:events  ('b206b-1 @*beats-store*))) => 4)

(fact "collect beats"
  (first (collect-beats b206b)) => '([0 57 1000 4 96] [0 60 1000 3 96] [0 69 1000 2 96] [0 76 1000 1 96])
  (second (collect-beats b206b)) => '([1000 59 1000 4 96] [1000 62 1000 3 96] [1000 67 1000 2 96] [1000 79 1000 1 96]))

(fact "collect by timing"
  (collect-by-timing 1000 '((0 57 1000 4 96) (1000 60 1000 3 96))) => '((0 57 1000 4 96)))

(fact "first place where all together"
  (first-place-where-all-together '((0 57 1000 4 96) (0 60 1000 3 96) (0 69 1000 2 96))) => 1000)


(fact "get onset notes from events"
  (get-onset-notes '((0 57 1000 4 96) (0 60 1000 3 96) (0 69 1000 2 96) (0 76 1000 1 96)))
  => '(57 60 69 76))

(fact "get smallest set"
  (get-smallest-set '(0 4 7)) => '(0 4 7))

(fact "get on beat"
  (get-on-beat '((53000 46 1000 4 96) (53000 53 500 3 96) (53000 62 500 2 96) (53000 62 1000 1 96) (53500 52 250 3 96) (53500 55 500 2 96) (53750 50 250 3 96)) 53000)
  => '((53000 46 1000 4 96) (53000 53 500 3 96) (53000 62 500 2 96) (53000 62 1000 1 96)))

(fact "create pitch class set"
  (create-pitch-class-set '(64 67 71)) => '(4 7 11))

(fact "set to zero"
  (set-to-zero '((31000 60 1000 4 96) (31000 67 1000 3 96) (31000 72 1000 2 96) (31000 76 1000 1 96)))
  => '((0 60 1000 4 96) (0 67 1000 3 96) (0 72 1000 2 96) (0 76 1000 1 96)))

(fact "get rules"
  (get-rules '(57 60 69 76) '(59 62 67 79) 'b206b-1)
  => '((3 2 2 b206b-1) (12 2 -2 b206b-1) (7 2 3 b206b-1) (9 2 -2 b206b-1) (4 2 3 b206b-1) (7 -2 3 b206b-1)))

(fact "get last beat events"
  (get-last-beat-events '((18000 48 1000 4 96) (11000 64 1000 3 96) (19000 67 1000 2 96) (18000 72 1000 1 96)))
  => '((18000 48 1000 4 96) (18000 64 1000 3 96) (18000 67 1000 2 96) (18000 72 1000 1 96)))

(fact "get re-time"
  (re-time '(((15000 60 1000 4 96) (15000 64 1000 3 96)))) => '(((0 60 1000 4 96) (0 64 1000 3 96))))

(fact "create pc set"
  (create-pc-set '(64 67 71)) => '(4 7 11))
