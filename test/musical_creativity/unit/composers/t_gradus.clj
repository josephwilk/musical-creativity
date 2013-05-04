(ns musical-creativity.unit.composers.t-gradus
  (:require
   [midje.sweet :refer :all]
   [musical-creativity.composers.gradus :refer :all]
   [overtone.music.pitch :as music]))

(defn pitch-as-note-checker [note-name]
  (chatty-checker [pitch]
                  (= pitch (music/note note-name))))

(defchecker pitch-as-note [note-name]
  (pitch-as-note-checker note-name))

(fact "find scale intervals"
  (find-scale-intervals
   [69 76]
   [36 38 40 41 43 45 47 48 50 52 53 55 57 59
    60 62 64 65 67 69 71 72 74 76 77 79 81 83
    84 86 88 89 91 93 95 96]) => [4])

(fact "find the legals"
  (find-the-legals
   '((69 57) (71 55) (72 57) (74 53) (71 55) (72 53) (74 50) (72 52))) =>
   '((12 16) (16 15) (15 21) (21 16) (16 19) (19 24) (24 20)))

(fact "remove illegal verticals"
  (remove-illegal-verticals
   [0 1 2 5 6 10 11 13 14 17 18 22 23 25 26 29 30 34 35 37 38]
   [[24 24] [24 23] [24 22] [24 21] [24 20]]) =>
   [[24 24] [24 21] [24 20]])

(fact "find all possible motions"
  (take 11 (find-all-possible-motions 24))  =>
  [[24 24] [24 23] [24 22] [24 21] [24 20]
   [24 19] [24 18] [24 17] [24 16] [24 15]
   [24 14]])

(fact "pair"
  (pair '((1 2 3) (4 5 6)))
  => '((1 4) (2 5) (3 6)))

(fact "combinations"
  (combinations 3 '(3 4 -3 -4)) => '((3 3) (3 4) (3 -3) (3 -4)))

(fact "choose-from-scale"
  (choose-from-scale 60 -3 '(36 38 40 41 43 45 47 48 50 52 53 55 57 59 60 62 64 65 67 69 71 72
                             74 76 77 79 81 83 84 86 88 89 91 93 95 96))
  => 57)

(fact "no-solution-exists?"
  (no-solution-exists?
   60
   '(69 71 72 76 74 72 74 72 71 69)
   '((-7 (1 1 2) (-1 -2 1)) (-9 (1 -1 -1) (-1 -2 2)) (-4 (1) (-1))
     (-4 (1 1) (-2 2))))
  => false)

(fact "select-new-seed-note"
  (select-new-seed-note
   '(69 71 72 76 74 72 74 72 71 69)
   '(36 38 40 41 43 45 47 48 50 52 53 55 57 59 60 62 64 65 67 69 71 72 74 76 77 79 81 83 84 86 88 89 91 93 95 96)
   '((-5 (4 0)) (-5 (4 0)))) =>  60)

(fact "collect-all"
  (collect-all '(4 0)
               '((-5 (4 0)))) => '((-5 (4 0))))

(fact "create choices"
(create-choices
 '(36 38 40 41 43 45 47 48 50 52 53 55 57 59 60 62 64 65 67 69 71 72
      74 76 77 79 81 83 84 86 88 89 91 93 95 96) 60) => '(62 64 59 57))

(fact "get new starting point"
  (get-new-starting-point '(57 55 53 52 55)) =>
  '(57 55 53 52))

(fact "reduce rule"
  (reduce-rule '(-11 (2 -1 -1 1) (-1 1 -1 nil))) =>
  '(-14 (-1 -1 1) (1 -1 nil)))

(fact "match rules freely"
  (match-rules-freely
   '(-9 (-1 -1 nil) (1 1 nil))
   '((-9 (-1 1 -1) (-1 -2 2)) (-9 (-1 -1 -1) (1 2 -1))
     (-12 (1 -1 -1) (-1 2 2)) (-11 (2 -1 -1) (-1 2 1)) (-4 (1) (2))
     (-4 (1 1) (-2 -1)) (-9 (1 -1 -1) (-1 -2 -1))
     (-7 (1 1 2) (-1 -2 -2))))  => nil)

(future-fact "create new line"
  (reset! new-line [])
  (create-new-line  (map music/note [:A3 :B3 :C4 :E4 :D4 :C4 :D4 :C4 :B3 :A3])
                    major-scale
                    (map music/note [:E3 :A2 :D3 :B2])
                    nil) => (map music/note [:A2 :G2 :F2 :G2 :F2 :A2 :G2 :A2 :B2 :D3]))

(fact "check relevant cf notes"
  (create-relevant-cf-notes '(57 55 57 55 53 57 55 57)
                            '(69 71 72 76 74 72 74 72 71 69)) => '(72 71))

(fact "matching interval rule"
  (match-interval-rule
   '((-1 -1 nil) (-1 -1 nil))
   '((-1 1 -1) (-1 -2 2))) => nil)

(fact "look ahead"
  (look-ahead 1 '(69 71 72 76 74 72 74 72 71 69) '(62) '(-4 nil nil) '((-9 (-1 1 -1) (-1 -2 2)) (-9 (-1 -1 -1) (1 2 -1))
                                                                       (-12 (1 -1 -1) (-1 2 2)) (-11 (2 -1 -1) (-1 2 1))
                                                                       (-4 (1) (2)) (-4 (1 1) (-2 -1))
                                                                       (-9 (1 -1 -1) (-1 -2 -1)) (-7 (1 1 2) (-1 -2 -2))))
  => truthy)

(fact "evalulate choices"
  (evaluate-choices '(69 71 72 76 74 72 74 72 71 69)
                    '(53 57 52 59)
                    '(57 55 57 55 53 57 55)) => 57)

(fact "evaluate"
  (let [note
        (evaluate (map music/note [:A3 :B3 :C4 :E4 :D4 :C4 :D4 :C4 :B3 :A3])
                  (map music/note [:C2 :E2 :B1])
                  (map music/note [:A1 :G2 :A2 :G2 :F2 :E2 :D2]))]
    (count note) => 1
    (first note)) => (pitch-as-note :E2))

(fact "test for parallel octaves and fifths"
  (parallel-octaves-and-fifths? '(69 71 72 76 74 72) 52 '(57 55 53 52 53)) => nil)

(fact "test for leaps"
  (leaps? '(57 55 57 55 53 57 53)) => truthy)

(fact "test for simultaneous leaps"
  (simultaneous-leaps? '(69 71 72 76 74 72 74 72 71 69) 60 '(57 55 57 55 59 57 55 57 59))  => nil)

(fact "test for direct fifths"
  (direct-fifths? '(69 71 72 76 74 72 74 72 71 69) 60 '(57 55 57 55 53 57 55 57 59)) => nil)

(fact "test for consecutive motions"
  (consecutive-motions? '(69 71 72 76 74 72 74 72 71 69) 65 '(57 55 53 55 59 57 59 60 62)) => nil)