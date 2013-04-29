(ns musical-creativity.unit.composers.t-gradus
  (:require
   [midje.sweet :refer :all]
   [musical-creativity.composers.gradus :refer :all]))

(fact "find scale intervals"
  (find-scale-intervals
   [69 76]
   [36 38 40 41 43 45 47 48 50 52 53 55 57 59
    60 62 64 65 67 69 71 72 74 76 77 79 81 83
    84 86 88 89 91 93 95 96]) => [4])

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

(fact "combinations"
  (combinations 3 '(3 4 -3 -4)) => '((3 3) (3 4) (3 -3) (3 -4)))

(fact "choose-from-scale"
  (choose-from-scale 60 -3 '(36 38 40 41 43 45 47 48 50 52 53 55 57 59 60 62 64 65 67 69 71 72
                             74 76 77 79 81 83 84 86 88 89 91 93 95 96))
  => 57)

(fact "stop-if-all-possibilities-are-nil"
  (stop-if-all-possibilities-are-nil
   60
   '(69 71 72 76 74 72 74 72 71 69)
   '((-7 (1 1 2) (-1 -2 1)) (-9 (1 -1 -1) (-1 -2 2)) (-4 (1) (-1))
     (-4 (1 1) (-2 2))))
  => nil)

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

(fact "create new line"
  (create-new-line
   '(69 71 72 76 74 72 74 72 71 69)
   '(36 38 40 41 43 45 47 48 50 52 53 55 57 59 60 62 64 65 67 69 71 72
        74 76 77 79 81 83 84 86 88 89 91 93 95 96)
   '(64 57 62 59)
   nil) =>
  '(57 55 53 55 53 57 55 57 59 62))