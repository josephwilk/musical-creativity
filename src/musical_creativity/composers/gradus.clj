(ns musical-creativity.composers.gradus)

(def major-scale '(36 38 40 41 43 45 47 48 50 52 53 55
                   57 59 60 62 64 65 67 69 71 72 74 76
                   77 79 81 83 84 86 88 89 91 93 95 96))

(def illegal-verticals '(0 1 2 5 6 10 11 13 14 17 18 22 23 25 26 29 30 34 35 -1 -2 -3 -4 -5 -6 -7 -8))
(def illegal-parallel-motions '((7 7)(12 12)(19 19)(24 24)))
(def illegal-double-skips '((3 3)(3 4)(3 -3)(3 -4)(-3 -3)(-3 -4)(-3 3)(-3 4)
                                 (4 3)(4 4)(4 -3)(4 -4)(-4 -3)(-4 -4)(-4 3)(-4 4)))
(def direct-fifths-and-octaves '((9 7)(8 7)(21 19)(20 19)))
(def solution ())
(def counterpoint ())
(def save-voices ())
(def rules ())
(def *seed-note* (atom 60))
(def seed-notes '(64 62 59 57 55 60) )
(def backtrack () )
(def *cantus-firmus* '(69 71 72 76 74 72 74 72 71 69))
(def new-line () )
(def save-rules () )
(def *print-state* true )
(def *auto-goals* () )
(def saved-templates () )

(def c1 36)
(def d1 38)
(def e1 40)
(def f1 41)
(def g1 43)
(def a1 45)
(def b1 47)
(def c2 48)
(def d2 50)
(def e2 52)
(def f2 53)
(def g2 55)
(def a2 57)
(def b2 59)
(def c3 60)
(def d3 62)
(def e3 64)
(def f3 65)
(def g3 67)
(def a3 69)
(def b3 71)
(def c4 72)
(def d4 74)
(def e4 76)
(def f4 77)
(def g4 79)
(def a4 81)
(def b4 83)
(def c5 84)
(def d5 86)
(def e5 88)
(def f5 89)
(def g5 91)
(def a5 93)
(def b5 95)
(def c5 96)

(def list-of-notes '(c1 d1 e1 f1 g1 a1 b1 c2 d2 e2 f2 g2 a2 b2 c3 d3 e3 f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5 d5
                        e5 f5 g5 a5 b5 c5) )
(def look-ahead ())
(def temporary-rules (atom []))
(def last-cantus-firmus (atom []))
(def past-model-length ())
(def models
  '(((72 71 74 72 71 69 67 69) (64 67 65 64 62 65 64 60))
    ((72 71 74 72 71 69 67 69) (57 55 53 57 55 53 55 53))
    ((72 71 74 72 71 69 67 69) (57 55 53 52 50 53 52 48))
    ((72 71 74 72 71 69 67 69) (64 67 65 64 67 65 64 60))
    ((69 71 72 69 71 72 74 77 76 74 72) (57 55 52 53 55 57 55 57 55 59 57))
    ((69 71 72 69 71 72 74 77 76 74 72) (57 55 52 53 55 57 55 53 55 53 52))
    ((69 71 72 69 71 72 74 77 76 74 72) (57 55 52 53 55 57 55 53 55 59 57))
    ((69 71 72 69 71 72 74 77 76 74 72) (57 55 52 53 55 57 55 57 60 59 60))
    ((69 71 72 69 71 72 74 77 76 74 72) (57 55 52 53 55 57 55 57 60 59 57))
    ((72 71 69 67 69 72 71 72) (64 62 60 64 62 60 62 64))
    ((72 71 69 67 69 72 71 72) (64 62 65 64 65 64 67 65))
    ((72 71 69 67 69 72 71 72) (57 59 60 64 62 60 62 64))
    ((72 71 69 67 69 72 71 72) (57 55 53 55 53 52 50 48))
    ((72 71 69 67 69 72 71 72) (64 62 65 64 65 64 62 64))
    ((72 71 69 67 69 72 71 72) (64 67 65 64 62 60 62 64))
    ((72 71 69 67 69 72 71 72) (57 59 60 64 62 64 67 65))
    ((72 71 69 67 69 72 71 72) (57 55 53 55 53 52 55 53))
    ((72 71 69 67 69 72 71 72) (64 62 65 64 62 60 62 60))
    ((72 71 69 67 69 72 71 72) (64 62 60 64 62 64 67 65))
    ((72 71 69 67 69 72 71 72) (64 67 65 64 62 64 67 65))
    ((72 71 69 67 69 72 71 72) (57 55 53 55 53 52 50 52))
    ((72 71 69 67 69 72 71 72) (64 67 65 64 62 64 62 60))
    ((72 71 69 67 69 72 71 72) (64 67 65 64 62 60 62 60))
    ((72 71 69 67 69 72 71 72) (64 62 60 64 62 64 62 60))
    ((69 71 72 76 74 72 71 72 74 72) (57 55 57 55 53 57 55 57 55 57))
    ((69 71 72 76 74 72 71 72 74 72) (57 55 57 55 53 57 55 52 53 57))
    ((69 71 72 76 74 72 71 72 74 72) (57 55 57 55 53 57 55 53 50 52))
    ((69 71 72 76 74 72 71 72 74 72) (57 55 57 55 59 57 59 57 59 57))
    ((69 71 72 76 74 72 71 72 74 72) (57 55 57 55 59 57 59 57 55 52))
    ((69 71 72 76 74 72 71 72 74 72) (57 55 53 52 53 52 50 48 47 45))
    ((69 71 72 76 74 72 71 72 74 72) (57 55 57 55 53 52 55 53 50 52))
    ((69 71 69 72 71 74 72 71 69) (57 55 53 52 55 53 57 55 57))
    ((69 71 69 72 71 74 72 71 69) (57 55 53 52 55 53 57 55 53))
    ((69 71 69 72 71 74 72 71 69) (57 55 53 52 55 53 52 50 53))
    ((69 71 72 76 74 72 74 72 71 69) (57 55 53 55 53 57 55 57 59 60))
    ((69 71 72 76 74 72 74 72 71 69) (57 55 57 55 53 52 50 52 50 53))
    ((69 71 72 76 74 72 74 72 71 69) (57 55 53 55 53 57 55 57 59 62))
    ((69 71 72 76 74 72 74 72 71 69) (57 55 53 52 53 52 53 57 55 57))
    ((69 71 72 76 74 72 74 72 71 69) (57 55 53 55 59 57 55 57 59 62))
    ((69 71 72 76 74 72 74 72 71 69) (57 55 53 52 53 52 50 52 55 53))
    ((69 71 72 76 74 72 74 72 71 69) (57 55 57 55 53 52 50 52 55 53))
    ((69 71 72 76 74 72 74 72 71 69) (57 55 53 52 53 57 55 57 55 57))
    ((69 71 72 76 74 72 74 72 71 69) (57 55 53 55 53 57 55 57 55 53))
    ((69 71 72 76 74 72 74 72 71 69) (57 55 53 55 59 57 55 57 55 53))
    ((69 71 72 76 74 72 74 72 71 69) (57 55 57 55 59 57 59 60 62 65))
    ((69 71 72 76 74 72 74 72 71 69) (57 55 57 55 59 57 59 60 62 60))
    ((69 71 69 72 71 74 72 71 69) (57 55 53 52 55 53 52 55 53))
    ((69 71 72 76 74 72 71 72 74 72) (57 55 57 55 59 57 55 53 50 52))
    ((69 71 72 74 71 72 74 72) (57 55 57 53 55 53 50 52))
    ((69 71 72 69 71 72 74 77 76 74 72) (57 55 52 53 55 57 55 53 55 53 57))))

(defn sortcar [lists]
  "sorts by the first element."
  (sort (fn [x y] (> (first x) (first x)))  lists))

(defn get-diatonic-note [current-note interval scale]
  "a simple variant of choose-from-scale which uses a diatonic interval as its second arg."
  (cond ((nil? interval)())
        ((> interval 0)(nth interval (member current-note scale)))
        (t (nth (abs interval) (member current-note (reverse scale))))))

(defn select-new-seed-note [cantus-firmus scale saved-templates]
  "select a logical new seed note."
  (get-diatonic-note (first cantus-firmus)
                     (first
                      (second
                       (first
                        (sortcar (return-counts (collect-all (get-map cantus-firmus scale) saved-templates))))))
                     scale))

(defn gradus
  "top-level function of the counterpoint program."
  [& [auto-goals print-state seed-note cantus-firmus]]
  (let [auto-goals (or auto-goals *auto-goals*)
        print-state (or print-state *print-state*)
        seed-note (or seed-note nil)
        cantus-firmus (or cantus-firmus *cantus-firmus*)]
    (when-not (= last-cantus-firmus *cantus-firmus*)
            (do
              (reset! temporary-rules [])
              (reset! last-cantus-firmus *cantus-firmus*)))

    (if seed-note (reset! *seed-note* seed-note)
        (let [test (select-new-seed-note *cantus-firmus* *major-scale* *saved-templates)]
          (if test (reset! *seed-note* test))))
    (setq auto-goals auto-goals)
    (setq print-state print-state)
    (setq cantus-firmus cantus-firmus)
    (if (nil? auto-goals)(set-default-goals))
    (if auto-goals (do (set-goals *models*)(setq auto-goals ())(setq past-model-length (length models))))
    (if (not (equal (length models*) *past-model-length*)) (set-goals models))
    (setq past-model-length (length *models))
    (setq new-line ())
    (setq solution
          (create-new-line
           cantus-firmus
           major-scale
           (mix (create-choices major-scale* *seed-note)) nil))
    (setq save-voices* (list (firstn (length *solution*) *cantus-firmus)
                             solution))
    (setq save-voices* (mapcar #'translate-into-pitchnames *save-voices))
    (setq counterpoint* (make-events (pair *save-voices)))
    (if (equal (length cantus-firmus*)(length (second *save-voices)))
      (push (analyze-for-template seed-note *cantus-firmus* *major-scale)
            saved-templates))
    counterpoint))

(defn create-new-line
  "creates a new line with the cantus firmus."
  ([cantus-firmus scale choices last-notes] (create-new-line cantus-firmus scale choices last-notes (length cantus-firmus)))
  ([cantus-firmus scale choices last-notes length)]

  (if (stop-if-all-possibilities-are-nil seed-note *cantus-firmus* rules)
    (format t "~a~&" "i can find no solution for this cantus firmus.")
    (if (<= length 0) new-line
        (let ((test (evaluate-choices cantus-firmus choices last-notes)))
          (if (nil? test)
            (do
             (if (nil? look-ahead)
               (pushnew (create-rule cantus-firmus (append last-notes (list (first choices)))) rules :test #'equal)
               (pushnew (create-rule cantus-firmus (append last-notes (list (first choices)))) temporary-rules :test #'equal))
             (do (setq save-rules* *rules)
                    (if (not (< (length rules*)(length *save-rules)))
                      (print-backtracking)))
             (let ((new-last-notes (get-new-starting-point last-notes)))
               (setf new-line* (butlast *new-line (- (length last-notes)(length new-last-notes))))
               (create-new-line cantus-firmus
                                scale
                                (remove (my-last last-notes)
                                        (mix (create-choices
                                              major-scale
                                              (if (nil? new-last-notes) seed-note (my-last new-last-notes)))))
                                new-last-notes
                                (+ length (- (length last-notes)(length new-last-notes))))))
            (do (setf new-line* (append *new-line (list test)))
                   (if print-state* (print-working cantus-firmus *new-line))
                   (create-new-line cantus-firmus
                                    scale
                                    (mix (create-choices major-scale test))
                                    (append last-notes (list test))
                                    (1- length))))))))

(defn get-new-starting-point
  "for backtracking - starts 2 earlier or nil"
  [last-notes]
  (cond ((<= (length last-notes) 1) ())
        (t (butlast last-notes 1))))

(defn evaluate-choices
  "runs the evaluate and look-ahead functions through the various choices."
  [cantus-firmus choices last-notes]
  (let ((correct-choices (evaluate cantus-firmus choices last-notes)))
    (if correct-choices (setq look-ahead* t)(setq *look-ahead ()))
    (if (> (length correct-choices) 0)
      (look-ahead-for-best-choice cantus-firmus last-notes correct-choices)
      (first correct-choices))))

(defn evaluate
  "evaluates the various choices for a next note based on the goals and current rules"
  [cantus-firmus choices last-notes]
  (let ((choice (first choices)))
    (cond ((nil? choices)())
          ((and (not (consult-rules (create-rule cantus-firmus (append last-notes (list choice)))))
                (not (test-for-vertical-dissonance (nth (length last-notes) cantus-firmus) choice))
                (not (test-for-parallel-octaves-and-fifths (firstn (1+ (length last-notes)) cantus-firmus)
                                                           choice last-notes))
                (not (test-for-leaps (append last-notes (list choice))))
                (not (test-for-simultaneous-leaps (firstn (1+ (length last-notes)) cantus-firmus)
                                                  choice last-notes))
                (not (test-for-direct-fifths (firstn (1+ (length last-notes)) cantus-firmus)
                                             choice last-notes))
                (not (test-for-consecutive-motions (firstn (1+ (length last-notes)) cantus-firmus)
                                                   choice last-notes)))
           (cons choice (evaluate cantus-firmus (rest choices) last-notes)))
          (t (evaluate cantus-firmus (rest choices) last-notes)))))

(defn create-choices [scale last-choice]
  "creates four possible choices - seconds and thirds - from a previous pitch choice."
  [(choose-from-scale last-choice 1 scale)
   (choose-from-scale last-choice 3 scale)
   (choose-from-scale last-choice -1 scale)
   (choose-from-scale last-choice -3 scale)])

(defn choose-from-scale [current-note interval-class scale]
  "gets the appropriate pitch from the current scale based on the interval class."
  (if (> interval-class 0)
    (nth (get-diatonic-interval interval-class) (member current-note scale))
    (nth (abs (get-diatonic-interval interval-class)) (member current-note (reverse scale)))))

(defn get-diatonic-interval [interval-class]
  "translates interval-classes into diatonic-interval classes."
  (case interval-class
     1 1
     2 1
     3 2
     4 2
    -1 -1
    -2 -1
    -3 -2
    -4 -2
    :else
    1))

(defn consult-rules [rule]
  "calling (consult-rules (-9 (2 -1 -1) (-1 2 -2))) consult-rules returned nil"
  (or (member rule rules :test #'equal)
      (member rule temporary-rules :test #'equal)))

(defn create-rule [cantus-firmus new-notes]
  "creates rules for the rules variable"
  (let ((the-list (the-last 4 new-notes)))
    (create-interval-rule
     (list (the-last (length the-list)
                     (butlast cantus-firmus (- (length cantus-firmus)(length new-notes)))) the-list))))

(defn test-for-vertical-dissonance
  "tests to ensure vertical dissonance"
  [cantus-firmus-note choice]
  (if (member (- cantus-firmus-note choice) illegal-verticals) choice))

(defn test-for-parallel-octaves-and-fifths
  "tests for parallel octaves and fifths."
  [cantus-firmus choice last-notes]
  (let ((cantus-firmus-to-here (firstn (1+ (length last-notes)) cantus-firmus)))
  (cond ((or (not (>= (length cantus-firmus-to-here) 2))(not (>= (length last-notes) 1))) ())
        ((member (list (abs (- (second-to-last cantus-firmus-to-here)(my-last last-notes)))
                       (abs (- (my-last cantus-firmus-to-here) choice)))
                      illegal-parallel-motions :test #'equal) t)
        (t nil))))

(defn test-for-leaps [extended-last-notes]
  "tests for leaps and avoids two in row and ensures that leaps are followed by contrary motion steps."
  (cond ((not (>= (length extended-last-notes) 3)) ())
        ((member (list (- (second-to-last extended-last-notes)(my-last extended-last-notes))
                       (- (third-to-last extended-last-notes)(second-to-last extended-last-notes)))
                 illegal-double-skips :test #'equal) t)
        ((and (> (abs (- (third-to-last extended-last-notes)(second-to-last extended-last-notes))) 2)
              (not (opposite-sign (list (- (second-to-last extended-last-notes)(my-last extended-last-notes))
                                        (- (third-to-last extended-last-notes)(second-to-last extended-last-notes))))))
         t)
        (t ())))

(defn test-for-simultaneous-leaps [cantus-firmus choice last-notes]
  "tests for the presence of simultaneous leaps."
  (let ((cantus-firmus-to-here  (firstn (1+ (length last-notes)) cantus-firmus)))
  (cond ((or (not (>= (length cantus-firmus-to-here) 2))(not (>= (length last-notes) 1))) ())
        ((and (skipp (the-last 2 cantus-firmus-to-here))(skipp (the-last 2 (append last-notes (list choice))))) t)
        (t ()))))

(defn test-for-direct-fifths [cantus-firmus choice last-notes]
  "tests for direct fifths between the two lines."
  (let ((cantus-firmus-to-here  (firstn (1+ (length last-notes)) cantus-firmus)))
    (cond ((or (not (>= (length cantus-firmus-to-here) 2))(not (>= (length last-notes) 1))) ())
          ((member (get-verticals (the-last 2 cantus-firmus-to-here)(the-last 2 (append last-notes (list choice))))
                   direct-fifths-and-octaves :test #'equal) t)
          (t ()))))

(defn test-for-consecutive-motions [cantus-firmus choice last-notes]
  "tests to see if there are more than two consecutive save-direction motions."
  (let ((cantus-firmus-to-here  (firstn (1+ (length last-notes)) cantus-firmus)))
  (cond ((or (not (> (length cantus-firmus-to-here) 3))(not (> (length last-notes) 2))) ())
        ((let ((last-four-cf (the-last 4 cantus-firmus-to-here))
               (last-four-newline (the-last 4 (append last-notes (list choice)))))
           (not (or (opposite-sign (list (first (get-intervals (firstn 2 last-four-cf)))
                                         (first (get-intervals (firstn 2 last-four-newline)))))
                    (opposite-sign (list (first (get-intervals (firstn 2 (rest last-four-cf))))
                                         (first (get-intervals (firstn 2 (rest last-four-newline))))))
                    (opposite-sign (list (first (get-intervals (the-last 2 last-four-cf)))
                                         (first (get-intervals (the-last 2 last-four-newline)))))))) t)
        (t ()))))

(defn create-interval-rule [rule]
  "creates the interval rule as in (-7 (2 2 2)(-1 1 2))."
  (list (first (find-scale-intervals (list (first (first rule))
                                            (first (second rule)))
                                      major-scale))
        (find-scale-intervals (first rule) major-scale)
        (find-scale-intervals (second rule)  major-scale)))

(defn reduce-to-within-octave [interval]
  "reduces diatonic intervals to within the octave."
  (cond ((and (> (abs interval) 7)(minusp interval))
         (reduce-to-within-octave (+ interval 7)))
        ((> (abs interval) 7)(- interval 7))
        ((zerop interval) -7)
        (t interval)))

(defn find-scale-intervals [notes scale]
  "returns the diatonic intervals between the notes according to the scale."
  (cond ((nil? (rest notes))())
        ((nil? (second notes))
         (cons nil (find-scale-intervals (rest notes) scale)))
        (t (cons (let ((first-note-test (member (first notes) scale :test #'equal))
                       (second-note-test (member (second notes) scale :test #'equal)))
                   (if (< (first notes)(second notes))
                     (length (butlast first-note-test (length second-note-test)))
                     (- (length (butlast second-note-test (length first-note-test))))))
                 (find-scale-intervals (rest notes) scale)))))

(defn look-ahead-for-best-choice [cantus-firmus last-notes correct-choices]
  "looks ahead for the best choice"
  (cond ((nil? correct-choices) ())
        ((not (look-ahead 1
                          cantus-firmus
                          (append last-notes (list (first correct-choices)))
                          (create-rule cantus-firmus (append last-notes (list (first correct-choices))))
                          rules))
         (first correct-choices))
        (t (look-ahead-for-best-choice cantus-firmus last-notes (rest correct-choices)))))

(defn look-ahead [amount cantus-firmus last-notes rule rules]
  "the top-level function for looking ahead."
  (match-rules-freely
   (reduce-rule (make-freer-rule amount (find-scale-intervals (create-relevant-cf-notes last-notes cantus-firmus) major-scale) rule))
   rules))

(defn create-relevant-cf-notes [last-notes cantus-firmus]
  "creates the set of forward reaching cf notes."
  (firstn 2 (nthcdr (1- (length last-notes)) cantus-firmus)))

(defn reduce-rule [rule]
  "reduces the front-end of the look-ahead rule."
  (if (<= (length (second rule)) 3) rule
      (let ((amount (- (length (second rule)) 3)))
        (cons (+ (first rule)(- (first (second rule)))(first (third rule)))
              (mapcar #'(lambda (x)(nthcdr amount x)) (rest rule))))))

(defn make-freer-rule [amount cf-notes rule]
  "adds the appropriate number of nils to the new line for look-ahead matching."
  (if (zerop amount) rule
      (make-freer-rule (1- amount)
                       (rest cf-notes)
                       (list (first rule)
                             (append (second rule)(list (first cf-notes)))
                             (append (third rule)(list nil))))))

(defn match-rules-freely [rule rules]
  "runs the match-rule function through the rules."
  (cond ((nil? rules)())
        ((and (equal (first rule)(first (first rules)))
              (match-interval-rule (rest rule)(rest (first rules)))) t)
        ((and (equal (first rule)(first (first rules)))
              (equal (length (second rule))(length (second (first rules))))
              (match-rule rule (first rules))) t)
        (t (match-rules-freely rule (rest rules)))))

(defn match-interval-rule [rule-for-matching rule]
  "matches the freer rule to the rule from rules."
  (cond ((and (nil? (first rule-for-matching))(nil? (first rule))) t)
        ((or (and (equal (very-first rule-for-matching)(very-first rule))
                  (equal (very-second rule-for-matching)(very-second rule)))
             (and (equal (very-first rule-for-matching)(very-first rule))
                  (nil? (very-second rule-for-matching))))
         (match-interval-rule (mapcar #'rest rule-for-matching) (mapcar #'rest rule)))
        (t nil)))

(defn match-rule [rule-for-matching rule]
  "matches the freer rule to the rule from rules."
  (cond ((and (nil? (first (rest rule-for-matching)))(nil? (first (rest rule)))) t)
        ((or (and (equal (very-first (rest rule-for-matching))(very-first (rest rule)))
                  (equal (very-second (rest rule-for-matching))(very-second (rest rule))))
             (and (equal (very-first (rest rule-for-matching))(very-first (rest rule)))
                  (nil? (very-second (rest rule-for-matching)))))
         (match-rule (cons (first rule-for-matching)(mapcar #'rest (rest rule-for-matching)))
                     (cons (first rule)(mapcar #'rest (rest rule)))))
        (t nil)))

(defn replenish-seed-notes []
  "replenishes the seednotes when when they have all been used."
  (setq seed-notes '(60 65 64 62 59 57 55 53)))

(defn set-goals [models]
  "sets the goals for the gradus program."
  (setf illegal-verticals (get-illegal-verticals models))
  (setf illegal-parallel-motions (find-illegal-parallels models))
  (setf direct-fifths-and-octaves (find-illegal-parallels models))
  (setf illegal-double-skips (possible-combinations '(3 4 -3 -4))))

(defn get-illegal-verticals [models]
  "returns all of the vertical intervals not in the models."
  (get-complement (get-the-verticals models)))

(defn get-complement
  "incrementally returns all of the intervals not in the verticals arg."
  ([verticals] (get-complement verticals 0))
  ([verticals number]

  (cond ((nil? verticals)())
        ((member number verticals)
         (get-complement (rest verticals)(1+ number)))
        (t (cons number (get-complement verticals (1+ number)))))))

(defn get-the-verticals [models]
  "collects the vertical intervals from the models used."
  (my-sort #'<
           (remove-duplicates
            (project
             (let ((voiced-music (pair (make-voices models))))
               (loop for pair in voiced-music
                     collect (- (first pair) (second pair))))) :test #'equal)))

(defn make-voices [models]
  "makes lists of the cantus firmus and accompanying line pitches."
  (list (apply #'append (mapcar #'first models))(apply #'append (mapcar #'second models))))

(defn project [numbers]
  ""
  (if (nil? numbers)()
      (append (pro (first numbers))
              (project (rest numbers)))))

(defn pro [number]
  "projects octaves out from number."
  (if (> number 12)
    (list (- number 12) number (+ number 12))
    (list number (+ number 12)(+ number 24))))

(defn my-sort [function lists]
  "non-destructively sorts its arg by function."
  (loop for item in (sort (loop for x in lists
                                collect (list x))  function :key #'car)
        collect (first item)))

(defn find-illegal-parallels [models]
  "returns the non-used parallels in the models which are assumed to be illegal."
  (let* ((illegal-verticals (get-illegal-verticals models)) ;;;good!
         (legal-verticals (remove-illegal-verticals illegal-verticals (find-all-possible-motions 24)))
         (model-verticals (find-legals models)))
    (remove-legal-motions model-verticals legal-verticals)))

(defn remove-legal-motions [legal-motions motions]
  "removes the legal motions from the motions arg."
  (cond ((nil? legal-motions) motions)
        ((member (first legal-motions) motions :test #'equal)
         (do (setf motions (remove (first legal-motions) motions :test #'equal))
                (remove-legal-motions (rest legal-motions) motions)))
        (t (remove-legal-motions (rest legal-motions) motions))))

(defn find-legals [models]
  "collects the legal motions in its arg."
  (if (nil? models)()
      (append (find-the-legals (pair (first models)))
              (find-legals (rest models)))))

(defn find-the-legals [paired-model]
  "discovers the legal motions in its arg."
  (if (nil? (rest paired-model))()
      (cons (list (- (first (first paired-model))(second (first paired-model)))
                  (- (first (second paired-model))(second (second paired-model))))
            (find-the-legals (rest paired-model)))))

(defn remove-illegal-verticals [illegal-verticals all-verticals]
  "removes the illegal verticals in its second arg."
  (cond ((nil? all-verticals) ())
        ((anyp illegal-verticals (first all-verticals))
         (remove-illegal-verticals illegal-verticals (rest all-verticals)))
        (t (cons (first all-verticals)
                 (remove-illegal-verticals illegal-verticals (rest all-verticals))))))

(defn find-all-possible-motions [extent &optional (value 0)(save-extent extent)]
  "returns all possible motions to its extent arg."
  (if (zerop extent)()
      (append (find-motions extent save-extent)
              (find-all-possible-motions (1- extent) value save-extent))))

(defn find-motions [extent value]
  "sub-function of find-all-possible-motions."
  (if (zerop value)()
      (cons (list extent value)
            (find-motions extent (1- value)))))

(defn anyp [find-list target-list]
 "returns any of first arg in second arg."
  (loop for find in find-list
        when (member find target-list :test #'equal)
        return find))

(defn possible-combinations [list &optional [save-list list)]
  "returns all possible combinations of its list arg."
  (if (nil? list)()
      (append (combinations (first list) save-list)
              (possible-combinations (rest list) save-list))))

(defn combinations [object list]
  "a sub-function of possible-combinations."
  (if (nil? list)()
      (cons (list object (first list))
            (combinations object (rest list)))))

(defn analyze-for-template [seed-note cantus-firmus scale]
  "returns the complete template (seed interval and map) for saving."
  (list (first (find-scale-intervals (list (first cantus-firmus) seed-note) scale))
        (get-map cantus-firmus scale)))

(defn get-map [cantus-firmus scale]
  "returns the map part of the template."
  (list (get-tessitura cantus-firmus scale)
        (first (find-scale-intervals (list (first cantus-firmus)(my-last cantus-firmus)) scale))))

(defn get-tessitura [cantus-firmus scale]
  "gets the tessitura or highest/lowest interval of a note list."
  (let ((up
         (abs (first (find-scale-intervals (list (first cantus-firmus)(apply #'max cantus-firmus)) scale))))
        (down
         (abs (first (find-scale-intervals (list (first cantus-firmus)(apply #'min cantus-firmus)) scale)))))
    (if (> up down) up (- down))))

(defn collect-all [map saved-templates]
  "collects all of the occurances of each member of its arg."
  (cond ((nil? saved-templates)())
        ((equal map (second (first saved-templates)))
         (cons (first saved-templates)
               (collect-all map (rest saved-templates))))
        (t (collect-all map (rest saved-templates)))))

(defn return-counts [templates]
  "simply adds the count of occurances to the beginning of each member of its arg."
  (if (nil? templates)()
      (cons (list (count (first templates) templates :test #'equal)(first templates))
            (return-counts (remove (first templates) templates :test #'equal)))))

(defn make-events (pitch-groupings &optional (ontime 0)]
  "makes consecutive events out of the pairs of pitches in its arg."
  (if (nil? pitch-groupings) ()
      (append (list (make-event ontime (first (first pitch-groupings)) 1)
                    (make-event ontime (second (first pitch-groupings)) 2))
              (make-events (rest pitch-groupings)(+ ontime 1000)))))

(defn make-event [ontime pitch channel]
  "creates an event based on args."
  (list ontime
        (if (symbolp pitch) (eval pitch) pitch)
        1000
        channel
        90))

(defn choose-one [list]
  "chooses one its arg randomly."
  (nth (random (length list) rs) list))

(defn mix [list]
  "mixes its arg arbitrarily"
  (let ((choice ()))
    (loop until (nil? list)
          do (setf choice (choose-one list))
          collect choice
          do (setf list (remove choice list :count 1)))))

(defn my-last [list]
  "returns th atom last of the list."
  (first (last list)))

(defn firstn [number list]
  "returns the first n of is list arg."
 (if (< (length list) number)(firstn (1- number) list)
     (butlast list (- (length list) number))))

(defn skipp [notes]
  "returns true if its two-number arg is a skip."
  (if (> (abs (- (second notes)(first notes))) 2) t))

(defn get-verticals [cantus-firmus new-line]
  "returns the intervals between two lines of counterpoint."
  (if (nil? cantus-firmus)()
      (cons (- (first cantus-firmus)(first new-line))
            (get-verticals (rest cantus-firmus)(rest new-line)))))

(defn get-intervals [notes]
  "returns a list of intervals one short of its pitch-list arg."
  (if (nil? (rest notes))()
      (cons (- (second notes)(first notes))
            (get-intervals (rest notes)))))

(defn opposite-sign [numbers]
  "returns t if the two numbers have opposite signs."
  (if (or (and (minusp (first numbers))(plusp (second numbers)))
          (and (plusp (first numbers))(minusp (second numbers)))) t))

(defn second-to-last [list]
  "returns the second to last of the list arg."
  (my-last (butlast list)))

(defn third-to-last [list]
  "returns the third to last of the list arg."
  (nth (- (length list) 3) (butlast list)))

(defn pair [voices]
  "pairs the two lists."
  (if (nil? (first voices))()
      (cons (list (first (first voices))(first (second voices)))
            (pair (list (rest (first voices))(rest (second voices)))))))

(defn print-backtracking []
  "simple printing function to show backtracking."
  (format t "~&~a~&~a~&~a~&" "backtracking.....there are now" (length rules) "rules."))

(defn print-working [cantus-firmus last-notes)
  "simple printing function for continuing to compose"
  (format t "~&~a~&~a~&" "working....." (list (translate-into-pitchnames cantus-firmus)(translate-into-pitchnames last-notes))))

(defn the-last [n list]
  "returns the last n of list."
  (if (< (length list) n) list
      (nthcdr (- (length list) n) list)))

(defn very-first [list]
  "returns the first of the first of list."
  (first (first list)))

(defn very-second [list]
  "returns the first of the second of list."
  (first (second list)))

(defn set-default-goals []
  "sets the default goals for the program."
  (setq illegal-verticals '(0 1 2 5 6 10 11 13 14 17 18 22 23 25 26 29 30 34 35 -1 -2 -3 -4 -5 -6 -7 -8))
  (setq illegal-parallel-motions '((7 7)(12 12)(19 19)(24 24)))
  (setq illegal-double-skips '((3 3)(3 4)(3 -3)(3 -4)(-3 -3)(-3 -4)(-3 3)(-3 4)
                                 (4 3)(4 4)(4 -3)(4 -4)(-4 -3)(-4 -4)(-4 3)(-4 4)))
  (setq direct-fifths-and-octaves '((9 7)(8 7)(21 19)(20 19))))

(defn stop-if-all-possibilities-are-nil [seed-note cantus-firmus rules]
  "for stopping if no solution exists."
  (check-for-nils
   (mapcar #'(lambda (x)
               (reduce-to-within-octave
                (first (find-scale-intervals (list (first cantus-firmus) x)
                                             major-scale))))
           (create-choices major-scale seed-note)) rules))

(defn check-for-nils [choices rules]
  "checking to see if all possible first notes produce rule-conflicting problems."
  (cond ((nil? choices) t)
        ((member (list (first choices)
                       nil nil) rules :test #'equal)
         (check-for-nils (rest choices) rules))
        (t nil)))

(defn translate-into-pitchnames [list-of-midi-note-numbers]
  "used to translate midi note numbers into note names."
  (if (nil? list-of-midi-note-numbers)()
      (cons (nth (position (first list-of-midi-note-numbers) major-scale*) *list-of-notes)
            (translate-into-pitchnames (rest list-of-midi-note-numbers)))))

(defn translate-rule-into-pitches [first-note rule]
  "translates rules into more readable pitch names."
  (list (translate-notes first-note (second rule))
        (translate-notes (get-diatonic-note first-note (first rule) major-scale)(third rule))))

(defn translate-notes [first-note intervals]
  "translates interval lists into note names for readability."
  (if (nil? intervals)(translate-into-pitchnames (list first-note))
      (let ((test (get-diatonic-note first-note (first intervals) major-scale)))
        (append (translate-into-pitchnames (list first-note))
              (translate-notes test (rest intervals))))))

(defn evaluate-pitch-names [voices]
  "evaluates the pitch names of its arg into midi note numbers."
    (mapcar #'(lambda (x)(mapcar #'eval x)) voices))

(defn create-canon []
  "creates a simple canon in two voices using gradus."
  (setq seed-note (- (my-last *cantus-firmus) 12))
  (gradus)
  (setq save-voices* (evaluate-pitch-names *save-voices))
  (let ((theme (append *cantus-firmus* (mapcar #'(lambda (x)(+ x 12))(second *save-voices))))
         (lower-voice (mapcar #'(lambda (x)(- x 12)) theme)))
    (make-events
     (pair (list (append theme theme theme (make-list (length  cantus-firmus) :initial-element 0))
                 (append (make-list (length  cantus-firmus) :initial-element 0) lower-voice lower-voice lower-voice))))))