(ns musical-creativity.composers.fuzzy
  (:require
   [musical-creativity.events :as events]))

; idevault values
(def default-duration 1000)
(def default-channel 1)
(def default-velocity 127)

; standard scales, chords, and notes
(def major-scale    [1 0 1 0 1 1 0 1 0 1 0 1])
(def nmin-scale     [1 0 1 1 0 1 0 1 1 0 1 0])
(def hmin-scale     [1 0 1 1 0 1 0 1 1 0 0 1])
(def chord-template [1 0 0 1 1 0 0 1 0 0 0 0])
(def fz-triad       [1 0 0 1 1 0 0.5 1 0 0 0 0])
(def fz-key         [1 0 1 0 1 1 0 1 0 1 0 1])
(def fz-note        [1 0 0 0 0 0 0 0 0 0 0 0])
(def current-scale major-scale)

; intervals up
(def fz-up-2 [0 0.9 1 0 0 0 0 0 0 0 0 0])
(def fz-up-3 [0 0 0 1 0.9 0 0 0 0 0 0 0])
(def fz-up-4 [0 0 0 0 0 1 0.5 0 0 0 0 0])
(def fz-up-5 [0 0 0 0 0 0 0.9 1 0.5 0 0 0])
(def fz-up-6 [0 0 0 0 0 0 0 0 1 0.9 0 0])
(def fz-up-7 [0 0 0 0 0 0 0 0 0 0 0.9 1])

;intervals down
(def fz-down-2b [0 0 0 0 0 0 0 0 0 0 1 0.9])
(def fz-down-3b [0 0 0 0 0 0 0 0 0.9 1 0 0])
(def fz-down-4b [0 0 0 0 0 0 0.5 1 0 0 0 0])
(def fz-down-5b [0 0 0 0 0.5 1 0.9 0 0 0 0 0])
(def fz-down-6b [0 0 0.9 1 0 0 0 0 0 0 0 0])
(def fz-down-7b [0 0.9 1 0 0 0 0 0 0 0 0 0])

(def old-chord-set [0 0 0 0 0 0 0 0 0 0 0 0])

(def last-solution (atom [0 0 0]))

; pitch classes
(def PC-C 0)
(def PC-DFLAT 1)
(def PC-D 2)
(def PC-EFLAT 3)
(def PC-E 4)
(def PC-F 5)
(def PC-GFLAT 6)
(def PC-G 7)
(def PC-AFLAT 8)
(def PC-A 9)
(def PC-BFLAT 10)
(def PC-B 11)

(declare common-tone-rules fz-un third-above fifth-above)

(defn minumum
  "Returns the minimum of its two args."
  [x y]
  (if (and x y)
    (min x  y)
    0))

(defn maximum
  "Returns the maximum of its two args."
  [x y]
  (if (and x y)
    (max x  y)
    0))

(defn fz-complement
  "Returns fuzzy complement (1 - membership) of a list."
  ([alist] (fz-complement alist 1.0))
  ([alist compvalue]
     (if (empty? alist)
       nil
       (cons (- 1 (first alist))
             (fz-complement (rest alist) compvalue)))))

(defn fz-intersect
  "Returns fuzzy intersection (minimums) of two lists"
  [lst1 lst2]
  (if (empty? lst1)
    nil
    (cons (minumum (first lst1) (first lst2))
          (fz-intersect (rest lst1) (rest lst2)))))

(defn common-tones-test
  "Applies common tone rules."
  [root-set third-set fifth-set old-chord-set]
  (common-tone-rules  root-set third-set fifth-set old-chord-set))

(defn common-tones
  "Sums the set intersections."
  [set1 set4]
  (apply + (fz-intersect set1 set4)))

(defn common-tone-rules
  "Returns set according to common tone rules."
  [set1 set2 set3 master]
  (let [result [0 0 0]
        result (assoc result 0 (common-tones set1 master))
        result (assoc result 1 (common-tones set2 master))
        result (assoc result 2 (common-tones set3 master))]
    result))

(defn top-n-positions
  "Returns positions of highest n values in list"
  [values how-many]
  (let [values-indexed (map-indexed vector values)
        sorted-list (sort (fn [[_ x] [_ y]] (> x y)) values-indexed)
        positions (map first sorted-list)]
    (take how-many positions)))

(defn ltop
  "Returns the topmost position of the list."
  [the-list]
  (first (top-n-positions the-list 1)))

(defn rotate-n
  "Rotates 12 element list n steps to right."
  [lst n]
  (let [shift-by (- 12 n)]
      (concat (subvec lst shift-by) (subvec lst 0 shift-by))))

(defn third-above
  "Returns a third above the note."
  [note]
  (first
   (top-n-positions
    (fz-intersect current-scale
                  (rotate-n fz-up-3 note)) 1)))

(defn fifth-above
  "Returns a fifth above the note."
  [note]
  (first
   (top-n-positions
    (fz-intersect current-scale
                  (rotate-n fz-up-5 note)) 1)))

(defn third-below
  "Returns a third below the note."
  [note]
  (first
   (top-n-positions
    (fz-intersect current-scale
                  (rotate-n fz-down-3b note)) 1)))

(defn fifth-below
  "Returns a fifth below the note."
  [note]
  (first
   (top-n-positions
    (fz-intersect current-scale
                  (rotate-n fz-down-5b note)) 1)))

(defn as-third
  "Returns pitch-class chord with arg as third."
  [note]
  (list (third-below note) note (fifth-above (third-below note))))

(defn as-fifth
  "Returns pitch-class chord with arg as fifth."
  [note]
  (list (fifth-below note) (third-above (fifth-below note)) note))

(defn ascend-list
  "Adds 12 to pitches lower than first one keeps chords in root position."
  [thelist]
  (let [root (first thelist)]
    (map (fn [pitch] (if (< pitch root) (+ pitch 12) pitch))
            thelist)))

(defn last-solution-test
  "Based on last-solution."
  [last-solution]
  (let [test (first (top-n-positions last-solution 1))]
    (cond
     (= 0 test) '(0 1 1)
     (= 1 test) '(1 1 0)
     :else
     '(1 0 0))))

(defn fz-u
  "Returns fuzzy union (maximums) of many lists."
  [the-lists]
  (if (empty? (rest the-lists))
    (first the-lists)
    (fz-un (first the-lists) (fz-u (rest the-lists)))))

(defn fz-union
  "Returns fuzzy union (maximums) of any number of lists"
  [& the-lists]
  (fz-u the-lists))

(defn fz-un
  "Returns fuzzy union (maximums) of two lists."
  [list1 list2]
  (when-not (empty? list1)
    (cons (maximum (first list1) (first list2))
          (fz-union (rest list1) (rest list2)))))

(defn make-set [pitch-list]
  (if (empty? pitch-list)
    [0 0 0 0 0 0 0 0 0 0 0 0]
    (fz-union (rotate-n fz-note (first pitch-list))
              (make-set (rest pitch-list)))))

(defn as-root
  "Returns pitch-class chord with arg as root."
  [note]
  [note (third-above note) (fifth-above note)])

(defn add-lists
  "Add two lists, member by member"
   [list1 list2]
     (map + list1 list2))

(defn favor-root-for-tonic
  "Gives slight edge to root position for tonic"
  [the-pc]
  (if (= 0 the-pc)
    '(0.1 0 0)
    '(0 0 0)))

(defn dither
  "Random tie breaker"
  []
  (let [choice (rand 3.0)]
    (cond
     (< choice 1) '(0.05 0 0)
     (< choice 2) '(0 0.05 0)
     :else
     '(0 0 0.05))))

(defn build-solution-set [the-pc last-solution]
  (let [root-set (make-set (as-root the-pc))
        third-set (make-set (as-third the-pc))
        fifth-set (make-set (as-fifth the-pc))
        old-chord-set old-chord-set]

    (-> [0 0 0]
        (add-lists (common-tones-test root-set third-set fifth-set old-chord-set))
        (add-lists (last-solution-test last-solution))
        (add-lists (favor-root-for-tonic the-pc))
        (add-lists (dither)))))

(defn pick-chord-with-more-rules
  "Returns pitch-classes of a chord."
  [note]
  (let [the-pc (rem note 12)]
    (reset! last-solution (build-solution-set the-pc @last-solution))
    (case (ltop @last-solution)
      2 (as-fifth the-pc)
      1 (as-third the-pc)

      (as-root the-pc))))

(defn add-octave
  "Adds octave to the list."
  ([the-list] (add-octave the-list 4))
  ([the-list the-octave]
     (map (fn [pitch] (+ pitch (* the-octave 12)))
             (ascend-list the-list))))

(defn make-event [pitch time]
  {:time time
   :pitch pitch
   :duration default-duration
   :channel default-channel
   :velocity default-velocity})

(defn make-chord-event
  "Simple notelist to chord."
  ([pitch-list] (make-chord-event pitch-list 0))
  ([pitch-list time]
     (map #(make-event % time) pitch-list)))

(defn pick-and-play-more-rules-chord
    "Returns a chord in event notation."
    [note time]
    (let [chord (pick-chord-with-more-rules (rem note 12))]
      (make-chord-event
       (add-octave chord 4) time)))

(defn fuzzy
  ([pcs] (fuzzy pcs 0))
  ([pcs time]
     (if (empty? pcs)
       []
       (concat (pick-and-play-more-rules-chord (first pcs) time)
               (fuzzy (rest pcs) (+ time 1000))))))

(defn compose []
  (events/make (fuzzy [0 4 7 5 7 11 0])))
