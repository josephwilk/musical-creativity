(ns musical-creativity.composers.fuzzy)

; idevault values
(def *default-dur* 1000)
(def *default-ch* 1)
(def *default-vel* 127)

; standard scales, chords, and notes
(def *maj-scale* [1 0 1 0 1 1 0 1 0 1 0 1])
(def *nmin-scale* [1 0 1 1 0 1 0 1 1 0 1 0])
(def *hmin-scale* [1 0 1 1 0 1 0 1 1 0 0 1])
(def *chord-template* [1 0 0 1 1 0 0 1 0 0 0 0])
(def *fz-triad* [1 0 0 1 1 0 0.5 1 0 0 0 0])
(def *fz-key* [1 0 1 0 1 1 0 1 0 1 0 1])
(def *fz-note* [1 0 0 0 0 0 0 0 0 0 0 0])
(def *current-scale* *maj-scale*)

; intervals up
(def *fz-2* [0 0.9 1 0 0 0 0 0 0 0 0 0])
(def *fz-3* [0 0 0 1 0.9 0 0 0 0 0 0 0])
(def *fz-4* [0 0 0 0 0 1 0.5 0 0 0 0 0])
(def *fz-5* [0 0 0 0 0 0 0.9 1 0.5 0 0 0])
(def *fz-6* [0 0 0 0 0 0 0 0 1 0.9 0 0])
(def *fz-7* [0 0 0 0 0 0 0 0 0 0 0.9 1])

;intervals down
(def *fz-2b* [0 0 0 0 0 0 0 0 0 0 1 0.9])
(def *fz-3b* [0 0 0 0 0 0 0 0 0.9 1 0 0])
(def *fz-4b* [0 0 0 0 0 0 0.5 1 0 0 0 0])
(def *fz-5b* [0 0 0 0 0.5 1 0.9 0 0 0 0 0])
(def *fz-6b* [0 0 0.9 1 0 0 0 0 0 0 0 0])
(def *fz-7b* [0 0.9 1 0 0 0 0 0 0 0 0 0])

(def *sol-set* (atom [0 0 0]))
(def *last-sol* [0 0 0])
(def *old-chord-set* [0 0 0 0 0 0 0 0 0 0 0 0])
(def *as-root-set* (atom [0 0 0 0 0 0 0 0 0 0 0 0]))
(def *as-third-set* (atom [0 0 0 0 0 0 0 0 0 0 0 0]))
(def *as-fifth-set* (atom [0 0 0 0 0 0 0 0 0 0 0 0]))

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

(declare common-tone-rules fz-un)
(defn fz-complement
  "Returns fuzzy complement (1 - membership) of a list."
  ([alist] (fz-complement alist 1.0))
  ([alist compvalue]
     (if (empty? alist)
       nil
       (cons (- 1 (first alist))
             (fz-complement (rest alist) compvalue)))))

(defn minumum
  "Returns the minimum of its two args."
  [x y]
  (if (and x y)
    (min x  y)
    0))

(defn fz-intersect
  "Returns fuzzy intersection (minimums) of two lists"
  [lst1 lst2]
  (if (empty? lst1)
    nil
    (cons (minumum (first lst1) (first lst2))
          (fz-intersect (rest lst1) (rest lst2)))))


(defn common-tones-test
  "Applies common tone rules."
  []
  (common-tone-rules  @*as-root-set* @*as-third-set* @*as-fifth-set* *old-chord-set*))

(defn sumup
  "Adds the members of a list."
  [the-list]
  (if (empty? the-list)
    0
    (+ (first the-list) (sumup (rest the-list)))))

(defn common-tones
  "Sums the set intersections."
  [set1 set4]
  (sumup (fz-intersect set1 set4)))

(defn common-tone-rules
  "Returns set according to common tone rules."
  [set1 set2 set3 master]
  (let [result [0 0 0]
        result (assoc result 0 (common-tones set1 master))
        result (assoc result 1 (common-tones set2 master))
        result (assoc result 2 (common-tones set3 master))]
    result))

(defn top-n-positions
  "Returns positions of highest n values in list, or nil if all 0."
  [the-list howmany]
  (when-not (= 0 howmany)  ; finished
    (if (nil? (sumup the-list)) ; empty list
      nil
      (let [alist the-list]
        (sort
         (cons
          (let [i (ref 0)
                k (ref 0)
                hsf (ref 0)] ;  hsf is highest so far
            (doseq [x alist]
              (when (> x hsf)
                (dosync (alter hsf x))
                (dosync (alter k i)))
              (dosync (alter i inc)))

            (dosync (alter alist (assoc alist k 0)))
            k) ; k is consed
          (top-n-positions alist (- howmany 1)))
         #'<)))))

(defn ltop
  "Returns the topmost position of the list."
  [thelist]
  (first (top-n-positions thelist 1)))

(defn ror-n
  "Rotates 12 element list n steps to right."
  [lst n]
  (let [shift-by (- 12 n)]
      (concat (subvec lst shift-by) (subvec lst 0 shift-by))))

(defn third-above
  "Returns a third above the note."
  [thenote]
  (first (top-n-positions (fz-intersect *current-scale* (ror-n *fz-3* thenote)) 1)))

(defn fifth-above
  "Returns a fifth above the note."
  [thenote]
  (first (top-n-positions (fz-intersect *current-scale* (ror-n *fz-5* thenote)) 1)))

(defn third-below
  "Returns a third below the note."
  [thenote]
  (first (top-n-positions (fz-intersect *current-scale* (ror-n *fz-3b* thenote)) 1)))

(defn fifth-below
  "Returns a fifth below the note."
  [thenote]
  (first (top-n-positions (fz-intersect *current-scale* (ror-n *fz-5b* thenote)) 1)))

(defn as-third
  "Returns pitch-class chord with arg as third."
  [thenote]
  (list (third-below thenote) thenote (fifth-above (third-below thenote))))

(defn as-fifth
  "Returns pitch-class chord with arg as fifth."
  [thenote]
  (list (fifth-below thenote) (third-above (fifth-below thenote)) thenote))

(defn ascend-list
  "Adds 12 to pitches lower than first one keeps chords in root position."
  [thelist]
  (let [root (first thelist)]
    (map (fn [x] (if (< x root) (+ x 12) x))
            thelist)))

(defn last-sol-test
  "Based on *last-sol*."
  []
  (let [test (first (top-n-positions *last-sol* 1))]
    (cond
     (= 0 test) '(0 1 1)
     (= 1 test) '(1 1 0)
     :else
     '(1 0 0))))

(defn maximum
  "Returns the maximum of its two args."
  [x y]
  (if (and x y)
    (max x  y)
    0))

(defn fz-u
  "Returns fuzzy union (maximums) of many lists."
  [the-lists]
  (if (empty? (rest the-lists))
    (first the-lists)
    (fz-un (first the-lists) (fz-u (rest the-lists)))))

(defn fz-union
  "Returns fuzzy union (maximums) of any number of lists"
  [&rest the-lists]
     (fz-u the-lists))

(defn fz-un
  "Returns fuzzy union (maximums) of two lists."
  [lst1 lst2]
  (if (empty? lst1)
    nil
    (cons (maximum (first lst1) (first lst2))
          (fz-union (rest lst1) (rest lst2)))))

(defn make-set [pitch-list]
  (if (empty? pitch-list) [0 0 0 0 0 0 0 0 0 0 0 0]
      (fz-union (ror-n *fz-note* (first pitch-list))
                (make-set (rest pitch-list)))))

(declare third-above fifth-above)

(defn as-root
  "Returns pitch-class chord with arg as root."
  [thenote]
  [ thenote (third-above thenote) (fifth-above thenote)])

(defn add-lists [list1 list2]
   "Add two lists, member by member"
     (map + list1 list2))

(defn favor-root-for-tonic
  "Gives slight edge to root position for tonic"
  [thePC]
   (if (= 0 thePC) '(0.1 0 0) '(0 0 0)))

(defn dither
  "Random tie breaker"
  []
  (let [choice (rand 3.0)]
    (cond
     (< choice 1) '(0.05 0 0)
     (< choice 2) '(0 0.05 0)
     :else
     '(0 0 0.05))))

(defn pick-chord-w-more-rules
    "Returns pitch-classes of a chord."
    [thenote]

    (let [the-pc (rem thenote 12)]
      (reset! *sol-set* [0 0 0])

      (reset! *as-root-set* (make-set (as-root the-pc)))

      (println "HERE?")

      (reset! *as-third-set* (make-set (as-third the-pc)))
      (reset! *as-fifth-set* (make-set (as-fifth the-pc)))
      (reset! *sol-set* (add-lists @*sol-set* (common-tones-test)))
      (reset! *sol-set* (add-lists @*sol-set* (last-sol-test)))
      (reset! *sol-set* (add-lists @*sol-set* (favor-root-for-tonic the-pc)))
      (reset! *sol-set* (add-lists @*sol-set* (dither)))

      (case (ltop (reset! *last-sol* @*sol-set*))
        (2 (as-fifth the-pc))
        (1 (as-third the-pc))
        :else
        (as-root the-pc))))

(defn add-oct
  "Adds octave to the list."
  ([theList] (add-oct theList 4))
  ([theList the-octave]
     (map (fn [x] (+ x (* the-octave 12)))
             (ascend-list theList))))

(defn make-chord-event
  "Simple notelist to chord."
  ([pitch-list] (make-chord-event pitch-list 0))
  ([pitch-list time]
      (if (empty? pitch-list) []
          (cons (list time (first pitch-list) *default-dur* *default-ch* *default-vel*)
                (make-chord-event (rest pitch-list) time)))))

(defn pick-and-play-more-rules-chord [thenote time]
    "Returns a chord in event notation."
    (make-chord-event
     (add-oct
      (pick-chord-w-more-rules (rem thenote 12)) 4) time))

(defn fuzzy
  ([pcs] (fuzzy pcs 0))
  ([pcs time]
     (if (empty? pcs) []
         (concat (pick-and-play-more-rules-chord (first pcs) time)
                 (fuzzy (rest pcs) (+ time 1000))))))

(defn compose []
  (fuzzy [0 4 7 5 7 11 0]))
