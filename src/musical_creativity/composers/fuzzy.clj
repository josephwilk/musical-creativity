(ns musical-creativity.composers.fuzzy)

; idevault values
(def *default_dur* 1000)
(def *default_ch* 1)
(def *default_vel* 127)

; standard scales, chords, and notes
(def *maj_scale* [1 0 1 0 1 1 0 1 0 1 0 1])
(def *nmin_scale* [1 0 1 1 0 1 0 1 1 0 1 0])
(def *hmin_scale* [1 0 1 1 0 1 0 1 1 0 0 1])
(def *chord_template* [1 0 0 1 1 0 0 1 0 0 0 0])
(def *fz_triad* [1 0 0 1 1 0 0.5 1 0 0 0 0])
(def *fz_key* [1 0 1 0 1 1 0 1 0 1 0 1])
(def *fz_note* [1 0 0 0 0 0 0 0 0 0 0 0])
(def *current_scale* *maj_scale*)

; intervals up
(def *fz_2* [0 0.9 1 0 0 0 0 0 0 0 0 0])
(def *fz_3* [0 0 0 1 0.9 0 0 0 0 0 0 0])
(def *fz_4* [0 0 0 0 0 1 0.5 0 0 0 0 0])
(def *fz_5* [0 0 0 0 0 0 0.9 1 0.5 0 0 0])
(def *fz_6* [0 0 0 0 0 0 0 0 1 0.9 0 0])
(def *fz_7* [0 0 0 0 0 0 0 0 0 0 0.9 1])

;intervals down
(def *fz_2b* [0 0 0 0 0 0 0 0 0 0 1 0.9])
(def *fz_3b* [0 0 0 0 0 0 0 0 0.9 1 0 0])
(def *fz_4b* [0 0 0 0 0 0 0.5 1 0 0 0 0])
(def *fz_5b* [0 0 0 0 0.5 1 0.9 0 0 0 0 0])
(def *fz_6b* [0 0 0.9 1 0 0 0 0 0 0 0 0])
(def *fz_7b* [0 0.9 1 0 0 0 0 0 0 0 0 0])

(def *sol_set* [0 0 0])
(def *last_sol* [0 0 0])
(def *old_chord_set* [0 0 0 0 0 0 0 0 0 0 0 0])
(def *as_root_set* (atom [0 0 0 0 0 0 0 0 0 0 0 0]))
(def *as_third_set* [0 0 0 0 0 0 0 0 0 0 0 0])
(def *as_fifth_set* [0 0 0 0 0 0 0 0 0 0 0 0])

; pitch classes
(def PC_C 0)
(def PC_DFLAT 1)
(def PC_D 2)
(def PC_EFLAT 3)
(def PC_E 4)
(def PC_F 5)
(def PC_GFLAT 6)
(def PC_G 7)
(def PC_AFLAT 8)
(def PC_A 9)
(def PC_BFLAT 10)
(def PC_B 11)

(declare common_tone_rules)

(defn fz_complement
  "Returns fuzzy complement (1 - membership) of a list."
  ([alist] (fz_complement alist 1.0))
  ([alist compvalue]
     (if (empty? alist)
       nil
       (cons (- 1 (first alist))
             (fz_complement (rest alist) compvalue)))))

(defn fz_intersect
  "Returns fuzzy intersection (minimums) of two lists"
  [lst1 lst2]
  (if (empty? lst1)
    nil
    (cons (minumum (first lst1) (first lst2))
          (fz_intersect (rest lst1) (rest lst2)))))


(defn common_tones_test
  "Applies common tone rules."
  []
  (common_tone_rules *as_root_set* *as_third_set* *as_fifth_set* *old_chord_set*))

(defn common_tones
  "Sums the set intersections."
  [set1 set4]
  (sumup (fz_intersect set1 set4)))

(defn common_tone_rules
  "Returns set according to common tone rules."
  [set1 set2 set3 master]
  (let [result [0 0 0]
        result (assoc result 0 (common_tones set1 master))
        result (assoc result 1 (common_tones set2 master))
        result (assoc result 2 (common_tones set3 master))]
    result))


(defn ltop
  "Returns the topmost position of the list."
  [thelist]
  (first (top_n_positions thelist 1)))

(defn third_above
  "Returns a third above the note."
  [thenote]
  (first (top_n_positions (fz_intersect *current_scale* (ror_n *fz_3* thenote)) 1)))

(defn fifth_above
  "Returns a fifth above the note."
  [thenote]
  (first (top_n_positions (fz_intersect *current_scale* (ror_n *fz_5* thenote)) 1)))

(defn third_below
  "Returns a third below the note."
  [thenote]
  (first (top_n_positions (fz_intersect *current_scale* (ror_n *fz_3b* thenote)) 1)))

(defn fifth_below
  "Returns a fifth below the note."
  [thenote]
  (first (top_n_positions (fz_intersect *current_scale* (ror_n *fz_5b* thenote)) 1)))

(defn as_third
  "Returns pitch-class chord with arg as third."
  [thenote]
  (list (third_below thenote) thenote (fifth_above (third_below thenote))))

(defn as_fifth
  "Returns pitch-class chord with arg as fifth."
  [thenote]
  (list (fifth_below thenote) (third_above (fifth_below thenote)) thenote))

(defn ascend_list
  "Adds 12 to pitches lower than first one keeps chords in root position."
  [thelist]
  (let [root (first thelist)]
    (map (fn [x] (if (< x root) (+ x 12) x))
            thelist)))

(defn ror_n
  "Rotates 12 element list n steps to right."
  [lst n]
  (let [shift_by (- 12 n)]
      (concat (subseq lst shift_by) (subseq lst 0 shift_by))))

(defn sumup
  "Adds the members of a list."
  [the-list]
  (if (empty? the-list)
    0
    (+ (first the-list) (sumup (rest the-list)))))

(defn top_n_positions
  "Returns positions of highest n values in list, or nil if all 0."
  [the-list howmany]
    (if (= 0 howmany)  ; finished
      nil
      (if (nil? (sumup the-list)) ; empty list
        nil
        (let [alist the-list]
;          (dosync (alter  alist (copy-list the-list)))  ;   copy input list
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
            (top_n_positions alist (- howmany 1)))
           #'<)))))

(defn last_sol_test
  "Based on *last_sol*."
  []
  (let [test (first (top_n_positions *last_sol* 1))]
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

(defn minumum
  "Returns the minimum of its two args."
  [x y]
  (if (and x y)
    (min x  y)
    0))

(defn fz_un
  "Returns fuzzy union (maximums) of two lists."
  [lst1 lst2]
  (if (empty? lst1)
    nil
    (cons (maximum (first lst1) (first lst2))
          (fz_union (rest lst1) (rest lst2)))))
(defn fz_u
  "Returns fuzzy union (maximums) of many lists."
  [the-lists]
  (if (empty? (rest the-lists))
    (first the-lists)
    (fz_un (first the-lists) (fz_u (rest the-lists)))))

(defn fz_union
  "Returns fuzzy union (maximums) of any number of lists"
  [&rest the-lists]
     (fz_u the-lists))


(defn make_set [pitch_list]
  (if (empty? pitch_list) [0 0 0 0 0 0 0 0 0 0 0 0]
      (fz_union (ror_n *fz_note* (first pitch_list))
                (make_set (rest pitch_list)))))

(declare third_above fifth_above)

(defn as_root
  "Returns pitch-class chord with arg as root."
  [thenote]
  (list thenote (third_above thenote) (fifth_above thenote)))

(defn add_lists [list1 list2]
   "Add two lists, member by member"
     (map + list1 list2))

(defn favor_root_for_tonic
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

(defn pick_chord_w_more_rules [thenote]
    "Returns pitch-classes of a chord."
    (let [the_pc (rem thenote 12)
          solSet [0 0 0]]
      (reset! *as_root_set* (make_set (as_root the_pc)))
      (reset! *as_third_set* (make_set (as_third the_pc)))
      (reset! *as_fifth_set* (make_set (as_fifth the_pc)))
      (reset! solSet (add_lists solSet (common_tones_test)))
      (reset! solSet (add_lists solSet (last_sol_test)))
      (reset! solSet (add_lists solSet (favor_root_for_tonic the_pc)))
      (reset! solSet (add_lists solSet (dither)))
      (case (ltop (reset! *last_sol* solSet))
        (2 (as_fifth the_pc))
        (1 (as_third the_pc))
        :else
        (as_root the_pc))))

(defn add_oct
  "Adds octave to the list."
  ([theList] (add_oct theList 4))
  ([theList the-octave]
     (map (fn [x] (+ x (* the-octave 12)))
             (ascend_list theList))))

(defn make_chord_event
  "Simple notelist to chord."
  ([pitch_list] (make_chord_event pitch_list 0))
  ([pitch_list time]
      (if (empty? pitch_list) []
          (cons (list time (first pitch_list) *default_dur* *default_ch* *default_vel*)
                (make_chord_event (rest pitch_list) time)))))

(defn pick_and_play_more_rules_chord [thenote time]
    "Returns a chord in event notation."
    (make_chord_event
     (add_oct
      (pick_chord_w_more_rules (rem thenote 12)) 4) time))

(defn fuzzy
  ([pcs] (fuzzy pcs 0))
  ([pcs time]
     (if (empty? pcs) []
         (concat (pick_and_play_more_rules_chord (first pcs) time)
                 (fuzzy (rest pcs) (+ time 1000))))))
