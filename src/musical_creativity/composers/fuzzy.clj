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
(def *as_root_set* [0 0 0 0 0 0 0 0 0 0 0 0])
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

(defn fuzzy [pcs & [time 0]]
      (if (null pcs) ()
          (append (pick_and_play_more_rules_chord (first pcs) time)
                  (fuzzy (rest pcs) (+ time 1000)))))

(defn pick_and_play_more_rules_chord [thenote time]
    "Returns a chord in event notation."
          (make_chord_event
           (add_oct
            (pick_chord_w_more_rules (rem thenote 12)) 4) time))

(defn add_lists [list1 list2]
   "Add two lists, member by member"
     (mapcar #'+ list1 list2))

(defn pick_chord_w_more_rules [thenote]
    "Returns pitch-classes of a chord."
    (let ((the_pc (rem thenote 12)) (solSet '(0 0 0)))
      (setq *as_root_set* (make_Set (as_root the_pc)))
      (setq *as_third_set* (make_Set (as_third the_pc)))
      (setq *as_fifth_set* (make_Set (as_fifth the_pc)))
      (setq solSet (add_lists solSet (common_tones_test)))
      (setq solSet (add_lists solSet (last_sol_test)))
      (setq solSet (add_lists solSet (favor_root_for_tonic the_pc)))
      (setq solSet (add_lists solSet (dither)))
      (case (ltop (setq *last_sol* solset))
        (2 (as_fifth the_pc))
        (1 (as_third the_pc))
        (otherwise (as_root the_pc)))))

(defn make_set [pitch_list]
         (if (null pitch_list) '(0 0 0 0 0 0 0 0 0 0 0 0)
             (fz_union (ror_n *fz_note* (first pitch_list))
                       (make_set (rest pitch_list)))))

(defn ror_n
  "Rotates 12 element list n steps to right."
  [lst n]
    (let ((shift_by (- 12 n)))
      (append (subseq lst shift_by) (subseq lst 0 shift_by))))

(defn fz_union 
  "Returns fuzzy union (maximums) of any number of lists"
  [&rest theLists]
     (fz_u theLists))

(defn favor_root_for_tonic
  "Gives slight edge to root position for tonic"
  [thePC]
   (if (zerop thePC) '(0.1 0 0) '(0 0 0)))

(defn dither 
  "Random tie breaker"
  []
     (let ((choice (random 3.0)))
       (cond
        ((< choice 1) '(0.05 0 0))
        ((< choice 2) '(0 0.05 0))
        (t '(0 0 0.05)))))

(defn common_tones_test 
  "Applies common tone rules."
  []
       (common_tone_rules *as_root_set* *as_third_set* *as_fifth_set* *old_chord_set*))

(defn common_tone_rules
  "Returns set according to common tone rules."
  [set1 set2 set3 master]
       (let ((result '(0 0 0)))
         (setf (first result) (common_tones set1 master))
         (setf (second result) (common_tones set2 master))
         (setf (third result) (common_tones set3 master))
         result))

(defn common_tones 
  "Sums the set intersections."
  [set1 set4]
       (sumup (fz_intersect set1 set4)))

(defn ltop 
  "Returns the topmost position of the list."
  [thelist]
    (car (top_n_positions thelist 1)))

(defn fz_complement 
  "Returns fuzzy complement (1 - membership) of a list."
  [alist & [compvalue 1.0]]
       (if (null alist)
         nil
         (cons (- 1 (first alist))
               (fz_complement (rest alist) compvalue))))

(defn fz_intersect 
  "Returns fuzzy intersection (minimums) of two lists"
  [lst1 lst2]
       (if (null lst1)
         nil
         (cons (minumum (first lst1) (first lst2))
               (fz_intersect (rest lst1) (rest lst2)))))

(defn maximum 
  "Returns the maximum of its two args."
  [x y]
     (if (and (realp x) (realp y))
              (max x  y)
              0))

(defn minumum
  "Returns the minimum of its two args."
  [x y]
     (if (and (realp x) (realp y))
              (min x  y)
              0))

(defn fz_un 
  "Returns fuzzy union (maximums) of two lists."
  [lst1 lst2]
       (if (null lst1)
         nil
         (cons (maximum (first lst1) (first lst2))
               (fz_union (rest lst1) (rest lst2)))))

(defn fz_u 
  "Returns fuzzy union (maximums) of many lists."
  [theLists]
     (if (null (rest theLists))
               (first thelists)
               (fz_un (first theLists) (fz_u (rest theLists)))))

(defn sumup 
  "Adds the members of a list."
  [theList]
     (if (null thelist)
       0
       (+ (first theList) (sumup (rest theList)))))

(defn top_n_positions 
  "Returns positions of highest n values in list, or nil if all 0."
  [thelist howmany]
    (if (zerop howmany)  ; finished
      nil
      (if (null (sumup theList)) ; empty list
        nil
        (let (alist)
          (setf alist (copy-list thelist))  ;   copy input list
          (sort
           (cons
            (let ( ( i 0) (k 0) (hsf 0)) ;  hsf is highest so far
              (dolist (x alist)
                (if (> x hsf)
                  (setf hsf x k i))
                (setf i (+ i 1)))
              (setf (elt alist k) 0)
              k) ; k is consed
            (top_n_positions alist (1- howmany)))
           #'<)))))

(defn ascend_list
  "Adds 12 to pitches lower than first one keeps chords in root position."
  [thelist]
       (let ((root (first thelist)))
         (mapcar #'(lambda (x) (if (< x root) (+ x 12) x))
                 thelist)))

(defn add_oct 
  "Adds octave to the list."
  [theList & [theOctave 4]]
    (mapcar #'(lambda (x) (+ x (* theoctave 12)))
            (ascend_list theList)))

(defn make_chord_event 
  "Simple notelist to chord."
  [pitch_list &optional (time 0)]
    (if (null pitch_list) ()
        (cons (list time (first pitch_list) *default_dur* *default_ch* *default_vel*)
              (make_chord_event (rest pitch_list) time))))

(defn third_above 
  "Returns a third above the note."
  [thenote]
    (car (top_n_positions (fz_intersect *current_scale* (ror_n *fz_3* thenote)) 1)))

(defn fifth_above 
  "Returns a fifth above the note."
  [thenote]
    (car (top_n_positions (fz_intersect *current_scale* (ror_n *fz_5* thenote)) 1)))

(defn third_below 
  "Returns a third below the note."
  [thenote]
    (car (top_n_positions (fz_intersect *current_scale* (ror_n *fz_3b* thenote)) 1)))

(defn fifth_below
  "Returns a fifth below the note."
  [thenote]
    (car (top_n_positions (fz_intersect *current_scale* (ror_n *fz_5b* thenote)) 1)))

(defn as_root 
  "Returns pitch-class chord with arg as root."
  [thenote]
    (list thenote (third_above thenote) (fifth_above thenote)))

(defn as_third 
  "Returns pitch-class chord with arg as third."
  [thenote]
    (list  (third_below thenote) thenote (fifth_above (third_below thenote))))

(defn as_fifth 
  "Returns pitch-class chord with arg as fifth."
  [thenote]
    (list (fifth_below thenote) (third_above (fifth_below thenote)) thenote))

(defn last_sol_test 
  "Based on *last_sol*."
  []
       (let ((test (car (top_n_positions *last_sol* 1))))
         (cond
          ((= 0 test) '(0 1 1))
          ((= 1 test) '(1 1 0))
          (t '(1 0 0)))))
