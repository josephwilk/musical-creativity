(ns musical-creativity.composers.improvise)

(def *new-work* ())
(def *the-last-first-choice* ())
(def *database-names* ())
(def *lexicons* ())
(def *groupings* ())
(def seed 1)
(def *grouping-names* ())
(def *first-groupings* ())
(def test ())
(def destination-name ())
(def name ())
(def tied-events ())
(def *save-groupings* ())

(def *channel-1* (atom 1))
(def *channel-2* (atom 1))
(def *channel-3* (atom 1))
(def *channel-4* (atom 1))
(def *channel-5* (atom 1))
(def *channel-6* (atom 1))
(def *channel-7* (atom 1))
(def *channel-8* (atom 1))
(def *channel-9* (atom 1))
(def *channel-10* (atom 1))
(def *channel-11* (atom 1))
(def *channel-12* (atom 1))
(def *channel-13* (atom 1))
(def *channel-14* (atom 1))
(def *channel-15* (atom 1))
(def *channel-16* (atom 1))

(reset! *channel-1* 1)
(reset! *channel-2* 1)
(reset! *channel-3* 1)
(reset! *channel-4* 1)
(reset! *channel-5* 1)
(reset! *channel-6* 1)
(reset! *channel-7* 1)
(reset! *channel-8* 1)
(reset! *channel-9* 1)
(reset! *channel-10* 1)
(reset! *channel-11* 1)
(reset! *channel-12* 1)
(reset! *channel-13* 1)
(reset! *channel-14* 1)
(reset! *channel-15* 1)
(reset! *channel-16* 1)

(defn all-equal [set-1 set-2]
  "this function voids the need to rebuild the database when improvise is called with the same names."
  (cond ((and (nil? set-1)(nil? set-2)) t)
        ((member (first set-1) set-2 :test #'equal)
         (all-equal (rest set-1)(remove (first set-1) set-2)))
        (:else ())))

(defn improvise-it []
  "recombines the groupings, applies a new overall duration set, and makes the data playable."
  (reset! *new-work*
        (reduce-ties (make-playable
                      (let* ((chosen-grouping
                              (choose-a-random-start-grouping *lexicons*))
                             (next-choice (destination (eval chosen-grouping))))
                        (if (equal next-choice 'end) (list chosen-grouping)
                            (cons chosen-grouping (sequence-through-groupings next-choice))))))))

(defn collect-groupings [events &optional (cut 0)]
  "top level function to collect groupings from the database."
  (if (nil? (find-next-new-ontime events))(list (list (list cut (+ (very-first events)(third (first events)))) events))
      (let* ((cutoff-time (find-next-new-ontime events))
             (grouping (get-all-simultaneous-attacks events))
             (clipped-grouping (clip cutoff-time grouping)))
        (cons (list (list (very-first events) cutoff-time) clipped-grouping)
              (collect-groupings (append (remainder cutoff-time grouping)
                                         (remove-all grouping events))
                                 cutoff-time)))))

(defn find-next-new-ontime [events &optional (time (very-first events))]
  "finds the next new ontime past the onset events."
  (cond ((nil? events)())
        ((> (very-first events) time) (very-first events))
        (:else (find-next-new-ontime (rest events) time))))

(defn get-all-simultaneous-attacks [events &optional (time (very-first events))]
  "returns all of the events with the same initial ontime at the nead of events."
  (if (or (nil? events)(not (equal time (very-first events)))) ()
      (cons (first events)
            (get-all-simultaneous-attacks (rest events) time))))

(defn clip [cutoff-time grouping]
  "clips the endings off of events which extend beyond the entrance of a new event."
  (cond ((or (nil? cutoff-time)(nil? grouping))())
        ((<= (+ (very-first grouping)(third (first grouping))) cutoff-time)
         (cons (first grouping)
               (clip cutoff-time (rest grouping))))
        (:else (cons (append (firstn 2 (first grouping))
                         (list (- cutoff-time (very-first grouping)))
                         (nthcdr 3 (first grouping))
                         (list 'tie))
                 (clip cutoff-time (rest grouping))))))

(defn remainder [cutoff-time grouping]
  "returns the remainder of the events which extend beyond the entrance of a new event."
  (cond ((nil? grouping)())
        ((<= (+ (very-first grouping)(third (first grouping))) cutoff-time)
         (remainder cutoff-time (rest grouping)))
        (:else (cons (append (list cutoff-time)
                         (list (second (first grouping)))
                         (list (- (third (first grouping))(- cutoff-time (very-first grouping))))
                         (nthcdr 3 (first grouping)))
                 (remainder cutoff-time (rest grouping))))))

(defclass lexicon []
  ((grouping-names :initarg :grouping-names :initform nil :accessor grouping-names)
   (last-choice :initarg :last-choice :initform nil :accessor last-choice))
  (:documentation "the top-level object which stores grouping names."))

(defclass grouping []
  ((name :initarg :name :initform nil :accessor name)
   (timing :initarg :timing :initform nil :accessor timing)
   (destination :initarg :destination :initform nil :accessor destination)
   (events :initarg :events :initform nil :accessor events)
   (lexicon :initarg :lexicon :initform nil :accessor lexicon))
  (:documentation "the object for storing groupings."))

(defn create-a-complete-database [names-of-eventlists]
  "top-level of the database creating program."
  (reset!  *database-names* (remove-duplicates (append names-of-eventlists *database-names*)))
  (loop for event-list-name in names-of-eventlists
        do (create-database-and-put-into-lexicons event-list-name (eval event-list-name)))
  t)

(defn create-database-and-put-into-lexicons [source events]
  "pujts the various data into each object and then the object itself into the proper lexicon."
   (reset! *groupings* (collect-groupings events))
   (create-database source)
   (loop for grouping in *grouping-names*
         do (let ((lexicon-name (make-name-of-lexicon (mapcar #'second (events (eval grouping))))))
              (if (boundp lexicon-name)
                (progn
                  (setf (grouping-names (eval lexicon-name))
                        (cons grouping (grouping-names (eval lexicon-name))))
                  (setf (lexicon (eval grouping)) lexicon-name))
                (progn (set lexicon-name
                            (make-instance 'lexicon :grouping-names (list grouping)))
                       (setf (lexicon (eval grouping)) lexicon-name)
                       (reset! *lexicons* (append *lexicons* (list lexicon-name)))))))
   *lexicons*)

(defn create-database [source &optional (beginning t)]
  "the low-level function for creating instances of grouping objects."
   (reset! *grouping-names* ())
   (reset! destination-name ())
   (let ((groupings *groupings*))
     (loop until (nil? groupings)
           do (reset! test groupings)
           do (reset! name (make-name-of-object source (mapcar #'second (second (first groupings)))))
           do (reset! destination-name (if (nil? (second groupings)) 'end
                                         (make-new-name-of-object source (mapcar #'second (second (second groupings))))))
           collect (set name
                        (make-instance 'grouping
                          :name source
                          :timing (first (first groupings))
                          :destination destination-name
                          :events (second (first groupings))))
           do (reset! *grouping-names* (append *grouping-names* (list name)))
           do (if beginning (progn (setf *first-groupings* (append *first-groupings* (list name)))
                                   (setf beginning ())))
           do (reset! groupings (rest groupings))))
   *grouping-names*)

(defn make-name-of-lexicon [pitches]
        " calling (make-name-of-lexicon (0))
            make-name-of-lexicon returned lexicon-[1]-0
           where the bracketed number is the order."
  (implode (append '(lexicon-) (interspace-hyphens pitches))))

(defn make-name-of-object [name pitches]
  "makes names for objects."
  (implode (append  (list name '[ (incf seed) '] '-)
                   (interspace-hyphens pitches))))

(defn make-new-name-of-object [name pitches]
  "creates the names of objects that follow other objects."
  (implode (append (list name '[ (1+ seed) '] '-)  (interspace-hyphens pitches))))

(defn interspace-hyphens [list]
  "places hyphens between the various symbols in its lits arg."
  (if (nil? (rest list)) list
      (append (list (first list) '-)
              (interspace-hyphens (rest list)))))

(defn remove-data []
  "cleans up databases for starting over."
  (reset! *first-groupings* ())
  (mapcar #'makunbound *lexicons*)
  (reset! *lexicons* ())
  (mapcar #'makunbound *grouping-names*)
  (reset! *grouping-names* ())
  (reset! *groupings* ())
  (reset! *save-groupings* ())
  (reset! *database-names* ())
  (reset! test ())
  (reset! name ()))

(defn choose-a-random-start-grouping [lexicons]
  "returns a randomly chosen object for begining a recombination."
  (reset! *the-last-first-choice*
        (choose-beginning-grouping
         (grouping-names (eval (choose (remove-ends lexicons)))))))

(defn choose-beginning-grouping [list]
  "chooses randomly from its list arg but avoids the end and rests."
  (let ((test (nth (random (length list) *rs*)
                   list)))
    (cond ((nil? (rest list)) (first list))
          ((and
            (not (equal (destination (eval test)) 'end))
            (not (zerop (get-first-pitch (events (eval test)))))
            (not (equal test (last-choice (eval (lexicon (eval test))))))
            (and (> (length list) 1)(not (equal *the-last-first-choice* test))))
           test)
          (:else (choose-beginning-grouping list)))))

(defn choose [list]
        "chooses randomly from its list arg."
  (nth (random (length list) *rs*)
                   list))

(defn remove-ends [lexicons]
  "removes lexicons that contain only final groupings."
  (cond ((nil? lexicons)())
        ((check-for-only-ends (grouping-names (eval (first lexicons))))
         (remove-ends (rest lexicons)))
        (:else (cons (first lexicons)(remove-ends (rest lexicons))))))

(defn check-for-only-ends [groupings]
  "checks to see if the grouping contains only ending objects."
  (cond ((nil? groupings) t)
        ((equal (destination (eval (first groupings))) 'end)
         (check-for-only-ends (rest groupings)))
        (:else ())))

(defn sequence-through-groupings [choice]
  "collects properly connected groupings."
  (cond ((equal choice 'end)())
        ((equal (destination (eval choice)) 'end)
         (list choice))
        (:else (let ((new-choice (select choice)))
             (cons new-choice
                   (sequence-through-groupings (destination (eval new-choice))))))))

(defn select [choice]
  "selects randomly from objects in the same lexicon."
  (if (zerop (get-first-pitch (events (eval choice))))
    choice
    (choose-one (grouping-names (eval (lexicon (eval choice)))))))

(defn reduce-ties [events]
  "connects tied events and returns their joined composites."
  (loop until (nil? events)
        do (setf tied-events (if (equal (my-last (first events)) 'tie)
                              (get-complementary-events (first events)(rest events))))
        collect (if tied-events (progn (setf events (cons (first events)
                                                         (remove-all tied-events (rest events))))
                                      (butlast (add-them (first events) tied-events)))
                    (first events))
        do (setf events (rest events))))

(defn add-them [event events]
  "creates one event from two based on tie."
  (append (firstn 2 event)
          (list (apply #'+ (mapcar #'third (cons event events))))
          (nthcdr 3 event)))

(defn get-complementary-events [event events]
  "finds the complementary event to one with a tie as its final element."
  (cond ((nil? events) nil)
        ((and
          (equal (second event)(get-first-pitch events))
          (within-range (+ (first event)(third event))
                        (list (very-first events) t (very-first events)))
          (equal (my-last event) 'tie))
         (cons (first events)
               (get-complementary-events (first events) (rest events))))
        (:else (get-complementary-events event (rest events)))))

(defn within-range [number range]
  "returns t if the number is within or equal to the boundaries of the range arg."
  (if (and (>= number (first range))
           (<= number (third range))) t))

(defn remove-all [ remove-events events]
  "a non-destructive way to remove a series of events from a list
          of events."
  (if (nil? remove-events) events
      (remove-all (rest remove-events)
                  (remove-it (first remove-events) events))))

(defn remove-it [event events]
  "removes the first arg from the second arg once based on the first two elements."
  (cond ((nil? events)())
        ((and
          (equal (first event)(very-first events))
          (equal (second event)(get-first-pitch events)))
         (rest events))
        (:else (cons (first events)(remove-it event (rest events))))))

(defn get-first-pitch [events]
  "returns the first pitch in events."
  (second (first events)))

(defn make-playable [contiguous-groupings]
  "makes the object groupings into playable events as well as recombining them with a different database timing sequence. "
  (reduce-ties
   (sortcar #'<
            (apply #'append
                   (set-timings (mapcar #'first
                                        (collect-groupings (eval (choose-one *database-names*))))
                                (mapcar #'(lambda (x)(timing (eval x))) contiguous-groupings)
                                (mapcar #'(lambda (x)(events (eval x))) contiguous-groupings))))))

(defn set-timings [new-timings old-timings groupings &optional (current-time 0)]
  "resets the timings of the groupings so they will play consecutively."
  (if (or (nil? new-timings)(nil? groupings)(nil? (second (first new-timings))))()
      (cons (mapcar #'(lambda (x)(append (list current-time)
                                         (list (second x))
                                         (list (* (/ (third x)(- (second (first old-timings))(first (first old-timings))))
                                                  (- (second (first new-timings))(first (first new-timings)))))
                                         (nthcdr 3 x)))
                    (first groupings))
            (set-timings (rest new-timings)
                         (rest old-timings)
                         (rest groupings)
                         (+ current-time (- (second (first new-timings))(very-first new-timings)))))))


(defn improvise [databases]
  "this fcn creates a new database if necessary (i.e., new names in its arg) and runs improvise-it."
  (if (all-equal databases *database-names*)
    (improvise-it)
    (progn (create-a-complete-database databases)
           (improvise-it))))
