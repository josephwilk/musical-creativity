(ns musical-creativity.composers.chorale
  (:require
   [clojure.math.numeric-tower :as math]
   [data.chorale :as chorale]
   [clojure.string :as str]))

(def *composer* 'bach)

(def *beats-store* (atom {}))
(def *lexicon-store* (atom {}))

(def *lexicons* (atom []))

(def *rules-storage* (atom ()))

(def *mix-names* ())
(def *mix* ())

(def bach-dominants-tonics ())

(def bach-dominants ())
(def bach-tonics ())

(def bach-start-beats (atom ()))
(def bach-compose-beats (atom ()))
(def bach-rules (atom ()))

(def *end* (atom false))
(def *history* (atom ()))

(def *events* (atom ()))
(def *save-events* (atom ()))

(def *tonic* (atom 'major))
(def *early-exit?* (atom false))

(def *compose-number* 0)
(def *histories* ())

(def *previous-beat* (atom nil))

(def *beat-size* 1000)

(def bach ['bach-compose-beats 'bach-start-beats 'bach-rules])
(def *beats* 4)
(def bach-form [])

(declare get-rule)

(defn my-remove [objects-to-be-remove list-of-objects]
  (remove #(some #{%} objects-to-be-remove) list-of-objects))

(defn position [thing list]
  (let [index (.indexOf list thing)]
    (when (>= index 0) index)))

(defn choose-one [list]
  (nth list (rand-int (count list))))

(defn implode [list]
  (str/join "" list))

(defn explode [atom]
  (vec atom))

(defn make-instance [type attributes]
  attributes)

(defn my-push [stuff place-name]
  "A simple synonym for push."
  (reset! place-name (cons stuff @place-name)))

(defn make-name [db-name counter]
  "Simple synonym for imploding the database name and number."
  (symbol (str (name  db-name)  "-" counter)))

(defn hyphenate [note-numbers]
  "Hyphenates the numbers in its arg."
  (if (empty? note-numbers)()
      (concat (if (empty? (rest note-numbers))
                (list (first note-numbers))
                (list (first note-numbers) '-))
              (hyphenate (rest note-numbers)))))

(defn get-onset-notes
  "Gets the onset pitches for its arg."
  [events]
  (let [onbeat (ffirst events)
        onbeat-events (filter (fn [event]
                                (= (first event) onbeat)) events)]
    (map #(second %) onbeat-events)))

(defn sort-by-first-element [lists]
  (sort (fn [[x & _] [y & _]] (< x y))  lists))

(defn set-to-zero
  "Sets the events to zero."
  ([events] (set-to-zero events (ffirst events)))
  ([events subtract]
     (if (empty? events)
       ()
       (cons (cons (- (ffirst events) subtract)
                   (rest (first events)))
             (set-to-zero (rest events) subtract)))))

(defn my-last [list]
  "Returns th atom last of the list."
  (first (last list)))

(defn a-thousand? [number]
  "Returns the number under 1000."
  (if (= 0 (mod number 1000)) true))

(defn fourth [list]
  (nth list 3))

(defn third [list]
  (nth list 2))

(defn member [value list]
  (if (seq list)
    (if (= value (first list))
      list
      (recur value (rest list)))))

(defn remove-nils [list]
  (remove #(nil? %) list))

(defn plot-timings [events]
  "Plots out the times of each beat."
  (if (empty? events)()
      (cons (list (fourth (first events))(+ (ffirst events)(third (first events))))
            (plot-timings (rest events)))))

(defn make-lists-equal [lists]
  "Ensures the two lists are equal in length."
  (cond
   (> (count (first lists)) (count (second lists)))
   (list (take (count (second lists)) (first lists)) (second lists))
   (> (count (second lists)) (count (first lists)))
   (list (first lists) (take (count (first lists)) (second lists)))
   :else
   lists))

(defn reduce-interval [interval]
  "Reduces the interval mod 12."
  (cond
   (<= (math/abs interval) 12)
   interval
   (< interval 0)
   (reduce-interval (+ interval 12))
        :else
        (reduce-interval (- interval 12))))

(defn get-rules1 [start-notes destination-notes name]
  "Does the grunt work for get-rules."
  (if (or (empty? (rest start-notes))(empty? (rest destination-notes)))
    (reverse @*rules-storage*)
    (do
      (reset! *rules-storage* (concat (reverse (get-rule (- (first destination-notes) (first start-notes))
                                                         (first start-notes) start-notes destination-notes name)) @*rules-storage*))
      (get-rules1 (rest start-notes) (rest destination-notes) name))))


(defn get-rule [voice start-note start-notes destination-notes name]
  "Gets the rule between first two args."
  (if (or (empty? (rest start-notes))(empty? destination-notes))()
      (cons (list (reduce-interval (- (second start-notes) start-note))
                  voice
                  (- (second destination-notes) (second start-notes))
                  name)
            (get-rule voice start-note (rest start-notes)(rest destination-notes) name))))

(defn get-rules [start-notes destination-notes name]
  "Gets the intervals between adjacent sets of the two args."
  (reset! *rules-storage* ())
  (let [test (make-lists-equal (list start-notes destination-notes))]
    (get-rules1 (first test)(second test) name)))

(defn boundp [thing]
  (nil? thing))

(defn swap-unless-includes [reference data]
  (when-not (some #{data} @reference)
    (swap! reference conj data)))

(defn find-beat [name]
  (@*beats-store* name))

(defn find-in-lexicon [name]
  (@*lexicon-store* name))

(defn lexicon-contains? [lexicon-name]
  "Sees if the lexicon exists."
  (contains? @*lexicon-store* lexicon-name))

(defn make-lexicon-name
  "Creates the appropriate lexicon name for the object."
  ([note-numbers] (make-lexicon-name note-numbers *mix-names*))
  ([note-numbers names]
     (cond
      (empty? *mix*)
      (implode (cons *composer* (cons '- (hyphenate note-numbers))))
      (empty? names)
      (implode (cons *composer* (cons '- (hyphenate note-numbers))))
      (boundp (implode (cons (first names) (cons '- (hyphenate note-numbers)))))
      (implode (cons (first names) (cons '- (hyphenate note-numbers))))
      :else
      (make-lexicon-name note-numbers (shuffle (rest names))))))

(defn put-beat-into-lexicon
  "Puts the beat arg into the appropriate lexicon."
  [beat-name]
  (let [beat (find-beat beat-name)
        lexicon-name (make-lexicon-name (:start-notes beat))]
    (if (and (lexicon-contains? lexicon-name)
             (not (member beat-name (:beats (find-in-lexicon lexicon-name)))))
      (do
        (reset! *lexicon-store* (update-in @*lexicon-store* [lexicon-name :beats] conj beat-name))
        lexicon-name)
      (do
        (reset! *lexicon-store* (assoc @*lexicon-store* lexicon-name (make-instance 'lexicon {:beats (list beat-name)})))
        (swap-unless-includes *lexicons* lexicon-name)
        lexicon-name))))

(defn return-beat
  "Returns the beat number of the initiating event."
  ([channel-events] (return-beat channel-events (ffirst channel-events)))
  ([channel-events start-time]
     (cond (empty? channel-events) nil
           (and (a-thousand? (ffirst channel-events))
                (not (= start-time (ffirst channel-events))))
           (/ (- (ffirst channel-events) start-time) 1000)
           :else (return-beat (rest channel-events)
                          start-time))))

(defn create-pc-set [pitches]
  "Creates a full PC set."
  (if (empty? pitches) ()
      (cons (mod (first pitches) 12)
            (create-pc-set (rest pitches)))))

(defn create-pitch-class-set [pitches]
  "Sorts and gets a full pc-set."
  (sort < (distinct (create-pc-set pitches))))

(defn get-channel-numbers-from-events
  "simply gets the channel numbers from the music"
  ([events] (get-channel-numbers-from-events events []))
  ([events channels]
      (cond (empty? events) channels
            (not (member (fourth (first events)) channels))
            (get-channel-numbers-from-events (rest events) (cons (fourth (first events)) channels))
            :else (get-channel-numbers-from-events (rest events) channels))))

(defn collect-timings-by-channel [timings channel]
  "collects the timings of the channel indicated in second arg"
  (cond (empty? timings)()
        (= (ffirst timings) channel)
        (cons (first timings)
              (collect-timings-by-channel (rest timings) channel))
        :else (collect-timings-by-channel (rest timings) channel)))

(defn find-alignment [point channel]
  (cond (empty? channel)()
        (and (a-thousand? point)
             (filter #(= point (first %)) (map reverse channel)))
        true
        :else (find-alignment point (rest channel))))

(defn find-alignment-in-all-channels [point channels]
  "run this on the channels of the channel-point-lists"
  (cond
   (empty? channels)
   point
   (nil? point)
   point
   (find-alignment point (first channels))
   (find-alignment-in-all-channels point (rest channels))
   :else
   ()))

(defn all-together [channel channels]
  "Returns the appropriate channel timing."
  (cond
   (empty? channel)
   (second (my-last (my-last channels)))
   (find-alignment-in-all-channels (second (first channel)) channels)
   (find-alignment-in-all-channels (second (first channel)) channels)
   :else
   (all-together (rest channel) channels)))

(defn first-place-where-all-together [events]
  "This looks ahead to get the first time they end together"
  (let [test (plot-timings events)
        channels (get-channel-numbers-from-events events)
        ordered-timings-by-channel (map (fn [channel] (collect-timings-by-channel test channel)) channels)]

    (all-together (first ordered-timings-by-channel)
                  (rest ordered-timings-by-channel))))

(defn collect-by-timing [timing events]
  "Collects the events accoring to timing."
  (filter (fn [event]
            (<= (+ (first event) (fourth event)) timing))
          events))

(defn collect-beats [events]
   (if (empty? events)
    ()
    (let [sync-time (first-place-where-all-together events)
          test (collect-by-timing sync-time events)
          reduced-test (drop (count test) events)]
      (cons test
              (collect-beats reduced-test)))))

(defn make-beat [name beats]
  (let [start-notes (get-onset-notes (first beats))
        destination-notes (get-onset-notes (second beats))
        events (first beats)
        rules (cons (get-rules start-notes destination-notes name)
                    (list name (ffirst (sort-by-first-element events))))
        composer-rules (eval (symbol (str *composer* "-rules")))]

    (my-push rules composer-rules)

    (make-instance 'beat-it {:start-notes start-notes
                             :destination-notes destination-notes
                             :events events
                             :voice-leading (first rules)})))

(defn- find-composer-beats-atom []
  (var-get (resolve (symbol (str "musical-creativity.composers.chorale/" *composer* "-compose-beats")))))

(defn- find-composer-start-beats-atom []
  (eval (str *composer* "-compose-beats")))

(defn start-beats [db-name]
  (remove-nils
   (collect-beats
    (set-to-zero
     (sort-by-first-element (chorale/find-db db-name))))))

(defn create-complete-database
  ([db-names] (create-complete-database db-names 1))
  ([db-names counter]
     (if (empty? db-names)
       true
       (do
         (loop [beats (start-beats (first db-names))
                counter counter
                start true]

           (when-not (empty? beats)
             (let [name (make-name (first db-names) counter)
                   instance (make-beat name beats)]
               (reset! *beats-store* (assoc @*beats-store* name instance))
               (put-beat-into-lexicon name)
               (my-push name (find-composer-beats-atom))

               (when start
                 (my-push name (eval (symbol (str *composer* '- 'start-beats)))))

               (print ".")
               (flush)

               (recur (rest beats) (+ 1 counter) nil))))
         (create-complete-database (rest db-names))))))

(defn on-beat [events ontime]
  "Returns t if the events conform to ontime."
  (cond
   (empty? events)
   true
   (and (a-thousand? (ffirst events))(= (ffirst events) ontime))
   (on-beat (rest events) ontime)
   :else
   ()))

(defn get-on-beat
  "Returns the on beat from the events."
  [events ontime]
  (cond
   (empty? events)
   ()
   (and
    (a-thousand? (ffirst events))
    (= (ffirst events) ontime))
   (cons (first events)
         (get-on-beat (rest events) ontime))
   :else
   ()))

(defn get-pitches [events]
  "Gets the pitches from its arg."
  (map #(second %) events))

(defn project
  "Projects the pc-set through its inversions."
  ([set] (project set (count set) 0))
  ([set length times]
      (if (= length times)()
          (cons set
                (project (concat (rest set)(list (+ 12 (first set)))) length (+ 1 times))))))

(defn get-interval [set]
  "Returns the intervals between set members."
  (if (empty? (rest set))
    () (cons (- (second set)(first set))
             (get-interval (rest set)))))

(defn get-intervals [sets]
  "Returns the intervals in the sets."
  (if  (empty? sets)()
       (cons (math/abs (apply #'+ (get-interval (first sets))))
             (get-intervals (rest sets)))))

(defn get-smallest-set [set]
  "Returns the set with the smallest outer boundaries."
  (let [projected-sets (project set)
        set-differentials (get-intervals projected-sets)
        sorted-differentials (sort < set-differentials)]
    (nth projected-sets (position (first sorted-differentials) set-differentials))))

(defn triad? [events]
  "Checks to see if the events are a triad."
  (when-not (empty? events)
    (let [pitches (get-pitches events)
          pitches-class-set (create-pitch-class-set pitches)
          pitch-classes (get-smallest-set pitches-class-set)]
      (and (= (count pitch-classes) 3)
           (and (> (- (second pitch-classes)(first pitch-classes)) 2)
                (< (- (second pitch-classes)(first pitch-classes)) 5))
           (and (> (- (third pitch-classes)(second pitch-classes)) 2)
                (< (- (third pitch-classes)(second pitch-classes)) 5))))))

(defn members-all [arrows target]
  "Checks to see if arrows are all in target."
  (cond (empty? arrows) true
        (member (first arrows) target)
        (members-all (rest arrows) target)
        :else ()))

(defn find-triad-beginning
  "Returns the db with a triad beginning."
  []
  (let [test (choose-one @(eval (first (eval *composer*))))
        beat (find-beat test)
        on-beat (get-on-beat (:events beat) (ffirst (:events beat)))
        pcs (create-pitch-class-set (get-pitches on-beat))]
    (if (and (triad? on-beat)
             (or (members-all '(0 4 8) pcs)
                 (members-all '(0 4 7) pcs)
                 (members-all '(0 5 8) pcs)
                 (members-all '(2 7 11) pcs))
             (<= (third (first (:events beat))) 1000)
             (= (count (:events beat)) 4))
      test
      (recur))))

(defn chop
  "Chops beats over 1000 into beat-sized pieces."
  ([event] (chop event (first event) (third event)))
  ([event begin-time duration]
      (if (< duration 1000)()
          (cons (concat (list begin-time)
                        (list (second event))
                        '(1000)
                        (drop 3 event))
                (chop event (+ begin-time 1000)(- duration 1000))))))

(defn remainder
  "Returns the remainder of the beat."
  ([event] (remainder event (first event) (third event)))
  ([event begin-time duration]
     (cond (empty? event)()
           (= duration 1000)()
           (< duration 1000) (list (concat (list begin-time)
                                           (list (second event))
                                           (list duration)
                                           (drop  3 event)))
           :else (remainder event (+ begin-time 1000)(- duration 1000)))))

(defn get-full-beat
  "Returns one full beat of the music."
  ([events] (get-full-beat events (ffirst events) 0))
  ([events begin-time duration]
     (cond (empty? events)()
           (= (+ duration (third (first events))) 1000)
           (list (first events))
           (> (+ duration (third (first events))) 1000)
           (list (concat (take 2 (first events))
                         (list (- 1000 duration))
                         (drop  3 (first events))))
           :else (cons (first events)
                   (get-full-beat (rest events)
                                  (+ begin-time (third (first events)))
                                  (+ (third (first events)) duration))))))

(defn get-channel [n music]
  "Gets the nth channel of the music."
  (cond (empty? music)()
        (= (fourth (first music)) n)
        (cons (first music)(get-channel n (rest music)))
        :else (get-channel n (rest music))))

(defn remainders
  "Returns remainders of beats."
  ([events] (remainders events (ffirst events) 0))
  ([events begin-time duration]
     (cond
      (empty? events)
      ()
      (= (+ duration (third (first events))) 1000)
      ()
      (> (+ duration (third (first events))) 1000)
      (list (concat (list (+ begin-time (- 1000 duration)))
                    (list (second (first events)))
                    (list (- (third (first events)) (- 1000 duration)))
                    (drop  3 (first events))))
      :else
      (remainders (rest events)
                  (+ begin-time (third (first events)))
                  (+ (third (first events)) duration)))))

(defn remove-full-beat
  "Removes one full beat from the events arg."
  ([events]  (remove-full-beat events (ffirst events) 0))
  ([events begin-time duration]
     (cond
      (empty? events)
      ()
      (>= (+ duration (third (first events))) 1000)
      (rest events)
      :else
      (remove-full-beat (rest events)
                        (+ begin-time (third (first events)))
                        (+ (third (first events)) duration)))))

(defn get-other-channels [channel-not-to-get events]
  "Returns all but the first arg channeled events."
  (cond (empty? events)()
        (= (fourth (first events)) channel-not-to-get)
        (get-other-channels channel-not-to-get (rest events))
        :else (cons (first events)
                (get-other-channels channel-not-to-get (rest events)))))

(defn chop-into-bites [events]
  "Chops beats into groupings."
  (cond
   (empty? events)
   ()
   (and (= (third (first events)) 1000)
        (a-thousand? (ffirst events)))
   (cons (list (first events))
         (chop-into-bites (rest events)))
   (> (third (first events)) 1000)
   (cons (chop (first events))
         (chop-into-bites (concat (remainder (first events)) (rest events))))
   :else
   (cons (get-full-beat (get-channel (fourth (first events)) events))
         (chop-into-bites (concat (remainders (get-channel (fourth (first events)) events))
                                  (concat (remove-full-beat (get-channel (fourth (first events)) events))
                                          (get-other-channels (fourth (first events)) events)))))))

(defn break-into-beats [events]
  "Breaks events into beat-sized groupings."
  (sort-by-first-element (apply concat (chop-into-bites (sort-by-first-element events)))))

(defn get-long-phrases [distances]
  "Returns phrases of greater than 120000 duration."
  (cond (empty? (rest distances))()
        (> (- (second distances)(first distances)) 12000)
        (cons (take 2 distances)
              (get-long-phrases (rest distances)))
        :else (get-long-phrases (rest distances))))

(defn get-region [begin-time end-time events]
  "Returns the region boardered by begin and end times."
  (cond (empty? events)()
        (and (>= (ffirst events) begin-time)
             (< (ffirst events) end-time))
        (cons (first events)
              (get-region begin-time end-time (rest events)))
        :else (get-region begin-time end-time (rest events))))

(defn not-beyond [channel-events]
  "Returns events beyond the initial ontime."
  (if (not (> (apply + (map third channel-events)) 1000)) true))

(defn not-beyond-1000
  "Returns t if the beat does not contain events beyond the incept time."
  ([beat] (not-beyond-1000 beat 1))
  ([beat channel]
      (cond (= channel 5) true
            (not-beyond (get-channel channel beat))
            (not-beyond-1000 beat (+ channel 1))
            :else ())))

(defn find-cadence-place [ordered-events]
  "Returns the best place for a first cadence."
  (let [beats (collect-beats ordered-events)]
    (map (fn [beat] (ffirst beat))
         (filter (fn [beat]
                   (and (on-beat (take 4 beat)(ffirst beat))
                        (triad? (take 4 beat))
                        (not-beyond-1000 beat))) beats))))

(defn positions [number list]
  "Shows the positions of number in list."
  (loop [positions []
         number number
         list list]
    (if (empty? (member number list))
      positions
      (let [position (position number list)]
        (recur (conj positions position)
               (+ 1 number)
               (replace 'x number list))))))

(defn find-closest [number list]
  "finds the closest number in list to number."
  (let [test (map (fn [item] (math/abs (- number item))) list)]
    (nth (choose-one (positions (first (sort < test)) test)) list)))

(defn find-best-on-time [on-times]
  "Finds the best ontime."
  (find-closest (+ (/ (- (my-last on-times)(first on-times)) 2)(first on-times)) on-times))

(defn remove-region [begin-time end-time events]
  "Removes the region boardered by begin and end times."
  (cond (empty? events)()
        (and (>= (ffirst events) begin-time)
             (< (ffirst events) end-time))
        (remove-region begin-time end-time (rest events))
        :else (cons (first events)
                (remove-region begin-time end-time (rest events)))))

(defn remove-all [stuff other-stuff]
  "Removes all of the stuff from the other-stuff."
  (loop [stuff stuff
         other-stuff other-stuff]
    (if (empty? stuff)
      other-stuff
      (recur (rest stuff)
             (remove (first stuff) other-stuff)))))

(defn resolve-it
  "Resolves the beat if necessary."
  ([beat] (resolve-it beat (ffirst beat)))
  ([beat on-time]
      (cond (empty? beat)()
            (= (third (first beat)) 1000)
            (cons (first beat)
                  (resolve-it (rest beat) on-time))
            :else (let [test (get-on-beat (get-channel (fourth (first beat)) beat) on-time)]
                    (cons (if (>= (third (first test)) 1000)(first test)
                              (concat (take 2 (first test)) '(1000) (drop 3 (first test))))
                          (resolve-it (remove-all (get-channel (fourth (first beat)) beat) beat) on-time))))))

(defn discover-cadence [missing-cadence-locations ordered-events]
  "Discovers an appropriate cadence."
  (let [relevant-events (get-region (first missing-cadence-locations)(second missing-cadence-locations) ordered-events)
        places-for-cadence (find-cadence-place relevant-events)
        best-location-for-new-cadence (if places-for-cadence (find-best-on-time places-for-cadence) nil)]
    (if (empty? best-location-for-new-cadence) ordered-events
    (sort-by-first-element
     (concat (resolve-it (get-region best-location-for-new-cadence (+ best-location-for-new-cadence 1000) relevant-events))
             (remove-region best-location-for-new-cadence (+ best-location-for-new-cadence 1000) ordered-events))))))

(defn find-1000s
  "Returns the ontime if the ordered events are all duration 1000."
  ([ordered-events] (find-1000s ordered-events (ffirst ordered-events)))
  ([ordered-events start-time]
     (cond (empty? ordered-events)()
           (and (let [channel-1-event (first (get-channel 1 ordered-events))]
                  (and (= (third channel-1-event) 1000)
                       (= start-time (first channel-1-event))))
                (let [channel-1-event (first (get-channel 2 ordered-events))]
                  (and (= (third channel-1-event) 1000)
                       (= start-time (first channel-1-event))))
                (let [channel-1-event (first (get-channel 3 ordered-events))]
                  (and (= (third channel-1-event) 1000)
                       (= start-time (first channel-1-event))))
                (let [channel-1-event (first (get-channel 4 ordered-events))]
                  (and (= (third channel-1-event) 1000)
                       (= start-time (first channel-1-event)))))
           start-time
           :else (find-1000s (rest ordered-events)))))

(defn find-2000s
  "Returns events of 2000 duration."
  ([ordered-events] (find-2000s ordered-events (ffirst ordered-events)))
  ([ordered-events start-time]
      (cond (empty? ordered-events)()
            (and (let [channel-1-event (first (get-channel 1 ordered-events))]
                   (and (= (third channel-1-event) 2000)
                        (= start-time (first channel-1-event))))
                 (let [channel-1-event (first (get-channel 2 ordered-events))]
                   (and (= (third channel-1-event) 2000)
                        (= start-time (first channel-1-event))))
                 (let [channel-1-event (first (get-channel 3 ordered-events))]
                   (and (= (third channel-1-event) 2000)
                        (= start-time (first channel-1-event))))
                 (let [channel-1-event (first (get-channel 4 ordered-events))]
                   (and (= (third channel-1-event) 2000)
                        (= start-time (first channel-1-event)))))
            start-time
            :else (find-2000s (rest ordered-events)))))


(defn discover-cadences [missing-cadence-locations ordered-events]
  "Makes an appropriate cadence possible."
  (if (empty? missing-cadence-locations) ordered-events
      (discover-cadences (rest missing-cadence-locations)
                     (discover-cadence (first missing-cadence-locations) ordered-events))))

(defn distance-to-cadence [ordered-events]
  "Returns the distance tocadence of the arg."
  (let [quarter-note-distance (find-1000s ordered-events)
        half-note-distance (find-2000s ordered-events)]
    (cond (and (empty? quarter-note-distance)(empty? half-note-distance)) ()
          (empty? quarter-note-distance) half-note-distance
          (empty? half-note-distance) quarter-note-distance
          :else (if (> quarter-note-distance half-note-distance) half-note-distance
                quarter-note-distance))))

(defn clear-to [distance-to-cadence ordered-events]
  "Clears the events up to the cadence."
  (cond (empty?  ordered-events)()
        (<= (ffirst  ordered-events) distance-to-cadence)
        (clear-to distance-to-cadence (rest ordered-events))
        :else (cons (first ordered-events)
                (clear-to distance-to-cadence (rest ordered-events)))))

(defn find-cadence-start-times [ordered-events]
  "Finds the cadence start times."
  (let [distance-to-cadence (distance-to-cadence ordered-events)]
    (cond (empty? ordered-events)()
          (empty? distance-to-cadence)
          (find-cadence-start-times (rest ordered-events))
          :else (cons distance-to-cadence
                  (find-cadence-start-times (clear-to distance-to-cadence ordered-events))))))

(defn transpose [amt events]
  "Transposes the events according to its first arg."
  (filter (fn [event]
            (not (= 0 (second event)))
            (concat (list (first event))(list (+ (second event) amt))(drop  2 event)))))

(defn get-note-timing [event time]
  "grunt work for get-beat-length"
  (- (+ (first event)(third event)) time))

(defn get-beat-length [events]
  "this is used in re-time for setting the new time!
   requires that the first in events be sorted to current time!"
  (let [time (ffirst events)]
    (first (sort > (map (fn [event] (get-note-timing event time)) events)))))

(defn match-them [chord full-chord allowance]
  "Matches the chord with the list of pitches within the allowance."
  (cond (empty? chord) true
        (and (not (member (first chord) full-chord))
             (= 0 allowance))
        ()
        (not (member (first chord) full-chord))
        (match-them (rest chord) full-chord (- allowance 1))
        :else (match-them (rest chord) full-chord allowance)))

(defn reduce-it [note base]
  "Reduces its first arg mod12 below its second arg."
  (if (< note base)
    note
    (reduce-it (- note 12) base)))

(defn project-octaves [note]
  "Projects its arg through a series of octaves."
  (let [base-note (reduce-it note 20)]
    (loop [results []
           current-note (reduce-it note 20)]
      (if  (> current-note 120)
        results
        (recur (conj results (+ 12 current-note))
               (+ 12 current-note))))))

(defn match-harmony [one two]
  "Checks to see if its args match mod-12."
  (match-them (sort < one)
              (apply concat
                     (map (fn [note]
                            (project-octaves note)) two))
              (math/floor (/ (count one) 4))))

(defn all-members [list target]
  "Checks to see if its first arg members are present in second arg."
  (clojure.set/superset? (set target) (set list)))

(defn get-all-events-with-start-time-of [start-time events]
  "As its name suggests."
  (cond
   (empty? events)
   ()
   (= (ffirst events) start-time)
   (cons (first events)
         (get-all-events-with-start-time-of start-time (rest events)))
   :else (get-all-events-with-start-time-of start-time (rest events))))

(defn get-last-beat-events [events]
  (let [sorted-events (sort-by-first-element events)
        last-event (last sorted-events)
        begin-time (first last-event)
        last-beat (get-all-events-with-start-time-of begin-time events)]
    (if (and (= (count last-beat) 4)
             (a-thousand? (third (first last-beat))))
      last-beat)))

(defn get-db-n [exploded-lexicon]
  "Checks for dashes in the db-name."
  (cond
   (= (first exploded-lexicon) '-)
   ()
   (empty? exploded-lexicon)
   ()
   :else
   (cons (first exploded-lexicon)
         (get-db-n (rest exploded-lexicon)))))

(defn get-db-name [lexicon]
  "Returns the database name."
  (first (str/split lexicon #"-")))

(defn incf-beat
  "Increments the beat number."
  [beat]
  (when-not (nil? beat)
    (let [beat (str beat)
          last-beat-digit (Integer/parseInt (str (last (explode beat))))]
      (str (get-db-name beat) "-" (+ 1 last-beat-digit)))))

(defn match-bach-tonic [the-events]
  "Returns true if the events are tonic."
  (let [events (get-last-beat-events (break-into-beats the-events))]
    (and (not (empty? events))
         (and (all-members (map second events) (apply concat (map (fn [note] (project-octaves note)) '(60 64 67))))
              (match-harmony (sort < (map second events)) '(60 64 67))
                                                            (match-harmony (sort > (map second events)) '(60 64 67))))))

(defn find-events-duration
  "Returns the events duration."
  ([events] (find-events-duration events 0))
  ([events duration]
     (cond (empty? events)
           duration
           (= (fourth (first events)) 1)
           (find-events-duration (rest events) (+ duration (third (first events))))
           :else (find-events-duration (rest events) duration))))

(defn- retimed-events [events current-time]
  (map (fn [event]
         (cons (+ (first event) current-time)
               (rest event)))
       (set-to-zero (first events))))

(defn re-time
  "Re-times the beats to fit together."
  ([event-lists] (re-time event-lists 0))
  ([event-lists current-time]
     (if (empty? event-lists)
       ()
       (cons (retimed-events event-lists current-time)
             (re-time (rest event-lists)
                      (+ current-time (get-beat-length (first event-lists))))))))

(defn highest-lowest-notes [events]
  "Returns the highest and lowest pitches of its arg."
  (list (first (sort > (map (fn [event] (second event)) (get-channel 1 events))))
        (first (sort < (map (fn [event] (second event)) (let [test (get-channel 4 events)]
                                                          (if (empty? test)(get-channel 2 events) test)))))))

(defn put-it-in-the-middle [extremes]
  "Gets the average."
  (math/round (/ (+ (second extremes)(first extremes)) 2)))

(defn transpose-to-bach-range [events]
  (let [high-low (highest-lowest-notes events)
        intervals-off (list (- 83 (first high-low))
                            (- 40 (second high-low)))
        middle (put-it-in-the-middle intervals-off)]
    (transpose middle events)))

(defn make-1000s [beat]
  "Makes all of the beat's durations into 1000s."
  (map (fn [event] (concat (take 2 event) '(1000) (drop 3 event))) beat))

(defn reset [beats subtraction]
  "Resets the beats appropriately."
  (if (empty? beats)()
      (cons (map (fn [event] (concat (list (- (first event) subtraction)) (rest event)) ) (first beats))
            (reset (rest beats) subtraction))))

(defn collapse [beats]
  "Collapses the final cadence."
  (cond (empty? beats)
        ()
        (and (= (count (first beats)) 4)
             (= (third (first (first beats))) 2000))
        (cons (make-1000s (first beats))
              (collapse (reset (rest beats) 1000)))
        :else (cons (first beats)
                    (collapse (rest beats)))))

(defn cadence-collapse [events]
  "Ensures the final chord will not have offbeats."
  (apply concat (collapse (collect-beats (sort-by-first-element  events)))))

(defn reset-events-to [events begin-time]
  "Resets the events for the delayed beat."
  (map (fn [event] (cons (+ begin-time (first event))(rest event))) (set-to-zero events)))

(defn delay-for-upbeat [events]
  "Delays the upbeat."
  (reset-events-to events 3000))

(defn all [first second]
  "Tests for presence of all of first arg in second arg."
  (cond (empty? first) true
        (member (first first) second)
        (all (rest first) second)
        :else ()))

(defn get-tonic  [events]
  "Returns the tonic."
  (and (or (all (create-pitch-class-set (get-pitches events))
                '(0 4 7))
           (all (create-pitch-class-set (get-pitches events))
                '(0 3 7)))
       (= 0 (first (create-pitch-class-set (get-pitches (get-channel 4 (sort-by-first-element  events))))))))

(defn check-mt [events]
  "Returns the major tonic."
  (get-tonic events))

(defn ensure-necessary-cadences [ordered-events]
  "Ensures the cadences are proper."
  (let [cadence-start-times (find-cadence-start-times ordered-events)]
    (discover-cadences (get-long-phrases (if (not (= 0 (first cadence-start-times))) (cons 0 cadence-start-times) cadence-start-times))
                       ordered-events)))

(defn- sorted-by-beat [beats]
  (map (fn [beat]
         (get-pitches (get-on-beat beat (ffirst beat)))) beats))

(defn check-for-parallel [events]
  (let [beats (collect-beats (take 30 (sort-by-first-element events)))
        sorted-pitches-by-beat (sorted-by-beat beats)]
    (and (= (count (first sorted-pitches-by-beat)) 4)
         (= (count (second sorted-pitches-by-beat)) 4)
         (or (and (> (- (ffirst sorted-pitches-by-beat)
                        (first (second sorted-pitches-by-beat))) 0)
                  (> (- (second (first sorted-pitches-by-beat))
                        (second (second sorted-pitches-by-beat))) 0)
                  (> (- (third (first sorted-pitches-by-beat))
                        (third (second sorted-pitches-by-beat))) 0)
                  (> (- (fourth (first sorted-pitches-by-beat))
                        (fourth (second sorted-pitches-by-beat)))) 0)
             (and (< (- (ffirst sorted-pitches-by-beat)
                        (first (second sorted-pitches-by-beat))) 0)
                  (< (- (second (first sorted-pitches-by-beat))
                        (second (second sorted-pitches-by-beat))) 0)
                  (< (- (third (first sorted-pitches-by-beat))
                        (third (second sorted-pitches-by-beat))) 0)
                  (< (- (fourth (first sorted-pitches-by-beat))
                        (fourth (second sorted-pitches-by-beat))) 0))))))

(defn wait-for-cadence
  "Ensures the cadence is the proper length."
  ([events] (wait-for-cadence events (ffirst events)))
  ([events start-time]
     (cond
      (empty? events)
      false
      (> (ffirst events) (+ start-time 4000))
      true
      (> (third (first events)) 1000)
      false
      :else
      (wait-for-cadence (rest events) start-time))))

(defn match-tonic-minor [the-events]
  (let [events (get-last-beat-events (break-into-beats the-events))]
    (when-not (empty? events)
      (let [projected-octaves (mapcat (fn [note] (project-octaves note)) '(60 63 67))]
        (and
         (all-members (map second events) projected-octaves)
         (match-harmony (sort < (map second events)) '(60 63 67))
         (match-harmony (sort > (map second events)) '(60 63 67)))))))

(defn- skip-generating-new-events? [counter current-beat]
  (reset! *early-exit?* (empty? (:destination-notes (find-beat current-beat))))
  (or
   @*early-exit?*
   (if (and (> counter 36)
            (if (= *tonic* 'minor)
              (and (> (find-events-duration (:events (find-beat current-beat))) *beat-size*)
                   (match-tonic-minor (:events (find-beat current-beat))))
              (and (> (find-events-duration (:events (find-beat current-beat))) *beat-size*)
                   (match-bach-tonic (:events (find-beat current-beat))))))
     (do
       (reset! *end* true)
       true))))

(defn build-events-for-beat [counter current-beat]
  (loop [events []
         counter counter
         current-beat current-beat]
    (if (skip-generating-new-events? counter current-beat)
      events
      (let [beat (find-beat current-beat)
            new-events (:events beat)
            destination-notes (:destination-notes beat)
            lexicon-name (make-lexicon-name destination-notes)
            beat-in-lexicon (find-in-lexicon lexicon-name)
            beat-choices (:beats beat-in-lexicon)
            new-beat (choose-one
                      (if (empty? (rest beat-choices))
                        beat-choices
                        (my-remove (list @*previous-beat* (incf-beat @*previous-beat*)) beat-choices)))]


        (swap! *history* conj current-beat)
        (reset! *previous-beat* current-beat)

        (recur (concat events new-events)
               (+ 1 counter)
               new-beat)))))

(defn- build-events [counter]
  (let [current-beat (find-triad-beginning)
        current-beat-events (:events (find-beat current-beat))]
    (if (match-tonic-minor (take 4 current-beat-events))
      (reset! *tonic* 'minor)
      (reset! *tonic* 'major))

    (let [events (build-events-for-beat counter current-beat)
          events-list (list current-beat-events)
          all-events (if-not (empty? events) (concat (list events) events-list) events-list)]
      (swap! *history* conj current-beat)
      (apply concat (re-time all-events)))))

(defn compose-b
  ([] (compose-b 0))
  ([counter]
     (reset! *end* false)
     (reset! *history* ())
     (reset! *events* (build-events counter))

     (if (and
          (not @*early-exit?*)
          (= *composer* 'bach))
       @*events*
       (reset! *events* ()))
     (reset! *history* (reverse @*history*))
     (if @*end*
       (swap! *history* conj (list (+ 1 *compose-number*))))))

(defn finished-composing? [events end?]
  (if (empty? events)
    false
    (let [last-event (last (sort-by-first-element events))
          event-sum (+ (first last-event) (third last-event))]
      (not
       (or
        (< event-sum 15000)
        (> event-sum 200000)
        (not (wait-for-cadence events))
        (check-for-parallel events)
        (not end?))))))

(defn finish []
  (reset! *save-events* @*events*)
  (reset! *events* (ensure-necessary-cadences (sort-by-first-element @*events*)))
  (if (not (check-mt (get-on-beat @*events* (ffirst @*events*))))
    (reset! *events* (delay-for-upbeat @*events*)))
  (if (and
       (false? *early-exit?*)
       (= *composer* 'bach))
    (reset! *events* (cadence-collapse (transpose-to-bach-range @*events*)))
    (reset! *events* ()))
  @*save-events*)

(defn compose-bach []
  (compose-b)
  (if (finished-composing? @*events* @*end*)
    (finish)
    (compose-bach)))

(defn compose []
  (create-complete-database chorale/bach-chorales-in-databases)
  (compose-bach))