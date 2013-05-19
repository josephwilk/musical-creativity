(ns musical-creativity.composers.recombinance
  (:require
   [clojure.math.numeric-tower :as math]
   [data.chorale               :as chorale]
   [musical-creativity.util    :refer :all]
   [clojure.string             :as str]))

(def *beats-store* (atom {}))
(def *lexicon-store* (atom {}))

(def *lexicons* (atom []))
(def *history* (atom ()))

(def *mix-names* ())
(def *mix* ())

(def *end?* (atom false))
(def *early-exit?* (atom false))
(def *compose-number* 0)

(def *tonic* (atom 'major))
(def *previous-beat* (atom nil))
(def *beat-size* 1000)
(def *beats* 4)

(def *composer* 'bach)
(def bach-dominants-tonics ())
(def bach-dominants ())
(def bach-tonics ())

(def bach-start-beats (atom ()))
(def bach-compose-beats (atom ()))
(def bach-rules (atom ()))
(def bach [bach-compose-beats bach-start-beats bach-rules])
(def bach-form [])

(defn timepoint-of [event]
  (first event))

(defn pitch-of [event]
  (second event))

(defn velocity-of [event]
  (third event))

(defn channel-of [event]
  (fourth event))

(defn instrument-of [event]
  (nth event 4))

(defn remove-from-list [objects-to-be-remove list-of-objects]
  (remove #(some #{%} objects-to-be-remove) list-of-objects))

(defn implode [list]
  (str/join "" list))

(defn explode [atom]
  (vec atom))

(defn get-onset-notes
  "Gets the onset pitches for its arg."
  [events]
  (let [onbeat (ffirst events)
        onbeat-events (filter (fn [event]
                                (= (timepoint-of event) onbeat)) events)]
    (map #(second %) onbeat-events)))

(defn sort-by-first-element [lists]
  (sort (fn [[x & _] [y & _]] (< x y))  lists))

(defn set-to-zero
  "Sets the events to zero."
  ([events] (set-to-zero events (ffirst events)))
  ([events subtract]
     (map (fn [event]
            (assoc (vec event) 0 (- (timepoint-of event) subtract))) events)))

(defn a-thousand? [number]
  "Returns the number under 1000."
  (= 0 (mod number 1000)))

(defn member [value list]
  (if (seq list)
    (if (= value (first list))
      list
      (recur value (rest list)))))

(defn remove-nils [list]
  (remove #(nil? %) list))

(defn- var-get-from-str [name]
  (var-get (ns-resolve 'musical-creativity.composers.recombinance (symbol name))))

(defn make-name
  "Simple synonym for imploding the database name and number."
  [db-name counter]
  (symbol (str (name db-name)  "-" counter)))

(defn hyphenate [note-numbers]
  (str/join "-" note-numbers))

(defn- find-composer-beats-atom []
  (var-get-from-str (str *composer* "-compose-beats")))

(defn- find-composer-start-beats-atom []
  (var-get-from-str (str *composer* "-start-beats")))

(defn- find-composer-rules-atom []
  (var-get-from-str (str *composer* "-rules")))

(defn- find-composer []
  (var-get-from-str (str *composer*)))

(defn reduce-interval [interval]
  "Reduces the interval mod 12."
  (cond
   (<= (math/abs interval) 12)
   interval
   (< interval 0)
   (reduce-interval (+ interval 12))
   :else
   (reduce-interval (- interval 12))))

(defn get-rule
  "Gets the rule between first two args."
  [voice start-note start-notes destination-notes name]
  (if (or (empty? (rest start-notes)) (empty? destination-notes))
    ()
    (cons (list (reduce-interval (- (second start-notes) start-note))
                voice
                (- (second destination-notes) (second start-notes))
                name)
          (get-rule voice start-note (rest start-notes) (rest destination-notes) name))))

(defn build-rules-for [start-notes destination-notes name]
  (loop [rules []
         start-notes start-notes
         destination-notes destination-notes]
    (if (or (empty? (rest start-notes)) (empty? (rest destination-notes)))
      (reverse rules)
      (let [new-rule (reverse (get-rule (- (first destination-notes) (first start-notes))
                                        (first start-notes) start-notes destination-notes name))]
        (recur (concat new-rule rules) (rest start-notes) (rest destination-notes))))))

(defn make-lists-equal-length
  [list1 list2]
  (let [equal-pairs (map vector list1 list2)]
    (list (map first equal-pairs) (map second equal-pairs))))

(defn get-rules
  "Gets the intervals between adjacent sets of the two args."
  [start-notes destination-notes name]
  (let [test (make-lists-equal-length start-notes destination-notes)
        test-start-notes (first test)
        test-destination-notes (second test)]
    (build-rules-for test-start-notes test-destination-notes name)))

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
      (bound? (implode (cons (first names) (cons '- (hyphenate note-numbers)))))
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
        (reset! *lexicon-store* (assoc @*lexicon-store* lexicon-name {:beats (list beat-name)}))
        (swap-unless-includes *lexicons* lexicon-name)
        lexicon-name))))

(defn return-beat
  "Returns the beat number of the initiating event."
  ([channel-events] (return-beat channel-events (ffirst channel-events)))
  ([channel-events start-time]
     (some (fn [event]
             (when (and (a-thousand? (timepoint-of event))
                        (not (= start-time (timepoint-of event))))
               (/ (- (timepoint-of event) start-time) 1000)))
           channel-events)))

(defn create-pitch-class-set
  "Sorts and gets a full pc-set."
  [pitches]
  (let [pitch-classes (map #(mod % 12) pitches)]
    (sort < (distinct pitch-classes))))

(defn get-channel-numbers-from-events
  "simply gets the channel numbers from the music"
  ([events] (get-channel-numbers-from-events events []))
  ([events channels]
     (map #(fourth %)
          (filter (fn [event]
                    (not (member (fourth (first events)) channels)))
                  (reverse events)))))

(defn find-alignment [point channel]
  (and (a-thousand? point)
       (some #(= point (second %)) channel)))

(defn find-alignment-in-all-channels
  [point channels]
  (when (and point
             (every? #(find-alignment point %) channels))
    point))

(defn all-together
  "Returns the appropriate channel timing."
  [channels all-channels]
  (let [together-timepoint (some (fn [[_ timepoint]]
                                   (find-alignment-in-all-channels timepoint all-channels))
                                 channels)]
    (or together-timepoint
        (second (last-first all-channels)))))

(defn collect-timings-by-channel
  "collects the timings of the channel indicated in second arg"
  [timings channel]
  (filter #(= (first %) channel) timings))

(defn plot-timings-of-each-beat
  [events]
  (map (fn [event]
         (list (channel-of event) (+ (timepoint-of event) (velocity-of event))))
       events))

(defn first-place-where-all-together [events]
  "This looks ahead to get the first time they end together"
  (let [test (plot-timings-of-each-beat events)
        channels (get-channel-numbers-from-events events)
        ordered-timings-by-channel (map (fn [channel] (collect-timings-by-channel test channel)) channels)]

    (all-together (first ordered-timings-by-channel)
                  (rest ordered-timings-by-channel))))

(defn collect-by-timing
  "Collects the events accoring to timing."
  [timing events]
  (filter (fn [[timepoint _ _ channel _]]
            (<= (+ timepoint channel) timing))
          events))

(defn collect-beats [events]
  (if (empty? events)
    ()
    (let [sync-time (first-place-where-all-together events)
          events-by-time  (collect-by-timing sync-time events)
          reduced-events (drop (count events-by-time) events)]
      (cons events-by-time
            (collect-beats reduced-events)))))

(defn make-beat [name beats]
  (let [start-notes (get-onset-notes (first beats))
        destination-notes (get-onset-notes (second beats))
        events (first beats)
        rules (cons (get-rules start-notes destination-notes name)
                    (list name (ffirst (sort-by-first-element events))))]

    (swap! (find-composer-rules-atom) conj rules)

    {:start-notes start-notes
     :destination-notes destination-notes
     :events events
     :voice-leading (first rules)}))

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
               (swap! (find-composer-beats-atom) conj name)

               (when start
                 (swap! (find-composer-start-beats-atom) conj name))

               (recur (rest beats) (+ 1 counter) nil))))
         (create-complete-database (rest db-names))))))

(defn on-beat?
  "Returns true if the events conform to ontime."
  [events ontime]
  (every? (fn [event]
            (and (a-thousand? (timepoint-of event))
                 (= (timepoint-of event) ontime)))
          events))

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

(defn get-pitches
  "Gets the pitches from its arg."
  [events]
  (map #(pitch-of %) events))

(defn get-interval
  "Returns the intervals between set members."
  [set]
  (if (empty? (rest set))
    ()
    (cons (- (second set) (first set))
          (get-interval (rest set)))))

(defn get-intervals
  "Returns the intervals in the sets."
  [sets]
  (map (fn [set]
         (math/abs (apply + (get-interval set))))
       sets))

(defn project
  "Projects the pc-set through its inversions."
  ([set] (project set (count set) 0))
  ([set length times]
     (if (= length times)
       ()
       (cons set
             (project (concat (rest set) (list (+ 12 (first set))))
                      length (+ 1 times))))))

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
           (and (> (- (second pitch-classes) (first pitch-classes)) 2)
                (< (- (second pitch-classes) (first pitch-classes)) 5))
           (and (> (- (third pitch-classes) (second pitch-classes)) 2)
                (< (- (third pitch-classes) (second pitch-classes)) 5))))))

(defn members-all [arrows target]
  "Checks to see if arrows are all in target."
  (every? #(some #{%} target) arrows))

(defn find-triad-beginning
  "Returns the db with a triad beginning."
  []
  (let [test (choose-one @(first (find-composer)))
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

(defn remainder
  "Returns the remainder of the beat."
  ([event] (remainder event (timepoint-of event) (velocity-of event)))
  ([event begin-time duration]
     (cond
      (empty? event)
      ()
      (= duration 1000)
      ()
      (< duration 1000) (list (concat (list begin-time)
                                      (list (pitch-of event))
                                      (list duration)
                                      (drop  3 event)))
      :else
      (remainder event (+ begin-time 1000) (- duration 1000)))))

(defn get-full-beat
  "Returns one full beat of the music."
  ([events] (get-full-beat events (ffirst events) 0))
  ([events begin-time duration]
     (cond (empty? events)
           ()
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

(defn chop
  "Chops beats over 1000 into beat-sized pieces."
  ([event] (chop event (timepoint-of event) (velocity-of event)))
  ([event begin-time duration]
      (if (< duration 1000)
        ()
        (cons (concat (list begin-time)
                      (list (pitch-of event))
                      '(1000)
                      (drop 3 event))
              (chop event (+ begin-time 1000) (- duration 1000))))))

(defn get-channel [channel music]
  "Gets the nth channel of the music."
  (filter (fn [note] (= (channel-of note) channel)) music))

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
   (let [event (first events)
         channel (channel-of event)
         events-for-channel (get-channel channel events)]
     (cons (get-full-beat events-for-channel)
           (chop-into-bites (concat (remainders events-for-channel)
                                    (concat (remove-full-beat events-for-channel)
                                            (get-other-channels channel events))))))))

(defn break-into-beats [events]
  "Breaks events into beat-sized groupings."
  (sort-by-first-element
   (apply concat (chop-into-bites (sort-by-first-element events)))))

(defn get-long-phrases
  "Returns phrases of greater than 120000 duration."
  [distances]
  (remove nil?
          (map (fn [start stop]
                 (when (> (- stop start) 12000)
                   [start stop]))
               distances
               (rest distances))))

(defn get-region
  "Returns the region boardered by begin and end times."
  [begin-time end-time events]
  (filter (fn [event]
            (and (>= (timepoint-of event) begin-time)
                 (<  (timepoint-of event) end-time)))
          events))

(defn not-beyond? [channel-events]
  (if (not (> (apply + (map third channel-events)) 1000)) true))

(defn not-beyond-1000?
  "Returns true if the beat does not contain events beyond the incept time."
  [beat]
  (every? (fn [channel]
            (not-beyond? (get-channel channel beat)))
          (range 1 4)))

(defn- cadence-place? [beat]
  (and (on-beat? (take 4 beat) (ffirst beat))
       (triad? (take 4 beat))
       (not-beyond-1000? beat)))

(defn find-cadence-place
  "Returns the best place for a first cadence."
  [ordered-events]
  (let [beats (collect-beats ordered-events)]
    (seq
     (map (fn [beat] (ffirst beat))
          (filter cadence-place? beats)))))

(defn positions [number list]
  "Shows the positions of number in list."
  (let [indexed-list (map-indexed vector list)]
    (for [[index element] indexed-list :when (= number element)] index)))

(defn find-closest
  "finds the closest number in list to number."
  [number list]
  (let [test (map (fn [item] (math/abs (- number item))) list)]
    (nth list (choose-one (positions (first (sort < test)) test)))))

(defn find-best-on-time [on-times]
  (find-closest
   (+
    (/ (- (last on-times) (first on-times)) 2)
    (first on-times))
   on-times))

(defn remove-region
  "Removes the region boardered by begin and end times."
  [begin-time end-time events]
  (concat (filter (fn [event]
                    (or (< (timepoint-of event) begin-time)
                        (>= (timepoint-of event) end-time)))
                  events)))

(defn remove-all [stuff other-stuff]
  (vec (clojure.set/difference (set other-stuff) (set stuff))))

(defn- build-suitable-event [event]
  (if (>= (velocity-of event) 1000)
    event
    (concat (take 2 event) '(1000) (drop 3 event))))

(defn resolve-beat
  "Resolves the beat if necessary."
  ([beat] (resolve-beat beat (ffirst beat)))
  ([beat on-time]
     (cond
      (nil? (seq beat))
      ()
      (= (third (first beat)) 1000)
      (cons (first beat)
            (resolve-beat (rest beat) on-time))
      :else
      (let [on-beat-candidate (get-on-beat (get-channel (channel-of (first beat)) beat) on-time)
            on-beat-event (first on-beat-candidate)]
        (cons (build-suitable-event on-beat-event)
              (resolve-beat (remove-all (get-channel (channel-of (first beat)) beat) beat) on-time))))))

(defn discover-cadence
  "Discovers an appropriate cadence."
  [missing-cadence-locations ordered-events]
  (let [relevant-events (get-region (first missing-cadence-locations) (second missing-cadence-locations) ordered-events)
        places-for-cadence (find-cadence-place relevant-events)
        best-location-for-new-cadence (when places-for-cadence (find-best-on-time places-for-cadence))]
    (if-not best-location-for-new-cadence
      ordered-events
      (sort-by-first-element
       (concat (resolve-beat (get-region best-location-for-new-cadence (+ best-location-for-new-cadence 1000) relevant-events))
               (remove-region best-location-for-new-cadence (+ best-location-for-new-cadence 1000) ordered-events))))))

(defn all-match-velocity? [velocity ordered-events start-time]
  (and (let [channel-1-event (first (get-channel 1 ordered-events))]
         (and (= (velocity-of channel-1-event) velocity)
              (= start-time (timepoint-of channel-1-event))))
       (let [channel-2-event (first (get-channel 2 ordered-events))]
         (and (= (velocity-of channel-2-event) velocity)
              (= start-time (timepoint-of channel-2-event))))
       (let [channel-3-event (first (get-channel 3 ordered-events))]
         (and (= (velocity-of channel-3-event) velocity)
              (= start-time (timepoint-of channel-3-event))))
       (let [channel-4-event (first (get-channel 4 ordered-events))]
         (and (= (velocity-of channel-4-event) velocity)
              (= start-time (timepoint-of channel-4-event))))))

(defn find-1000s
  "Returns the ontime if the ordered events are all duration 1000."
  ([ordered-events] (find-1000s ordered-events (ffirst ordered-events)))
  ([ordered-events start-time]
     (cond
      (empty? ordered-events)
      nil
      (all-match-velocity? 1000 ordered-events start-time)
      start-time
      :else
      (find-1000s (rest ordered-events)))))

(defn find-2000s
  "Returns events of 2000 duration."
  ([ordered-events] (find-2000s ordered-events (ffirst ordered-events)))
  ([ordered-events start-time]
      (cond
       (empty? ordered-events)
       nil
       (all-match-velocity? 2000 ordered-events start-time)
       start-time
       :else
       (find-2000s (rest ordered-events)))))

(defn discover-cadences
  "Makes an appropriate cadence possible."
  [missing-cadence-locations ordered-events]
  (reduce (fn [events location]
            (discover-cadence location events))
          ordered-events
          missing-cadence-locations))

(defn distance-to-cadence [ordered-events]
  "Returns the distance tocadence of the arg."
  (let [quarter-note-distance (find-1000s ordered-events)
        half-note-distance (find-2000s ordered-events)]
    (cond
     (and (nil? quarter-note-distance) (nil? half-note-distance))
     nil
     (nil? quarter-note-distance)
     half-note-distance
     (nil? half-note-distance)
     quarter-note-distance
     :else
     (if (> quarter-note-distance half-note-distance)
       half-note-distance
       quarter-note-distance))))

(defn clear-to
  "Clears the events up to the cadence."
  [distance-to-cadence ordered-events]
  (filter (fn [event]
            (> (timepoint-of event) distance-to-cadence))
   ordered-events))

(defn find-cadence-start-times
  "Finds the cadence start times."
  [ordered-events]
  (let [distance-to-cadence (distance-to-cadence ordered-events)]
    (cond
     (empty? ordered-events)
     ()
     (nil? distance-to-cadence)
     (find-cadence-start-times (rest ordered-events))
     :else
     (cons distance-to-cadence
           (find-cadence-start-times (clear-to distance-to-cadence ordered-events))))))

(defn transpose [amt events]
  "Transposes the events according to its first arg."
  (filter (fn [event]
            (not (= 0 (pitch-of event)))
            (concat (list (timepoint-of event))
                    (list (+ (pitch-of event) amt))
                    (drop  2 event))) events))

(defn get-note-timing [event time]
  "grunt work for get-beat-length"
  (- (+ (timepoint-of event) (velocity-of event)) time))

(defn get-beat-length [events]
  (let [time (ffirst events)]
    (first (sort > (map (fn [event] (get-note-timing event time)) events)))))

(defn match-them [chord full-chord allowance]
  "Matches the chord with the list of pitches within the allowance."
  (cond
   (empty? chord) true
   (and (not (member (first chord) full-chord))
        (= 0 allowance))
   ()
   (not (member (first chord) full-chord))
   (match-them (rest chord) full-chord (- allowance 1))
   :else
   (match-them (rest chord) full-chord allowance)))

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

(defn get-all-events-with-start-time-of
  [start-time events]
  (filter (fn [event]
            (= (timepoint-of event) start-time))
          events))

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

(defn inc-beat-number
  [beat]
  (when-not (nil? beat)
    (let [beat (str beat)
          last-beat-digit (Integer/parseInt (str (last (explode beat))))]
      (str (get-db-name beat) "-" (+ 1 last-beat-digit)))))

(defn match-bach-tonic?
  "Returns true if the events are tonic."
  [the-events]
  (let [events (get-last-beat-events (break-into-beats the-events))]
    (and (not (empty? events))
         (all-members (map second events) (apply concat (map #(project-octaves %) '(60 64 67))))
         (match-harmony (sort < (map second events)) '(60 64 67))
         (match-harmony (sort > (map second events)) '(60 64 67)))))

(defn find-events-duration
  "Returns the events duration."
  ([events] (find-events-duration events 0))
  ([events duration]
     (cond
      (empty? events)
      duration
      (= (channel-of (first events)) 1)
      (find-events-duration (rest events) (+ duration (velocity-of (first events))))
      :else
      (find-events-duration (rest events) duration))))

(defn- retimed-events [events current-time]
  (map (fn [event]
         (cons (+ (timepoint-of event) current-time)
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

(defn highest-lowest-notes
  "Returns the highest and lowest pitches of its arg."
  [events]
  (list (first (sort > (map (fn [event] (pitch-of event)) (get-channel 1 events))))
        (first (sort < (map (fn [event] (pitch-of event)) (let [test (get-channel 4 events)]
                                                          (if (empty? test) (get-channel 2 events) test)))))))

(defn put-it-in-the-middle [extremes]
  "Gets the average."
  (math/round (/ (+ (second extremes) (first extremes)) 2)))

(defn transpose-to-bach-range [events]
  (let [high-low (highest-lowest-notes events)
        intervals-off (list (- 83 (first high-low))
                            (- 40 (second high-low)))
        middle (put-it-in-the-middle intervals-off)]
    (transpose middle events)))

(defn make-1000s
  "Makes all of the beat's durations into 1000s."
  [beat]
  (map (fn [event] (concat (take 2 event) '(1000) (drop 3 event))) beat))

(defn reset-beats
  "Resets the beats appropriately."
  [beats subtraction]
  (if (empty? beats)()
      (cons (map (fn [event] (concat (list (- (timepoint-of event) subtraction)) (rest event)) ) (first beats))
            (reset-beats (rest beats) subtraction))))

(defn collapse
  "Collapses the final cadence."
  [beats]
  (cond
   (empty? beats)
   ()
   (and (= (count (first beats)) 4)
        (= (third (first (first beats))) 2000))
   (cons (make-1000s (first beats))
         (collapse (reset-beats (rest beats) 1000)))
   :else
   (cons (first beats)
         (collapse (rest beats)))))

(defn cadence-collapse
  "Ensures the final chord will not have offbeats."
  [events]
  (apply concat (collapse (collect-beats (sort-by-first-element events)))))

(defn reset-events-to
  "Resets the events for the delayed beat."
  [events begin-time]
  (map (fn [event] (cons (+ begin-time (timepoint-of event))(rest event))) (set-to-zero events)))

(defn delay-for-upbeat [events]
  (reset-events-to events 3000))

(defn all?
  "Tests for presence of all of first arg in second arg."
  [list1 list2]
  (empty? (clojure.set/difference (set list1) (set list2))))

(defn get-tonic
  "Returns the tonic."
  [events]
  (and (or (all? (create-pitch-class-set (get-pitches events)) '(0 4 7))
           (all? (create-pitch-class-set (get-pitches events)) '(0 3 7)))
       (= 0 (first (create-pitch-class-set (get-pitches (get-channel 4 (sort-by-first-element  events))))))))

(defn check-mt
  "Returns the major tonic."
  [events]
  (get-tonic events))

(defn ensure-necessary-cadences
  "Ensures the cadences are proper."
  [ordered-events]
  (let [cadence-start-times (find-cadence-start-times ordered-events)
        cadence-start-times (if-not (= 0 (first cadence-start-times)) (cons 0 cadence-start-times) cadence-start-times)
        long-phrases (get-long-phrases cadence-start-times)]
    (discover-cadences long-phrases ordered-events)))

(defn- sorted-by-beat [beats]
  (map (fn [beat]
         (get-pitches (get-on-beat beat (ffirst beat)))) beats))



(defn parallel? [events]
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

(defn wait-for-cadence?
  "Ensures the cadence is the proper length."
  ([events] (wait-for-cadence? events (ffirst events)))
  ([events start-time]
     (cond
      (empty? events)
      false
      (> (timepoint-of (first events)) (+ start-time 4000))
      true
      (> (velocity-of (first events)) 1000)
      false
      :else
      (wait-for-cadence? (rest events) start-time))))

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
                   (match-bach-tonic? (:events (find-beat current-beat))))))
     (do
       (reset! *end?* true)
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
                        (remove-from-list (list @*previous-beat* (inc-beat-number @*previous-beat*)) beat-choices)))]

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

(defn compose-events
  ([] (compose-events 0))
  ([counter]
     (reset! *end?* false)
     (reset! *history* ())
     (let [events (build-events counter)]
       (reset! *history* (reverse @*history*))
       (when @*end?* (swap! *history* conj (list (+ 1 *compose-number*))))

       (if (or
            @*early-exit?*
            (not (= *composer* 'bach)))
         ()
         events))))

(defn valid-solution? [events end?]
  (if (empty? events)
    false
    (let [last-event (last (sort-by-first-element events))
          event-sum (+ (first last-event) (third last-event))]
      (not
       (or
        (< event-sum 15000)
        (> event-sum 200000)
        (not (wait-for-cadence? events))
        (parallel? events)
        (not end?))))))

(defn prepare-events [events early-exit?]
  (let [events (ensure-necessary-cadences (sort-by-first-element events))
        events (if-not (check-mt (get-on-beat events (ffirst events)))
                 (delay-for-upbeat events)
                 events)
        events (if (and (not early-exit?) (= *composer* 'bach))
                 (cadence-collapse (transpose-to-bach-range events))
                 events)]
    events))

(defn compose-bach []
  (let [events (compose-events)]
    (if (valid-solution? events @*end?*)
      (prepare-events events @*early-exit?*)
      (compose-bach))))

(defn load-bach-chorales []
  (create-complete-database chorale/bach-chorales-in-databases))

(defn- midi-to-event [midi]
  {:time     (timepoint-of midi)
   :pitch    (pitch-of midi)
   :velocity (velocity-of midi)
   :channel  (channel-of midi)})

(defn compose []
  (let [events (compose-bach)]
    (map midi-to-event events)))

(defn compose-original []
  (map midi-to-event
       (mapcat chorale/find-db (list (first chorale/bach-chorales-in-databases)
                                     (second chorale/bach-chorales-in-databases)
                                     (third chorale/bach-chorales-in-databases)))))