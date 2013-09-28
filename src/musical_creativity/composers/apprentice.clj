(ns musical-creativity.composers.apprentice
  (:require
   [clojure.math.numeric-tower :as math]))

(def weight-divisor 2)
(def keyword-weight 0.15)
(def successor-weight 0.5)
(def backward-chain-weight 0.1)
(def last-word-weight 0.2)

(def *initiate* (atom true))
(def *counter* (atom 0))

(def *sentences-store* (atom {}))
(def *words-store*     (atom {}))

(def *no-sentences* (atom ()))
(def *yes-sentences* (atom ()))
(def *yes* (atom ()))
(def *no* (atom ()))
(def *keyword* (atom ()))
(def *keywords* (atom ()))

(def *predecessor* (atom nil))
(def *successor* (atom nil))
(def *last-word* (atom nil))
(def *last-words* (atom ()))
(def *all-words* (atom ()))
(def *input-work* (atom ()))
(def *weight-list* (atom ()))
(def *current-words* (atom ()))

(defn make-incipient-lexicon [] {:incipients ()})
(defn make-candence-lexicon  [] {:cadences ()})

(def *question-incipient-lexicon* (atom (make-incipient-lexicon)))
(def *answer-incipient-lexicon*   (atom (make-incipient-lexicon)))
(def *question-cadence-lexicon*   (atom (make-candence-lexicon )))
(def *answer-cadence-lexicon*     (atom (make-candence-lexicon)))

(defn reset-all! []
  (reset! *initiate* true)
  (reset! *counter* 0)
  (reset! *sentences-store* {})
  (reset! *no-sentences* ())
  (reset! *yes-sentences* ())
  (reset! *yes* ())
  (reset! *no* ())
  (reset! *keyword* ())
  (reset! *keywords* ())
  (reset! *predecessor* nil)
  (reset! *successor* nil)
  (reset! *last-word* nil)
  (reset! *last-words* ())
  (reset! *words-store* {})
  (reset! *all-words* ())
  (reset! *input-work* ())
  (reset! *weight-list* ())
  (reset! *current-words* ())
  (reset! *question-incipient-lexicon* (make-incipient-lexicon))
  (reset! *answer-incipient-lexicon*   (make-incipient-lexicon))
  (reset! *question-cadence-lexicon*   (make-candence-lexicon))
  (reset! *answer-cadence-lexicon*     (make-candence-lexicon)))

(declare find-no find-yes)

(defn member [item col] (boolean (some #{item} col)))
(defn my-sort [fun things] (sort fun things))

(defn position [thing list]
  "returns position of thing in list or nil"
  (let [index (.indexOf list thing)]
    (when (>= index 0) index)))

(defn lookup-sentence [sentence]
  (@*sentences-store* sentence))

(defn make-sentence [id atts]  (reset! *sentences-store* (assoc @*sentences-store* id atts)))
(defn update-sentence [id field val] (reset! *sentences-store* (assoc-in @*sentences-store* [id field] val))
  val)
(defn sentence-seen? [sentence] (some #{sentence} (keys @*sentences-store*)))

(defn lookup-word [word]  (@*words-store* word))
(defn make-word [id atts]  (reset! *words-store* (assoc @*words-store* id atts)))
(defn update-word [id field val]
  (reset! *words-store* (assoc-in @*words-store* [id field] val))
  val)

(defn word-seen? [word] (some #{word} (keys @*words-store*)))
(defn swap-word! [word word-map] (reset! *words-store* (assoc @*words-store* word word-map)))

(defn explode [thing] (rest (clojure.string/split (str thing) #"")))
(defn third [col] (nth col 2))
(defn my-last [thing] (last thing))

(defn sortcdr [thing]
  (sort (fn [x y] (> (last x) (last y))) thing))

(defn choose-one [list]
  "randomly pick a value from the list"
  (when-not (empty? list)
    (nth list (rand-int (count list)))))

(defn remove-duplicates [list] (distinct list))

(defn my-remove [to-be-removed list-of-things]
  "removes each element of first arg from second arg."
  (if (empty? to-be-removed)
    list-of-things
    (my-remove (rest to-be-removed)
               (remove (fn [item] (= item (first to-be-removed))) list-of-things))))

(defn pushnew [item col] (reset! col (concat [item] @col)))
(defn make-list-into-string [list] (str list))

(defn implode [thing] thing)
(defn push [item col] (reset! col (concat [item] @col)) )

(defn frequency [item list] (count (filter #(= % item) list)))

(defn set-table-sequence [dialog weights] )
(defn listp [thing] (list? thing))

(defn process-run-function [thing])
(defn make-timings [thing] thing)
(defn message-dialog [thing] (when (seq thing) (println "> " thing)))
(defn play-events [events] events)

(defn choose-the-one [stuff]
  "simply chooses one object pseudo-randomly from its arg."
  (choose-one stuff))

(defn remove-it [thing things]
  "removes its first arg from its second arg."
  (cond
   (empty? things) ()
   (= thing (ffirst things))
   (remove-it thing (rest things))
   :else (cons (first things)
               (remove-it thing (rest things)))))

(defn remove-them [list things]
  "removes its first arg from its second arg."
  (if (empty? list) things
      (remove-them (rest list) (remove-it (first list) things))))

(defn round-it [n]
  "simple utility to limit decimal places."
  (float (/ (math/round (* n 100)) 100)))

(defn other-lexicon-type [type]
  "returns words from the opposite of its arg sentence type."
  (if (= type "?") @*answer-cadence-lexicon* @*question-cadence-lexicon*))

(defn punish [associations words]
  "Punishes the weights with a * statement from user."
  (if (empty? words)
    associations
    (let [test (some (fn [word-weight]
                       (when (= (first words) (first word-weight))
                         word-weight))
                     associations)]
      (if test
        (punish (cons (list (first test) (round-it (/ (second test) weight-divisor)))
                      (remove #(= % test) associations))
                (rest words))
        (punish associations (rest words))))))

(defn get-sentence-type [sentence]
  "returns the sentence type of question or statement."
  (my-last (explode (my-last sentence))))

(defn make-new-name []
  "returns a new sentence name."
  (str "sentence-" @*counter*))

(defn get-keyword [sentence]
  "gets the keyword from its arg."
  (let [test (map (fn [word] (count (explode word))) sentence)]
    (nth sentence (position (first (my-sort > test)) test))))

(defn recognize-no [sentence]
  "this function finds the first ocurance of the no word (followed by a *) and
   places it in the *no-sentences* listing.
   calling (recognize-no (too bad you cant answer!))
   recognize-no returned nil"
  (if-not (empty? (find-no sentence))
    (pushnew (first (keys @*sentences-store*)) *no-sentences*)
    nil))

(defn recognize-yes [sentence]
  "this function finds the first ocurance of the yes word (followed by a $) and
   places it in the *yes-sentences* listing."
  (if-not (empty? (find-yes sentence))
    (pushnew (first (keys @*sentences-store*)) *yes-sentences*)
    nil))

(defn find-yes
  "tests the sentence to see if it contains the yes word."
  [sentence]
  (cond
   (or (member (first sentence) '(! ? $)) (empty? sentence)) ()
   (or (member "$" (explode (first sentence)))
       (member @*yes* (list (first sentence)))
       (if (empty? (rest sentence))
         (member @*yes* (list (implode (butlast (explode (first sentence))))))))
   (let [test (butlast (explode (first sentence)))]
     (if (= (my-last test) "*")
       (reset! *yes* (butlast (implode test)))
       (reset! *yes* (implode (list (first sentence))))))
   :else (find-yes (rest sentence))))

(defn find-no
  "tests the sentence to see if it contains the no word."
  [sentence]
  (cond
   (or (member (first sentence) '(! ? $)) (empty? sentence)) ()
   (or (member "*" (explode (first sentence)))
       (member @*no* (list (first sentence)))
       (if (empty? (rest sentence))
         (member @*no* (list (implode (butlast (explode (first sentence))))))))
   (let [test (butlast (explode (first sentence)))]
     (if (= (my-last test) "*")
       (reset! *no* (butlast (implode test)))
       (reset! *no* (implode (list (first sentence))))))
   :else (find-no (rest sentence))))

(defn establish-keywords
  "establishes all of the principal keywords."
  [sentence ]
  (let [no-test  (recognize-no sentence)
        yes-test (recognize-yes sentence)]
    (reset! *predecessor* ())
    (reset! *successor* (second sentence))
    (reset! *last-word* (my-last sentence))
    (when-not (or yes-test no-test)
      (reset! *keyword* (get-keyword sentence)))
    (when-not (or yes-test no-test)
      (pushnew @*keyword* *keywords*))
    (when-not (or yes-test no-test)
      (pushnew (my-last sentence) *last-words*))))

(defn remove-object-twice
  "removes the object and its target twice."
  ([object associations] (remove-object-twice object associations 2))
  ([object associations times]
     (cond
      (zero? times) associations
      (empty? associations)()
      (= object (ffirst associations))
      (remove-object-twice object (rest associations) (dec times))
      :else (cons (first associations)
                  (remove-object-twice object (rest associations) times)))))

(defn compound-associations
  "aggregates all of the same word weightings into single entries."
  [associations]
  (distinct
   (map (fn [[association-1 weight-1]]
          (list association-1
                (reduce (fn [tally ass]
                          (if (= (first ass) association-1)
                            (+ tally (second ass))
                            tally)) 0
                            associations)))
        associations)))

(defn compare-words
  "compares the first word with the second for similarities."
  [first-word second-word]
  (let [test-1 (explode first-word)
        test-2 (explode second-word)]
    (if (or (= test-1 (take (count test-1) (explode second-word)))
            (= test-2 (take (count test-1) (explode first-word)))) true)))

(defn make-sentence-object [sentence sentence-type name]
  "associations in this version are of four types:
  1) keyword found in *keyword*, weight being keyword-weight
  2) last words found in *last-word*, weight being last-word-weight
  3) next words found in *successor*, successor-weight
  4) all remaining words found in *all-words*, weight being backward-chain-weight
   the only exception being the word for no - this will not be in the vocabulary"
  (make-sentence name {:name (list name)
                       :sentence-type (list sentence-type)
                       :sentence (list sentence)
                       :length-of-sentence (list (count sentence))
                       :parse-it ()
                       :origination 'user})
  (swap! *counter* inc))

(defn make-weight-list [name weight]
  (list (list name weight)))

(defn add-word-to-word-weightlists
  "adds new words backchain style to all previous words in the database."
  [word keyword last-word all-words]
  (doall
   (map
    (fn [current-word]
      (when-not (= current-word word)
        (let [new-associations
              (compound-associations
               (concat (:associations (lookup-word current-word))
                       (list
                        (cond
                         (= word keyword)
                         (list word (round-it (/ keyword-weight 2)))
                         (= word last-word)
                         (list word (round-it (/ last-word-weight 2)))
                         :else (list word backward-chain-weight)))))]

          (update-word current-word :associations new-associations))))
    all-words)))

(defn build-associations
  ([word all-words] (build-associations word all-words ()))
  ([word all-words current-associations]
     (compound-associations
      (concat
       (when (and @*keyword* (not= word @*keyword*))
         (make-weight-list @*keyword* keyword-weight))
       (when (and @*last-word* (not= word @*last-word*))
         (make-weight-list @*last-word* last-word-weight))
       (when (and @*successor* (not= word @*successor*))
         (make-weight-list @*successor* successor-weight))
       (map (fn [item]
              (list item backward-chain-weight)) (my-remove (list word) all-words))
       current-associations))))

(defn update-or-create-word [word sentence sentence-type name]
  (cond
   (and (not (member word (keys @*words-store*))) (not (word-seen? word)))
   (do
     (make-word word {:name (list name)
                      :sentence-type (list sentence-type)
                      :sentence (list sentence)
                      :length-of-sentence (list (count sentence))
                      :predecessors (list @*predecessor*)
                      :successors (list @*successor*)
                      :keywords (list @*keyword*)
                      :word-type (list sentence-type)
                      :positions-in-sentence (list (inc (position word sentence)))
                      :associations (build-associations word @*all-words*)
                      :usage 1
                      :used-before? true})
     (when-not (= sentence-type "*") (push word *all-words*))
     (reset! *input-work* (rest @*input-work*)))

   (and (word-seen? word) (not (:used-before? (lookup-word word))))
   (let [word-data (lookup-word word)]
     (swap-word! word (->
                       word-data
                       (assoc :name (cons name (:name word-data)))
                       (assoc :sentence-type (list sentence-type))
                       (assoc :sentence (list sentence))
                       (assoc :length-of-sentence (list (count sentence)))
                       (assoc :predecessors (list @*predecessor*))
                       (assoc :successors (list @*successor*))
                       (assoc :keywords (list @*keyword*))
                       (assoc :positions-in-sentence (list (inc (position word sentence))))
                       (assoc :word-type (list sentence-type))
                       (assoc :associations (build-associations word @*all-words*))
                       (assoc :usage 1)
                       (assoc :used-before? true)))
     (when-not (= sentence-type "*") (push word *all-words*))
     (reset! *input-work* (rest @*input-work*)))
   :else (let [word-data (lookup-word word)]
           (swap-word! word
                       (->
                        word-data
                        (assoc :name  (cons name (:name word-data)))
                        (assoc :sentence-type (cons sentence-type (:sentence-type word-data)))
                        (assoc :sentence (cons sentence (:sentence word-data)))
                        (assoc :length-of-sentence (cons (count sentence) (:length-of-sentence word-data)))
                        (assoc :predecessors (cons @*predecessor* (:predecessors word-data)))
                        (assoc :successors (cons @*successor* (:successors word-data)))
                        (assoc :keywords (cons @*keyword* (:keywords word-data)))
                        (assoc :positions-in-sentence (cons (inc (position word sentence)) (:positions-in-sentence word-data)))
                        (assoc :word-type(cons sentence-type (:word-type word-data)))
                        (assoc :associations (build-associations word @*all-words* (:associations word-data)))
                        (assoc :usage (inc (:usage word-data)))
                        (assoc :used-before? true))))))

(defn make-word-objects [sentence sentence-type name]
  (doall
   (map (fn [word]
          (println :word word :type sentence-type)

          (update-or-create-word word sentence sentence-type name)
          (reset! *predecessor* word)

          ;;WIP
          (if (< (+ (position word sentence) 2) (count sentence))
            (reset! *successor* (nth sentence (+ (position word sentence) 2)))
            ;(reset! *successor* nil)
            )
          ;WIP(pushnew word *words-store*)

          (if (not= sentence-type "*")
            (doall (map
                    (fn [item] (add-word-to-word-weightlists item @*keyword* @*last-word* @*all-words*)) sentence)))) sentence)))

(defn figure-speac [word]
  "this function sets up parsing structure in sentences for future creation of sentences and
   atn use. important to note that word types are figured contextually based on their current usage
   and thus don't require a separate parse entry in their slots."
  (let [count-for-word (frequency word (keys @*words-store*))
        total-words (count (keys @*words-store*))]
    (cond
     (< count-for-word (/ total-words 10))
     'c
     (< count-for-word (/ total-words 8))
     'e
     (< count-for-word (/ total-words 6))
     'a
     (< count-for-word (/ total-words 4))
     'p
     :else 's)))

(defn parse-sentence [sentence name]
  "parses the sentence fully."
  (update-sentence name :parse-it (map (fn [word] (figure-speac word)) sentence)))

(defn define-incipients [sentence sentence-type]
  "defines the incipients for the sentence."
  (if (=  sentence-type "?")
    (update-sentence @*question-incipient-lexicon* :incipients
                     (cons (first sentence) (:incipients (lookup-sentence @*question-incipient-lexicon*))))
    (update-sentence @*answer-incipient-lexicon* :incipients
                     (cons (first sentence) (:incipients (lookup-sentence @*answer-incipient-lexicon*))))))

(defn define-cadences [sentence sentence-type]
  "finds and returns its arg's cadences."
  (when-not (=  sentence-type "*")
    (if (=  sentence-type "?")
      (update-sentence @*question-cadence-lexicon* :cadences
            (cons (my-last sentence) (:cadences (lookup-sentence @*question-cadence-lexicon*))))
      (update-sentence @*answer-cadence-lexicon* :cadences
            (cons (my-last sentence) (:cadences (lookup-sentence @*answer-cadence-lexicon*)))))))

(defn get-element-from-words
  "this is a test function for getting infer from words. Type can be
   predecessors successors keywords word-type positions-in-sentence associations usage music. weight is stored in associations."
  [type]
  (let [words (remove-duplicates (keys @*words-store*))]
    (map (fn [x] (list x (type (lookup-word x)))) words)))

(defn new-text []
  "gets the elements from words and sets the table sequence thusly."
  (reset! *weight-list* (or (get-element-from-words :associations) nil))
  (println (reverse @*weight-list*)))

(defn reduce-weight
  "reduces the weight of each entry  in word for all of the words in sentence."
  [word sentence]
  (let [new-associations (punish (:associations (lookup-word word)) sentence)]
    (update-word word :associations new-associations)))

(defn reduce-weighting [sentence-1 sentence-2]
  "sentence 1 here is the initiating sentence."
  (map (fn [word]
         (cons word (reduce-weight word sentence-2))) sentence-1))

(defn reward [associations words]
  "rewards the weights with a * statement from user."
  (if (empty? words) associations
      (let [test (some (fn [word-weight]
                         (when (= (first words) (first word-weight))
                           word-weight))
                       associations)]
        (if test (reward (cons (list (first test) (round-it (* (second test) weight-divisor)))
                               (remove #(= test %) associations))
                         (rest words))
            (reward associations (rest words))))))

(defn add-weight
  "increases the weight of each entry in word for all of the words in sentence."
  [word sentence]
  (println @*words-store*)
  (let [associations (:associations (lookup-word word))]
    (update-word word :associations (reward associations sentence))))

(defn add-weighting
  "sentence 1 here is the initiating sentence."
  [sentence-1 sentence-2]
  (map (fn [word] (cons word (add-weight word sentence-2))) sentence-1))

(defn get-music-associations [associations]
  (cond
   (empty? associations)()
   (:events (lookup-word (ffirst associations)))
   (cons (first associations)
         (get-music-associations (rest associations)))
   :else (get-music-associations (rest associations))))

(defn get-music-words [words]
  (cond
   (empty? words)()
   (:events (lookup-word (first words)))
   (cons (first words)
         (get-music-words (rest words)))
   :else (get-music-words (rest words))))

(defn get-word-words [words]
  (cond
   (empty? words) ()
   (:events (lookup-word (first (if (list? (first words))
                           (list (ffirst words))
                           (list (first words))))))
   (get-word-words (rest words))
   :else (cons (first words)
               (get-word-words (rest words)))))

(defn get-word-associations [associations]
  (cond
   (empty? associations)()
   (:events (lookup-word (ffirst associations)))
   (get-word-associations (rest associations))
   :else (cons (first associations)
           (get-word-associations (rest associations)))))

(defn choose-the-highest-rated-word [words]
  "chooses the highest choice from among ties for the honor."
  (first
   (choose-one (let [rated-words (sortcdr words)
                     rating (second (first rated-words))]
                 (remove (fn [word] (when-not (= (second word) rating) true)) rated-words)))))


(defn current-words-list [current-word cadences]
  (loop [current-word current-word
         collected-words []]
    (if (or (nil? current-word) (member current-word cadences))
      collected-words
      (do
        (pushnew current-word *current-words*) ;;;these must be subtracted from options to avoid repeats
        (let [collected-words-col
              (cons collected-words
                    (let [test (choose-the-highest-rated-word
                                (remove-them
                                 (concat @*current-words*
                                         (get-music-words (:cadences
                                                           (if (= type "?")
                                                             @*question-cadence-lexicon*
                                                             @*answer-cadence-lexicon*))))
                                 (get-music-associations (:associations (lookup-word current-word)))))]
                      (if test
                        test
                        (choose-the-one
                         (get-music-words (map (fn [association] (first association) ) (:associations (lookup-word current-word))))))))]
          (recur current-word collected-words-col))))))

(defn- pick-words [current-word]
  (let [test (choose-the-highest-rated-word
              (remove-them
               (concat @*current-words*
                       (get-word-words (:cadences
                                        (if (= type "?")
                                          @*question-cadence-lexicon*
                                          @*answer-cadence-lexicon*))))
               (get-word-associations (:associations (lookup-word current-word)))))]
    (if test
      test
      (choose-the-one
       (get-word-words (map (fn [association] (first association)) (:associations (lookup-word current-word))))))))

(defn choices-thing [type sentence]
  (let [choices (compound-associations
                 (apply concat
                        (map (fn [word] (get-music-associations (:associations (lookup-word word)))) sentence)))
        incipients (if (= type "?")
                     (my-remove (list (eval @*no*))
                                (get-music-words (:incipients @*answer-incipient-lexicon*)))
                     (get-music-words (:incipients @*question-incipient-lexicon*)))]
    (reset! *current-words* ())
    (if (or (empty? choices) (empty? incipients))
      ()
      (let [current-word                                            ;;;here's where we get a current word - the highest rated word in choices
            (let [trial (choose-the-highest-rated-word
                         (remove-them
                          (get-music-words (:cadences (if (=  type "?")
                                                        @*question-cadence-lexicon*
                                                        @*answer-cadence-lexicon*)) )
                          choices))]
              (if trial trial (get-music-words (choose-one incipients))))            ;;;here is where we resort to incipients if necessary
            cadences (get-music-words (:cadences (lookup-sentence (other-lexicon-type type))))]  ;;;this variable will indicate when we must stop!
        (let [new-sentence (cons current-word (current-words-list current-word cadences))]
          new-sentence)))))

(defn default-reply-thing [type sentence]
  (let [choices (compound-associations                              ;;;this is a pro-rated list of all of the associations of the sentence argument
                 (apply concat
                        (map (fn [word] (get-word-associations (:associations (lookup-word word)))) sentence)))
        incipients (if (=  type "?")     ;;;this is a just in case choices is nil listing of alternatives sentence incipients
                     (my-remove (list (eval @*no*))
                                (get-word-words (:incipients @*answer-incipient-lexicon*)))
                     (get-word-words (:incipients @*question-incipient-lexicon*)))]
    (reset! *current-words* ())
    (if (or (nil? choices) (nil? incipients))
      ()
      (let [current-word (let [trial (choose-the-highest-rated-word
                                      (remove-them
                                       (get-word-words (:cadences (if (=  type "?")
                                                                    @*question-cadence-lexicon*
                                                                    @*answer-cadence-lexicon*)))
                                       choices))]
                           (if trial trial (choose-one (get-word-words incipients))))
            cadences (:cadences (lookup-sentence (other-lexicon-type type)))]
        (let [new-sentence
              (cons current-word
                    (loop [current-word current-word
                           current-words []]
                      (if (or (nil? current-word) (member current-word cadences))
                        current-words
                        (let [current-word (pick-words current-word)
                              new-current-words (cons current-word  current-words)]
                          (pushnew current-word *current-words*)
                          (recur current-word new-current-words)))))]
          new-sentence)))))

(defn process-no []
  (reduce-weighting (first (:sentence (lookup-sentence (third (keys @*sentences-store*)))))
                    (first (:sentence (lookup-sentence (second (keys @*sentences-store*))))))
  (list "*"))

(defn process-yes []
  (add-weighting (first (:sentence (lookup-sentence (third (keys @*sentences-store*)))))
                 (first (:sentence (lookup-sentence (second (keys @*sentences-store*))))))
  (list "$"))

(defn reply [type sentence]
  "this function creates sentences by using the various associations in each word in the sentence argument."
  (cond
   (recognize-no sentence)
   (process-no)

   (recognize-yes sentence)
   (process-yes)

   (:events (lookup-word (first sentence)))
   (choices-thing type sentence)

   :else (default-reply-thing type sentence)))

(defn display [response]
  "simple making of list into string."
  (make-list-into-string response))

(defn put-sentence-into-database [sentence]
  "puts the sentence into the database."
  (establish-keywords sentence)
  (let [sentence-type (get-sentence-type sentence)
        name (make-new-name)]
    (make-sentence-object sentence sentence-type name)
    (make-word-objects sentence sentence-type name)

    (parse-sentence sentence name)
    (define-incipients sentence sentence-type)
    (define-cadences sentence sentence-type)

    (new-text)
    (reply sentence-type sentence)))

(defn fix-end-of-music-sentences [sentence]
  "attaches the punctuation to the end of the music sentence."
  (let [object (nth (- (count sentence) 2) sentence)
        the-name (implode (drop (- (count sentence) 2) sentence))]
    (make-word the-name {:name (:name (eval object))
                         :timing (:timing (eval object))
                         :destination (:destination (eval object))
                         :events (:events (eval object))
                         :usage (:usage (eval object))})
    (concat (butlast sentence 2) (list the-name))))

(defn event-loop []
  (loop []
    (let [user-input (read-string (read-line))
          input (if (and (word-seen? (first user-input))
                           (:events (lookup-word (first user-input))))
                  (fix-end-of-music-sentences user-input)
                  user-input)
          response (put-sentence-into-database input)]
      (when-not (empty? response)
        (let [name (implode (str "sentence-" @*counter*))
              sentence-type (my-last (explode (my-last response)))]
          (make-sentence name {:name 'me
                               :sentence-type sentence-type
                               :sentence (list response)
                               :length-of-sentence (count response)
                               :origination 'apprentice})
          (swap! *counter* inc)))
      (when-not (empty? response)
        (new-text)
        (if (and (not= (first response) "*")
                 (not= (first response) "$")
                 (not (nil? (first response)))
                 (:events (lookup-word (first response))))
          (process-run-function "play" 'play-events
                                (apply concat
                                       (make-timings
                                        (map (fn [x](:events (lookup-word x))) response)))))
        (message-dialog (make-list-into-string response)))
      (recur))))

(defn apprentice []
  "this function runs the program from the menu."
  (when @*initiate* (reset! *all-words* ()))

  (reset! *no* ())
  (reset! *yes* ())

  (event-loop))

(defn remove-cadences [choices]
  "removes the cadences from the choices."
  (cond
   (empty? choices)()
   (or (member (second (first choices)) (:cadences @*answer-cadence-lexicon*))
       (member (second (first choices)) (:cadences @*question-cadence-lexicon*)))
   (remove-cadences (rest choices))
   :else (cons (first choices)(remove-cadences (rest choices)))))

(defn pair [list-1 list-2]
  "pairs the two list args."
  ;; (loop for item in list-1
  ;;       collect (list item (first list-2))
  ;;       do (setf list-2 (rest list-2)))
)

(defn collect-parsings [sentences]
  "pairs the parsings with the words in the sentences."
  (pair (apply concat (map (fn [sentence] (first (:sentence (eval sentence)))) sentences))
        (apply concat (map (fn [sentence] (:parse-it (eval sentence))) sentences))))

(defn mix [list]
  "pseudo-randomly mixes the list arg."
  (if (empty? list) nil
      (let [choice (choose-one list)]
        (cons choice (mix (remove choice list :count 1))))))

(defn get-parse-elements [parse]
  "gets the speac parase elements for the speac sentence."
  (if (empty? parse)()
      (cons (first parse)
            (get-parse-elements (remove (fn [p] (= p (first parse))) parse)))))

(defn relate-words [words associations]
  "relates the words with their speac symbols to words with weightings alone."
  (cond
   (empty? words) ()
   (let [test (assoc (ffirst words) associations)]
     (if test (cons test (relate-words (rest words) associations)))) '(wat)
     :else (relate-words (rest words) associations)))

(defn get-associations [parsed-words]
  "gets the associations for its arg."
  (let [relevant-words
        (compound-associations
         (apply concat
                (map (fn [word] (:associations (lookup-word word)))
                     (first (:sentence (eval (first @*sentences-store*))))
                      )))]
    (relate-words relevant-words parsed-words)))

(defn parse-the-sentence [parse cadence]
  "the argument here is the parse found in any sentence parse-it slot."
  (concat (let [parse-lists (remove-cadences
                             (map (fn [item] (reverse item)) (get-associations (collect-parsings @*sentences-store*))))]
            (map (fn [element] (second (assoc element (mix parse-lists)))) (butlast parse)))
          (list (choose-one cadence))))

(defn any-greater-than? [n list-of-numbers]
  "determines if any in second arg are greater than its first arg."
  (cond (empty? list-of-numbers)()
        (> (first list-of-numbers) n) true
        :else (any-greater-than? n (rest list-of-numbers))))

(defn set-to-zero
  "sets the events to begin on zero."
  ([events] (set-to-zero events (ffirst events)))
  ([events subtract]
      (if (empty? events)()
          (cons (cons (- (ffirst events) subtract)
                      (rest (first events)))
                (set-to-zero (rest events) subtract)))))


(defn get-note-timing [event time]
  "grunt work for get-beat-length"
  (- (+ (first event)(third event)) time))

(defn get-beat-length [events]
  "this is used in re-time for setting the new time!
   requires that the first in events be sorted to current time!"
  (let [time (ffirst events)]
    (first (my-sort > (map (fn [event] (get-note-timing event time)) events)))))


(defn re-time
  "retimes the events lists to begin one after the other."
  ([event-lists] (re-time event-lists 0))
  ([event-lists current-time ]
      (if (empty? event-lists)()
          (cons (map (fn [event] (cons (+ (first event) current-time) (rest event)) ) (set-to-zero (first event-lists)))
                (re-time (rest event-lists) (+ current-time
                                               (get-beat-length
                                                (first event-lists))))))))

(defn play-sentence [sentence]
  "plays a sentence of word objects."
  (play-events (apply concat (re-time (map (fn [word] (:events (lookup-word word))) (remove (fn [word] (when-not (:events (lookup-word word)) true)) sentence))))))

(defn reveal-the-hidden-events [events]
  "reveals the events in words."
  (map (fn [x] (:events (eval x))) events))

(defn tester-for-hidden-events [sentence]
  "tests to see if arg is a music sentence."
  (and (sentence-seen? (first sentence))
       (:events (lookup-sentence (first sentence)))))

(defn return-only-music-sentences [sentence-objects]
  "the arg to this should be *sentences-store*."
  (let [real-sentences (apply concat (map (fn [x] (:sentence (eval x))) sentence-objects))]
    (remove (fn [sentence] (when-not (tester-for-hidden-events sentence) true)) real-sentences)))

(defn create-a-work [sentences]
 "the arg to this should be *sentences-store*"
  (reveal-the-hidden-events (apply concat (reverse (return-only-music-sentences sentences)))))

