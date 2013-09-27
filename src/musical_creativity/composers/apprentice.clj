(ns musical-creativity.composers.apprentice
  (:require
   [clojure.math.numeric-tower :as math]))

(def *hori-cons* 50)

(def *sentences* (atom {}))
(def *no-sentences* (atom ()))
(def *yes-sentences* (atom ()))
(def *yes* (atom ()))
(def *no* (atom ()))
(def *keyword* (atom ()))
(def *keywords* (atom ()))
(def *counter* (atom 0))
(def *keyword-weight* (atom 0.15))
(def *last-word-weight* (atom 0.2))
(def *successor-weight* (atom 0.5))
(def *backward-chain-weight* (atom 0.1))

(def *predecessor* (atom nil))
(def *successor* (atom nil))
(def *last-word* (atom nil))
(def *last-words* (atom ()))
(def *words* (atom {}))
(def *all-words* (atom ()))
(def *input-work* (atom ()))
(def *weight-list* (atom ()))
(def *response* (atom ()))
(def *weight-divisor* (atom 2))
(def *current-words* (atom ()))
(def *initiate* (atom true))
(def *input* (atom ()))
(def *name-list* (atom ()))
(def *process* (atom ()))

(defn make-incipient-lexicon [] {:incipients []})
(defn make-candence-lexicon  [] {:cadences []})

(def *question-incipient-lexicon* (atom (make-incipient-lexicon)))
(def *answer-incipient-lexicon*   (atom (make-incipient-lexicon)))
(def *question-cadence-lexicon*   (atom (make-candence-lexicon )))
(def *answer-cadence-lexicon*     (atom (make-candence-lexicon)))

(def *dialog-text* (atom ""))

(declare find-no find-yes)

(defn member [item col] (boolean (some #{item} col)))
(defn my-sort [fun things] (sort fun things))

(defn position [thing list]
  "returns position of thing in list or nil"
  (let [index (.indexOf list thing)]
    (when (>= index 0) index)))

(defn lookup-sentence [sentence]
  (@*sentences* sentence))

(defn make-sentence [id atts]  (reset! *sentences* (assoc @*sentences* id atts)))
(defn update-sentence [id field val] (reset! *sentences* (assoc-in @*sentences* [id field] val))
  val)
(defn sentence-seen? [sentence] (some #{sentence} (keys @*sentences*)))

(defn lookup-word [word]  (@*words* word))
(defn make-word [id atts]  (reset! *words* (assoc @*words* id atts)))
(defn update-word [id field val]
  (reset! *words* (assoc-in @*words* [id field] val))
  val)

(defn word-seen? [word] (some #{word} (keys @*words*)))
(defn swap-word! [word word-map] (reset! *words* (assoc @*words* word word-map)))

(defn explode [thing] (rest (clojure.string/split (str thing) #"")))
(defn third [col] (nth col 2))
(defn my-last [thing] (last thing))

(defn sortcdr [fun thing]
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
(defn boundp [thing] false )

(defn push [item col] (reset! col (concat [item] @col)) )

(defn frequency [item list] (count (filter #(= % item) list)))

(defn set-table-sequence [dialog weights] )
(defn listp [thing] (list? thing))
(defn read-from-string [thing] thing)
(defn process-run-function [thing])
(defn make-timings [thing] thing)
(defn message-dialog [thing] (println thing))
(defn play-events [events] events)

(defn choose-the-one [stuff]
  "simply chooses one object pseudo-randomly from its arg."
  (choose-one stuff))

(defn remove-it [thing things]
  "removes its first arg from its second arg."
  (cond
   (empty? things)()
   (= thing (ffirst things))
   (remove-it thing (rest things))
   :else (cons (first things)
               (remove-it thing (rest things)))))

(defn remove-them [list things]
  "removes its first arg from its second arg."
  (if (empty? list) things
      (remove-them (rest list)(remove-it (first list) things))))

(defn round-it [n]
  "simple utility to limit decimal places."
  (float (/ (math/round (* n 100)) 100)))

(defn other-lexicon-type [type]
  "returns words from the opposite of its arg sentence type."
  (if (= type '?) @*answer-cadence-lexicon* @*question-cadence-lexicon*))

(defn punish [associations words]
  "Punishes the weights with a * statement from user."
  (if (empty? words)
    associations
    (let [test (some (fn [word-weight]
                       (when (= (first words) (first word-weight))
                         word-weight))
                     associations)]
      (if test
        (punish (cons (list (first test) (round-it (/ (second test) @*weight-divisor*)))
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
    (pushnew (first (keys @*sentences*)) *no-sentences*)
    nil))

(defn recognize-yes [sentence]
  "this function finds the first ocurance of the yes word (followed by a ^) and
   places it in the *yes-sentences* listing."
  (if-not (empty? (find-yes sentence))
    (pushnew (first (keys @*sentences*)) *yes-sentences*)
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
  [sentence]
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
          [association-1 (reduce (fn [tally ass]
                                   (if (= (first ass) association-1)
                                     (+ tally (second ass))
                                     tally)) 0
                                     associations)])
        associations)))

(defn compare-words [first-word second-word]
  "compares the first word with the second for similarities."
  (let [test-1 (explode first-word)
        test-2 (explode second-word)]
    (if (or (=  test-1 (take (explode second-word) (count test-1)))
            (=  test-2 (take (explode first-word) (count test-1)))) true)))

(defn make-sentence-object [sentence sentence-type name]
  "associations in this version are of four types:
  1) keyword found in *keyword*, weight being *keyword-weight*
  2) last words found in *last-word*, weight being *last-word-weight*
  3) next words found in *successor*, *successor-weight*
  4) all remaining words found in *all-words*, weight being *backward-chain-weight*
   the only exception being the word for no - this will not be in the vocabulary"
  (make-sentence name {:name (list name)
                       :sentence-type (list sentence-type)
                       :sentence (list sentence)
                       :length-of-sentence (list (count sentence))
                       :parse-it ()
                       :origination 'user})
  (swap! *counter* inc))

(defn make-weight-list [name weight]
  "a simple cover for double listing."
  (list (list name weight)))

(defn add-word-to-word-weightlists
  "adds new words backchain style to all previous words in the database."
  [word]
  (doall
   (map
    (fn [item]
      (when-not (= item word)
        (update-word item :associations
                     (compound-associations
                      (concat (:associations (lookup-word item))
                              (list
                               (cond
                                (=  word @*keyword*)
                                (list word (round-it (/ @*keyword-weight* 2)))
                                (= word @*last-word*)
                                (list word (round-it (/ @*last-word-weight* 2)))
                                :else (list word @*backward-chain-weight*))))))))

    @*all-words*)))

(defn build-associations [word]
  (compound-associations
   (concat
    (when (and @*keyword* (not= word @*keyword*))
      (make-weight-list @*keyword* @*keyword-weight*))
    (when (and @*last-word* (not= word @*last-word*))
      (make-weight-list @*last-word* @*last-word-weight*))
    (when (and @*successor* (not= word @*successor*))
      (make-weight-list @*successor* @*successor-weight*))
    (map (fn [item]
           (list item @*backward-chain-weight*)) (my-remove (list word) @*all-words*)))))

(defn make-word-objects [sentence sentence-type name]
  "makes the words objects for colaborator."
  (doall
   (map (fn [word]
          (cond
           (and (not (member word (keys @*words*))) (not (word-seen? word)))
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
                              :associations (build-associations word)

                              :usage 1
                              :used-before? true})
             (if (not (=  sentence-type '*))
               (push word *all-words*))
             (reset! *input-work* (rest @*input-work*)))

           (and (word-seen? word) (not (:used-before? (lookup-word word))))
           (do
             (swap-word! word (->
                               (lookup-word word)
                               (assoc :name (cons name (:name (lookup-word word))))
                               (assoc :sentence-type (list sentence-type))
                               (assoc :sentence (list sentence))
                               (assoc :length-of-sentence (list (count sentence)))
                               (assoc :predecessors (list @*predecessor*))
                               (assoc :successors (list @*successor*))
                               (assoc :keywords (list @*keyword*))
                               (assoc :positions-in-sentence (list (inc (position word sentence))))
                               (assoc :word-type (list sentence-type))
                               (assoc :associations
                                 (compound-associations
                                  (concat (if (and @*keyword* (not (=  word @*keyword*)))
                                            (make-weight-list @*keyword* @*keyword-weight*))
                                          (if (and @*last-word* (not (=  word @*last-word*)))
                                            (make-weight-list @*last-word* @*last-word-weight*))
                                          (if (and @*successor* (not (=  word @*successor*)))
                                            (make-weight-list @*successor* @*successor-weight*))
                                          (map (fn [item]
                                                 (list item @*backward-chain-weight*)) (my-remove (list word) @*all-words*)))))
                               (assoc :usage 1)
                               (assoc :used-before? true)))
             (if (not (=  sentence-type '*))
               (push word *all-words*))
             (reset! *input-work* (rest @*input-work*)))
           :else (swap-word! word  (->
                                    (lookup-word word)
                                    (assoc :name  (cons name (:name (lookup-word word))))
                                    (assoc :sentence-type (cons sentence-type (:sentence-type (lookup-word word))))
                                    (assoc :sentence (cons sentence (:sentence (lookup-word word))))
                                    (assoc :length-of-sentence (cons (count sentence) (:length-of-sentence (lookup-word word))))
                                    (assoc :predecessors (cons @*predecessor* (:predecessors (lookup-word word))))
                                    (assoc :successors (cons @*successor* (:successors (lookup-word word))))
                                    (assoc :keywords (cons @*keyword* (:keywords (lookup-word word))))
                                    (assoc :positions-in-sentence (cons (inc (position word sentence)) (:positions-in-sentence (lookup-word word))))
                                    (assoc :word-type(cons sentence-type (:word-type (lookup-word word))))
                                    (assoc :associations
                                      (compound-associations
                                       (concat (if (and @*keyword* (not (=  word @*keyword*)))
                                                 (make-weight-list @*keyword* @*keyword-weight*))
                                               (if (and @*last-word* (not (=  word *last-word*)))
                                                 (make-weight-list @*last-word* @*last-word-weight*))
                                               (if (and @*successor* (not (=  word @*successor*)))
                                                 (make-weight-list @*successor* @*successor-weight*))
                                               (map (fn [item]
                                                      (list item @*backward-chain-weight*)) (my-remove (list word) @*all-words*))
                                               (:associations (lookup-word word)))))
                                    (assoc :usage (inc (:usage (lookup-word word))))
                                    (assoc :used-before? true))))

          (reset! *predecessor* word)
          ;WIP(reset! *successor* (nth sentence (+ (position word sentence) 2)))
          ;WIP(pushnew word *words*)
          (if (not (=  sentence-type '*))
            (doall (map
                    (fn [item] (add-word-to-word-weightlists item)) sentence)))) sentence)))

(defn figure-speac [word]
  "this function sets up parsing structure in sentences for future creation of sentences and
   atn use. important to note that word types are figured contextually based on their current usage
   and thus don't require a separate parse entry in their slots."
  (let [count-for-word (frequency word (keys @*words*))
        total-words (count (keys @*words*))]
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
  (update-sentence name :parse-it
        (map (fn [word]
               (figure-speac word) ) sentence)))

(defn define-incipients [sentence sentence-type]
  "defines the incipients for the sentence."
  (if (=  sentence-type '?)
    (update-sentence @*question-incipient-lexicon* :incipients
          (cons (first sentence) (:incipients (lookup-sentence @*question-incipient-lexicon*))))
    (update-sentence @*answer-incipient-lexicon* :incipients
          (cons (first sentence) (:incipients (lookup-sentence @*answer-incipient-lexicon*))))))

(defn define-cadences [sentence sentence-type]
  "finds and returns its arg's cadences."
  (when-not (=  sentence-type '*)
    (if (=  sentence-type '?)
      (update-sentence @*question-cadence-lexicon* :cadences
            (cons (my-last sentence) (:cadences (lookup-sentence @*question-cadence-lexicon*))))
      (update-sentence @*answer-cadence-lexicon* :cadences
            (cons (my-last sentence) (:cadences (lookup-sentence @*answer-cadence-lexicon*)))))))

(defn get-element-from-words
  "this is a test function for getting infer from words. Type can be
   predecessors successors keywords word-type positions-in-sentence associations usage music. weight is stored in associations."
  [type]
  (let [words (remove-duplicates (keys @*words*))]
    (map (fn [x] (list x (type (lookup-word x)))) words)))

(defn new-text []
  "gets the elements from words and sets the table sequence thusly."
  (reset! *weight-list*
          (let [test (get-element-from-words :associations)]
            (or test nil)))
  (set-table-sequence @*dialog-text* (reverse @*weight-list*)))

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
      (let [test (some #{(first words)}  associations)]
        (if test (reward (cons (list (first test) (round-it (* (second test) @*weight-divisor*)))
                               (remove test associations))
                         (rest words))
            (reward associations (rest words))))))

(defn add-weight [word sentence]
  "increases the weight of each entry  in word for all of the words in sentence."
  (let [associations (:associations (lookup-word word))]
    (update-word word :associations
                 (reward associations sentence))))

(defn add-weighting
  "sentence 1 here is the initiating sentence."
  [sentence-1 sentence-2]
  (map (fn [word]
         (cons word (add-weight word sentence-2))) sentence-1))

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
   (choose-one (let [rated-words (sortcdr > words)
                     rating (second (first rated-words))]
                 (remove (fn [word] (when-not (= (second word) rating) true)) rated-words)))))


(defn current-words-list [current-word cadences]
  (loop [current-word current-word
         collected-words []]

    (if (or (nil? current-word) (member current-word cadences))
      collected-words
      (do
        ;(setq test-word current-word)
        ;(setq tester-word (member current-word cadences))
        (pushnew current-word *current-words*) ;;;these must be subtracted from options to avoid repeats
        (let [collected-words-col (cons collected-words
                                        (let [test (choose-the-highest-rated-word
                                                    (remove-them
                                                     (concat @*current-words*
                                                             (get-music-words (:cadences
                                                                               (if (=  type '?)
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
                                        (if (= type '?)
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
        incipients (if (= type '?)
                     (my-remove (list (eval @*no*))
                                (get-music-words (:incipients @*answer-incipient-lexicon*)))
                     (get-music-words (:incipients @*question-incipient-lexicon*)))]
    (reset! *current-words* ())
    (if (or (empty? choices) (empty? incipients))
      ()
      (let [current-word                                            ;;;here's where we get a current word - the highest rated word in choices
            (let [trial (choose-the-highest-rated-word
                         (remove-them
                          (get-music-words (:cadences (if (=  type '?)
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
        incipients (if (=  type '?)     ;;;this is a just in case choices is nil listing of alternatives sentence incipients
                     (my-remove (list (eval @*no*))
                                (get-word-words (:incipients @*answer-incipient-lexicon*)))
                     (get-word-words (:incipients @*question-incipient-lexicon*)))]
    (reset! *current-words* ())
    (if (or (nil? choices) (nil? incipients))
      ()
      (let [current-word (let [trial (choose-the-highest-rated-word
                                      (remove-them
                                       (get-word-words (:cadences (if (=  type '?)
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
  (reduce-weighting (first (:sentence (lookup-sentence (third (keys @*sentences*)))))
                    (first (:sentence (lookup-sentence (second (keys @*sentences*))))))
  (list '*))

(defn process-yes []
  (add-weighting (first (:sentence (lookup-sentence (third (keys @*sentences*)))))
                 (first (:sentence (lookup-sentence (second (keys @*sentences*))))))
  (list '$))

(defn reply [type sentence]
  "this function creates sentences by using the various associations in   each word in the sentence argument."
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
    (reset! *response* (reply sentence-type sentence))
    (display @*response*)))

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
    (reset! *input* @*name-list*)
    (when (and (boundp (first @*name-list*)) (:events (lookup-word (first @*name-list*))))
      (reset! *name-list* (fix-end-of-music-sentences @*name-list*))
      (reset! *input* @*name-list*))

    (let [trial (put-sentence-into-database @*input*)]
      (if (not (empty? (read-from-string trial)))
        (do (let [name (implode (list 'sentence- @*counter*))
                  sentence-type (my-last (explode (my-last @*response*)))]
              (make-sentence name {:name 'me
                                   :sentence-type sentence-type
                                   :sentence (list @*response*)
                                   :length-of-sentence (count @*response*)
                                   :origination 'apprentice})
              (pushnew name *sentences*)
              (swap! @*counter* inc)))))
    (if (not (empty? @*response*))
      (do (new-text)
          (if (and (not (= (first @*response*) '*)) (not (= (first @*response*) '$))
                   (not (empty? (first @*response*)))
                   (:events (lookup-word (first @*response*))))
            (reset! *process* (process-run-function "play" 'play-events (apply concat (make-timings (map (fn [x](:events (eval x))) @*response*))))))
          (message-dialog (make-list-into-string @*response*)))
      (do (new-text)
          (message-dialog " ------- ")))))

(defn apprentice []
  "this function runs the program from the menu."
  (when @*initiate* (reset! *all-words* nil))

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
                (map (fn [word] (:associations (lookup-word word)) ) (first (:sentence (eval (first @*sentences*))))
                      )))]
    (relate-words relevant-words parsed-words)))

(defn parse-the-sentence [parse cadence]
  "the argument here is the parse found in any sentence parse-it slot."
  (concat (let [parse-lists (remove-cadences
                             (map (fn [item] (reverse item)) (get-associations (collect-parsings @*sentences*))))]
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
    (first (my-sort #'> (map (fn [event] (get-note-timing event time)) events)))))


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
  (and (boundp (first sentence))
       (:events (lookup-sentence (first sentence)))))

(defn return-only-music-sentences [sentence-objects]
  "the arg to this should be *sentences*."
  (let [real-sentences (apply concat (map (fn [x] (:sentence (eval x))) sentence-objects))]
    (remove (fn [sentence] (when-not (tester-for-hidden-events sentence) true)) real-sentences)))

(defn create-a-work [sentences]
 "the arg to this should be *sentences*"
  (reveal-the-hidden-events (apply concat (reverse (return-only-music-sentences sentences)))))

