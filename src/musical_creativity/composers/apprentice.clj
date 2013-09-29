(ns musical-creativity.composers.apprentice
  (:require
   [clojure.string :as string]
   [clojure.math.numeric-tower :as math]
   [musical-creativity.util :refer [position choose-one third frequency]]))

(def weight-divisor 2)
(def keyword-weight 0.75)
(def successor-weight 0.5)
(def backward-chain-weight 0.1)
(def last-word-weight 0.2)
(def predecessors-weight 0.5)
(def broad-keyword-weight 0.1)

(def ^:dynamic *counter* (atom 0))

(def ^:dynamic *sentences-store* (atom {}))
(def ^:dynamic *words-store*     (atom {}))

(def ^:dynamic *no-sentences*  (atom ()))
(def ^:dynamic *yes-sentences* (atom ()))
(def ^:dynamic *yes* (atom ()))
(def ^:dynamic *no*  (atom ()))

(def ^:dynamic *all-words*     (atom ()))
(def ^:dynamic *current-words* (atom ()))

(defn make-incipient-lexicon [] {:incipients ()})
(defn make-candence-lexicon  [] {:cadences ()})

(def ^:dynamic *question-incipient-lexicon* (atom (make-incipient-lexicon)))
(def ^:dynamic *answer-incipient-lexicon*   (atom (make-incipient-lexicon)))
(def ^:dynamic *question-cadence-lexicon*   (atom (make-candence-lexicon )))
(def ^:dynamic *answer-cadence-lexicon*     (atom (make-candence-lexicon)))

(defn reset-all! []
  (reset! *counter* 0)
  (reset! *sentences-store* {})
  (reset! *no-sentences* ())
  (reset! *yes-sentences* ())
  (reset! *yes* ())
  (reset! *no* ())
  (reset! *words-store* {})
  (reset! *all-words* ())
  (reset! *current-words* ())
  (reset! *question-incipient-lexicon* (make-incipient-lexicon))
  (reset! *answer-incipient-lexicon*   (make-incipient-lexicon))
  (reset! *question-cadence-lexicon*   (make-candence-lexicon))
  (reset! *answer-cadence-lexicon*     (make-candence-lexicon)))

(declare find-no find-yes)

(def question-type "?")
(def fact-type "!")
(def negative-type "*")
(def positive-type "$")

(defn question? [type] (= type question-type))
(defn fact? [type] (= type fact-type))
(defn negative? [type] (= type negative-type))
(defn positive? [type] (= type positive-type))

(defn member [item col] (boolean (some #{item} col)))

(defn all-sentences []
  (sort
   (fn [x y]
     (> (Integer/parseInt (last (string/split (str x) #"-")))
        (Integer/parseInt (last (string/split (str y) #"-")))))
   (keys @*sentences-store*)))

(defn lookup-sentence [sentence] (@*sentences-store* sentence))
(defn make-sentence [id atts]  (reset! *sentences-store* (assoc @*sentences-store* id atts)))
(defn update-sentence [id field val] (reset! *sentences-store* (assoc-in @*sentences-store* [id field] val))
  val)
(defn sentence-seen? [sentence] (some #{sentence} (all-sentences)))

(defn lookup-word [word]  (@*words-store* word))
(defn make-word [id atts]  (reset! *words-store* (assoc @*words-store* id atts)))
(defn update-word [id field val]
  (reset! *words-store* (assoc-in @*words-store* [id field] val))
  val)

(defn word-seen? [word] (some #{word} (keys @*words-store*)))
(defn swap-word! [word word-map] (reset! *words-store* (assoc @*words-store* word word-map)))

(defn update-incipient [ref new-value]
  (reset! ref {:incipients new-value})
  new-value)

(defn update-cadence [ref new-value]
  (reset! ref {:cadences new-value})
  new-value)

(defn explode [thing] (map str (vec (str thing))))
(defn implode [list] list)

(defn sort-by-last [thing]
  (sort (fn [x y] (> (last x) (last y))) thing))

(defn push-new [item col] (reset! col (concat [item] @col)))

(defn push [item col] (reset! col (concat [item] @col)))

(defn play-events [events] (println :play events))

(defn message [thing] (when (seq thing) (println "Alice> " (str thing))))

(defn remove-all
  "removes each element of first arg from second arg."
  [to-be-removed list-of-things]
  (if (empty? to-be-removed)
    list-of-things
    (remove-all (rest to-be-removed)
               (remove (fn [item] (= item (first to-be-removed))) list-of-things))))

(defn remove-by-ffirst [thing things]
  (cond
   (empty? things) ()
   (= thing (ffirst things))
   (remove-by-ffirst thing (rest things))
   :else (cons (first things)
               (remove-by-ffirst thing (rest things)))))

(defn remove-all-by-ffirst
  [list things]
  (if (empty? list)
    things
    (remove-all-by-ffirst (rest list) (remove-by-ffirst (first list) things))))

(defn round-it [n]
  (float (/ (math/round (* n 100)) 100)))

(defn other-lexicon-type
  "returns words from the opposite of its arg sentence type."
  [type]
  (if (question? type) @*answer-cadence-lexicon* @*question-cadence-lexicon*))

(defn punish
  "Punishes the weights with a * statement from user."
  [associations words]
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

(defn get-sentence-type
  "returns the sentence type of question or statement."
  [sentence]
  (last (explode (last sentence))))

(defn make-new-name
  "returns a new sentence name."
  []
  (str "sentence-" @*counter*))

(defn get-keyword [sentence]
  (let [test (map (fn [word] (count (explode word))) sentence)]
    (nth sentence (position (first (sort > test)) test))))

(defn recognize-no
  "finds the first ocurance of the no word (followed by a *) and
   places it in the *no-sentences* listing."
  [sentence]
  (if-not (empty? (find-no sentence))
    (push-new (first (all-sentences)) *no-sentences*)
    nil))

(defn recognize-yes
  "finds the first ocurance of the yes word (followed by a $) and
   places it in the *yes-sentences* listing."
  [sentence]
  (if-not (empty? (find-yes sentence))
    (push-new (first (all-sentences)) *yes-sentences*)
    nil))

(defn find-yes
  "tests the sentence to see if it contains the yes word."
  [sentence]
  (cond
   (or (member (first sentence) (list fact-type question-type positive-type)) (empty? sentence)) ()
   (or (member positive-type (explode (first sentence)))
       (member @*yes* (list (first sentence)))
       (if (empty? (rest sentence))
         (member @*yes* (list (implode (butlast (explode (first sentence))))))))
   (let [test (butlast (explode (first sentence)))]
     (if (= (last test) negative-type)
       (reset! *yes* (butlast (implode test)))
       (reset! *yes* (implode (list (first sentence))))))
   :else (find-yes (rest sentence))))

(defn find-no
  "tests the sentence to see if it contains the no word."
  [sentence]
  (cond
   (or (member (first sentence) (list fact-type question-type positive-type)) (empty? sentence)) ()
   (or (member negative-type (explode (first sentence)))
       (member @*no* (list (first sentence)))
       (if (empty? (rest sentence))
         (member @*no* (list (implode (butlast (explode (first sentence))))))))
   (let [test (butlast (explode (first sentence)))]
     (if (= (last test) negative-type)
       (reset! *no* (butlast (implode test)))
       (reset! *no* (implode (list (first sentence))))))
   :else (find-no (rest sentence))))

(defn establish-keywords
  "establishes all of the principal keywords."
  [sentence]
  (let [no-test  (recognize-no sentence)
        yes-test (recognize-yes sentence)
        sentence-context {:last-word (last sentence)}]
    (if-not (or yes-test no-test)
      (merge sentence-context {:keyword (get-keyword sentence)})
      sentence-context)))

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

(defn make-sentence-object
  "associations are of four types:
  1. keyword found: weight keyword-weight
  2. last word found: weight last-word-weight
  3. next word found in successor: successor-weight
  4. all remaining words found in *all-words*, weight being backward-chain-weight
   the only exception being the word for no - this will not be in the vocabulary"
  [sentence sentence-type name]
  (make-sentence name {:name (list name)
                       :sentence-type (list sentence-type)
                       :sentence (list sentence)
                       :length-of-sentence (list (count sentence))
                       :parse-it ()
                       :origination 'user})
  (swap! *counter* inc))

(defn make-weight-list [name weight] (list (list name weight)))

(defn add-word-to-word-weightlists
  "adds new words backchain style to all previous words in the database."
  [word keyword last-word all-words]

  (doseq [current-word all-words]
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

        (update-word current-word :associations new-associations)))))

(defn build-associations
  ([word word-context all-words] (build-associations word word-context all-words ()))
  ([word word-context all-words current-associations]
     (let [keyword (:keyword word-context)
           last-word (:last-word word-context)
           successor (:successor word-context)]
       (compound-associations
        (concat
         (when (and keyword (not= word keyword))
           (make-weight-list keyword keyword-weight))
         (when (and last-word (not= word last-word))
           (make-weight-list last-word last-word-weight))
         (when (and successor (not= word successor))
           (make-weight-list successor successor-weight))
         (map (fn [item]
                (list item backward-chain-weight)) (remove-all (list word) all-words))
         current-associations)))))

(defn update-or-create-word [word word-context sentence sentence-type name]
  (let [words-store @*words-store*
        all-words @*all-words*]
    (cond
     (and (not (member word (keys words-store))) (not (word-seen? word)))
     (do
       (make-word word {:name (list name)
                        :sentence-type (list sentence-type)
                        :sentence (list sentence)
                        :length-of-sentence (list (count sentence))
                        :predecessors (list (:predecessor word-context))
                        :successors (list (:successor word-context))
                        :keywords (list (:keyword word-context))
                        :word-type (list sentence-type)
                        :positions-in-sentence (list (inc (position word sentence)))
                        :associations (build-associations word word-context all-words)
                        :usage 1
                        :used-before? true})
       (when-not (negative? sentence-type) (push word *all-words*)))

     (and (word-seen? word) (not (:used-before? (lookup-word word))))
     (let [word-data (lookup-word word)]
       (swap-word! word (->
                         word-data
                         (assoc :name (cons name (:name word-data)))
                         (assoc :sentence-type (list sentence-type))
                         (assoc :sentence (list sentence))
                         (assoc :length-of-sentence (list (count sentence)))
                         (assoc :predecessors (list (:predecessor word-context)))
                         (assoc :successors (list (:successor word-context)))
                         (assoc :keywords (list (:keyword word-context)))
                         (assoc :positions-in-sentence (list (inc (position word sentence))))
                         (assoc :word-type (list sentence-type))
                         (assoc :associations (build-associations word word-context all-words))
                         (assoc :usage 1)
                         (assoc :used-before? true)))
       (when-not (negative? sentence-type) (push word *all-words*)))
     :else (let [word-data (lookup-word word)]
             (swap-word! word
                         (->
                          word-data
                          (assoc :name  (cons name (:name word-data)))
                          (assoc :sentence-type (cons sentence-type (:sentence-type word-data)))
                          (assoc :sentence (cons sentence (:sentence word-data)))
                          (assoc :length-of-sentence (cons (count sentence) (:length-of-sentence word-data)))
                          (assoc :predecessors (cons (:predecessor word-context) (:predecessors word-data)))
                          (assoc :successors (cons (:successor word-context) (:successors word-data)))
                          (assoc :keywords (cons (:keyword word-context) (:keywords word-data)))
                          (assoc :positions-in-sentence (cons (inc (position word sentence)) (:positions-in-sentence word-data)))
                          (assoc :word-type(cons sentence-type (:word-type word-data)))
                          (assoc :associations (build-associations word word-context all-words (:associations word-data)))
                          (assoc :usage (inc (:usage word-data)))
                          (assoc :used-before? true)))))))

(defn- words-with-successor-and-predecessor [sentence]
  (let [suc-and-pred
        (map vector sentence
             (partition 2 1 () sentence)
             (reverse (partition 2 1 () (reverse sentence))))]
    (map (fn [[word suc pred]]
           (cond
            (and (= 1 (count suc)) (= 1 (count pred)))
            (list word () ())
            (= 1 (count suc))
            (list word () pred)
            (= 1 (count pred))
            (list word suc ())
            :else (list word suc pred)))
         suc-and-pred)))

(defn make-word-objects [sentence sentence-type sentence-context name]
  (doseq [[word successor predecessor] (words-with-successor-and-predecessor sentence)]
    (let [word-context
          {:keyword      (:keyword sentence-context)
           :last-word    (:last-word sentence-context)
           :successor    (last successor)
           :predecessors (last predecessor)}]
      (update-or-create-word word word-context sentence sentence-type name)
      (when-not (negative? sentence-type)
        (doseq [item sentence]
          (add-word-to-word-weightlists item
                                        (:keyword word-context)
                                        (:last-word word-context) @*all-words*)  )))))

(defn figure-speac
  "this function sets up parsing structure in sentences for future creation of sentences and
   atn use. important to note that word types are figured contextually based on their current usage
   and thus don't require a separate parse entry in their slots."
  [word]
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
  (update-sentence name :parse-it (map (fn [word] (figure-speac word)) sentence)))

(defn define-incipients [sentence sentence-type]
  (if (question? sentence-type)
    (update-incipient *question-incipient-lexicon* (cons (first sentence) (:incipients @*question-incipient-lexicon*)))
    (update-incipient *answer-incipient-lexicon*   (cons (first sentence) (:incipients @*answer-incipient-lexicon*)))))

(defn define-cadences [sentence sentence-type]
  (when-not (negative? sentence-type)
    (if (question? sentence-type)
      (update-cadence *question-cadence-lexicon* (cons (last sentence) (:cadences @*question-cadence-lexicon*)))
      (update-cadence *answer-cadence-lexicon*   (cons (last sentence) (:cadences @*answer-cadence-lexicon*))))))

(defn get-element-from-words
  "getting infer from words. Type can be
   predecessors successors keywords word-type positions-in-sentence associations usage music.
   weight is stored in associations."
  [type]
  (let [words (distinct (keys @*words-store*))]
    (map (fn [x] (list x (type (lookup-word x)))) words)))

(defn new-text []
  (println (reverse (or (get-element-from-words :associations) nil))))

(defn reduce-weight
  "reduces the weight of each entry  in word for all of the words in sentence."
  [word sentence]
  (let [new-associations (punish (:associations (lookup-word word)) sentence)]
    (update-word word :associations new-associations)))

(defn reduce-weighting
  "sentence 1 here is the initiating sentence."
  [sentence-1 sentence-2]
  (doseq [word sentence-1] (cons word (reduce-weight word sentence-2))))

(defn reward
  "rewards the weights with a $ statement from user."
  [associations words]
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
  (first
   (choose-one
    (let [rated-words (sort-by-last words)
          rating (second (first rated-words))]
      (remove (fn [word] (when-not (= (second word) rating) true)) rated-words)))))

(defn current-words-list [current-word cadences]
  (loop [current-word current-word
         collected-words []]
    (if (or (nil? current-word) (member current-word cadences))
      collected-words
      (do
        (push-new current-word *current-words*)
        (let [collected-words-col
              (cons collected-words
                    (let [musical-words (get-music-words (:cadences
                                                          (if (question? type)
                                                            @*question-cadence-lexicon*
                                                            @*answer-cadence-lexicon*)))
                          test (choose-the-highest-rated-word
                                (remove-all-by-ffirst
                                 (concat @*current-words* musical-words)
                                 (get-music-associations (:associations (lookup-word current-word)))))]
                      (or test
                          (choose-one
                           (get-music-words (map (fn [association] (first association) ) (:associations (lookup-word current-word))))))))]
          (recur current-word collected-words-col))))))

(defn- pick-words [current-word]
  (let [word-words (get-word-words (:cadences
                                       (if (question? type)
                                         @*question-cadence-lexicon*
                                         @*answer-cadence-lexicon*)))
        test (choose-the-highest-rated-word
              (remove-all-by-ffirst
               (concat @*current-words* word-words)
               (get-word-associations (:associations (lookup-word current-word)))))]
    (or test
        (choose-one
         (get-word-words (map (fn [association] (first association)) (:associations (lookup-word current-word))))))))

(defn process-events [type sentence]
  (let [choices (compound-associations
                 (apply concat
                        (map (fn [word] (get-music-associations (:associations (lookup-word word)))) sentence)))
        incipients (if (question? type)
                     (remove-all (list @*no*)
                                (get-music-words (:incipients @*answer-incipient-lexicon*)))
                     (get-music-words (:incipients @*question-incipient-lexicon*)))]
    (reset! *current-words* ())
    (if (or (empty? choices) (empty? incipients))
      ()
      (let [current-word
            (let [musical-words (get-music-words (:cadences (if (question? type)
                                                        @*question-cadence-lexicon*
                                                        @*answer-cadence-lexicon*)) )
                  trial (choose-the-highest-rated-word (remove-all-by-ffirst musical-words choices))]
              (or trial (get-music-words (choose-one incipients))))
            cadences (get-music-words (:cadences (other-lexicon-type type)))

            new-sentence (cons current-word (current-words-list current-word cadences))]
        new-sentence))))

(defn build-reply-sentence [current-word cadences]
  (let [new-words (loop [current-word current-word
                         current-words []]
                    (if (or (nil? current-word) (member current-word cadences))
                      current-words
                      (let [next-word (pick-words current-word)]
                        (push-new next-word *current-words*)
                        (recur next-word (cons next-word current-words)))))
        new-sentence (cons current-word new-words)]
  new-sentence))

(defn pick-start-word [type choices incipients]
  (let [trial (choose-the-highest-rated-word
               (remove-all-by-ffirst
                (get-word-words (:cadences (if (question? type)
                                             @*question-cadence-lexicon*
                                             @*answer-cadence-lexicon*)))
                choices))]
  (or trial (choose-one (get-word-words incipients)))))

(defn build-a-response [type sentence]
  (let [choices (compound-associations
                 (mapcat (fn [word] (get-word-associations (:associations (lookup-word word)))) sentence))
        incipients (if (or (question? type))
                     (remove-all (list @*no*) (get-word-words (:incipients @*answer-incipient-lexicon*)))
                     (get-word-words (:incipients @*question-incipient-lexicon*)))]

    (reset! *current-words* ())
    (if (or (empty? choices) (empty? incipients))
      ()
      (let [current-word (pick-start-word type choices incipients)
            cadences (:cadences (other-lexicon-type type))]
        (build-reply-sentence current-word cadences)))))

(defn process-no []
  (reduce-weighting (first (:sentence (lookup-sentence (third (all-sentences)))))
                    (first (:sentence (lookup-sentence (second (all-sentences))))))
  (list negative-type))

(defn process-yes []
  (add-weighting (first (:sentence (lookup-sentence (third (all-sentences)))))
                 (first (:sentence (lookup-sentence (second (all-sentences))))))
  (list positive-type))

(defn reply
  "this function creates sentences by using the various associations in each word in the sentence argument."
  [type sentence]
  (cond
   (recognize-no sentence)
   (process-no)

   (recognize-yes sentence)
   (process-yes)

   (:events (lookup-word (first sentence)))
   (process-events type sentence)

   :else (build-a-response type sentence)))

(defn put-sentence-into-database [sentence]
  (let [sentence-context (establish-keywords sentence)
        sentence-type (get-sentence-type sentence)
        name (make-new-name)]
    (make-sentence-object sentence sentence-type name)
    (make-word-objects sentence sentence-type sentence-context name)

    (parse-sentence sentence name)
    (define-incipients sentence sentence-type)
    (define-cadences sentence sentence-type)

    (new-text)))

(defn fix-end-of-music-sentences
  "attaches the punctuation to the end of the music sentence."
  [sentence]
  (let [object (nth sentence (- (count sentence) 2))
        the-name (implode (drop (- (count sentence) 2) sentence))]
    (make-word the-name {:name (:name (eval object))
                         :timing (:timing (eval object))
                         :destination (:destination (eval object))
                         :events (:events (eval object))
                         :usage (:usage (eval object))})
    (concat (butlast sentence 2) (list the-name))))

(defn make-timings [thing] thing)

(defn event-loop []
  (loop []
    (print "user> ")
    (flush)
    (let [raw-input (read-line)
          user-input (read-string (str "(" raw-input ")"))
          input (if (and (word-seen? (first user-input))
                         (:events (lookup-word (first user-input))))
                  (fix-end-of-music-sentences user-input)
                  user-input)
          _ (put-sentence-into-database input)
          response (reply (get-sentence-type input) input)]
      (when-not (empty? response)
        (let [name (str "sentence-" @*counter*)
              sentence-type (last (explode (last response)))]
          (make-sentence name {:name 'me
                               :sentence-type sentence-type
                               :sentence (list response)
                               :length-of-sentence (count response)
                               :origination 'apprentice})
          (swap! *counter* inc))
        (new-text)
        (when (and (not= (first response) negative-type)
                 (not= (first response) positive-type)
                 (not (nil? (first response)))
                 (:events (lookup-word (first response))))
          (play-events (apply concat
                              (make-timings
                               (map (fn [w] (:events (lookup-word w))) response)))))
        (message response))
      (recur))))

(defn apprentice []
  (reset-all!)
  (event-loop))

(defn remove-cadences
  "removes the cadences from the choices."
  [choices]
  (cond
   (empty? choices)()
   (or (member (second (first choices)) (:cadences @*answer-cadence-lexicon*))
       (member (second (first choices)) (:cadences @*question-cadence-lexicon*)))
   (remove-cadences (rest choices))
   :else (cons (first choices) (remove-cadences (rest choices)))))

(defn pair [list-1 list-2]
  "pairs the two list args."
  ;; (loop for item in list-1
  ;;       collect (list item (first list-2))
  ;;       do (setf list-2 (rest list-2)))
)

(defn collect-parsings
  "pairs the parsings with the words in the sentences."
  [sentences]
  (pair (apply concat (map (fn [sentence] (first (:sentence (eval sentence)))) sentences))
        (apply concat (map (fn [sentence] (:parse-it (eval sentence))) sentences))))

(defn mix
  "pseudo-randomly mixes the list arg."
  [list]
  (if (empty? list) nil
      (let [choice (choose-one list)]
        (cons choice (mix (remove choice list :count 1))))))

(defn get-parse-elements
  "gets the speac parase elements for the speac sentence."
  [parse]
  (if (empty? parse)()
      (cons (first parse)
            (get-parse-elements (remove (fn [p] (= p (first parse))) parse)))))

(defn relate-words
  "relates the words with their speac symbols to words with weightings alone."
  [words associations]
  (cond
   (empty? words) ()
   (let [test (assoc (ffirst words) associations)]
     (if test (cons test (relate-words (rest words) associations)))) '(wat)
     :else (relate-words (rest words) associations)))

(defn get-associations
  "gets the associations for its arg."
  [parsed-words]
  (let [relevant-words
        (compound-associations
         (apply concat
                (map (fn [word] (:associations (lookup-word word)))
                     (first (:sentence (eval (first @*sentences-store*)))))))]
    (relate-words relevant-words parsed-words)))

(defn parse-the-sentence
  "takes parse found in any sentence parse-it slot."
  [parse cadence]
  (concat (let [parse-lists (remove-cadences
                             (map (fn [item] (reverse item)) (get-associations (collect-parsings @*sentences-store*))))]
            (map (fn [element] (second (assoc element (mix parse-lists)))) (butlast parse)))
          (list (choose-one cadence))))

(defn any-greater-than? [n list-of-numbers]
  (cond (empty? list-of-numbers)()
        (> (first list-of-numbers) n) true
        :else (any-greater-than? n (rest list-of-numbers))))

(defn set-to-zero
  ([events] (set-to-zero events (ffirst events)))
  ([events subtract]
      (if (empty? events)()
          (cons (cons (- (ffirst events) subtract)
                      (rest (first events)))
                (set-to-zero (rest events) subtract)))))

(defn get-note-timing [event time]
  (- (+ (first event)(third event)) time))

(defn get-beat-length
  "Used in re-time for setting the new time.
   requires that the first in events be sorted to current time"
  [events]
  (let [time (ffirst events)]
    (first (sort > (map (fn [event] (get-note-timing event time)) events)))))


(defn re-time
  "retimes the events lists to begin one after the other."
  ([event-lists] (re-time event-lists 0))
  ([event-lists current-time ]
      (if (empty? event-lists)()
          (cons (map (fn [event] (cons (+ (first event) current-time) (rest event)) ) (set-to-zero (first event-lists)))
                (re-time (rest event-lists) (+ current-time
                                               (get-beat-length
                                                (first event-lists))))))))

(defn play-sentence
  "plays a sentence of word objects."
  [sentence]
  (play-events (apply concat (re-time (map (fn [word] (:events (lookup-word word)))
                                           (remove (fn [word] (when-not (:events (lookup-word word)) true)) sentence))))))

(defn reveal-the-hidden-events
  "reveals the events in words."
  [events]
  (map (fn [x] (:events (eval x))) events))

(defn tester-for-hidden-events
  "test if sentence is a music sentence."
  [sentence]
  (and (sentence-seen? (first sentence))
       (:events (lookup-sentence (first sentence)))))

(defn return-only-music-sentences
  "takes a *sentences-store*."
  [sentence-objects]
  (let [real-sentences (apply concat (map (fn [x] (:sentence (eval x))) sentence-objects))]
    (remove (fn [sentence] (when-not (tester-for-hidden-events sentence) true)) real-sentences)))

(defn create-a-work
  "takes a *sentences-store*"
  [sentences]
  (reveal-the-hidden-events (apply concat (reverse (return-only-music-sentences sentences)))))
