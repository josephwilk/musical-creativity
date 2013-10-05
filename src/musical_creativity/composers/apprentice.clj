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

(def ^:dynamic *counter* (atom 0))

(def ^:dynamic *sentences-store* (atom {}))
(def ^:dynamic *words-store*     (atom {}))

(def ^:dynamic *yes* (atom ()))
(def ^:dynamic *no*  (atom ()))

(def ^:dynamic *question-lexicon* (atom {:incipients () :cadences ()}))
(def ^:dynamic *answer-lexicon*   (atom {:incipients () :cadences ()}))

(defn reset-all! []
  (reset! *counter* 0)
  (reset! *sentences-store* {})
  (reset! *words-store* {})
  (reset! *yes* ())
  (reset! *no* ())
  (reset! *question-lexicon* {:incipients () :cadences ()})
  (reset! *answer-lexicon*   {:incipients () :cadences ()}))

(def play-sentences "+")

(def question-type "?")
(def fact-type     "!")
(def negative-type "*")
(def positive-type "$")

(defn question? [type] (= type question-type))
(defn fact?     [type] (= type fact-type))
(defn negative? [type] (= type negative-type))
(defn positive? [type] (= type positive-type))

(defn member? [item col] (boolean (some #{item} col)))

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
(defn sentence-seen? [sentence] (contains? @*sentences-store* sentence))

(defn lookup-musical-word [word] (word {}))

(defn all-words [] (keys @*words-store*))
(defn lookup-word [word]  (@*words-store* word))
(defn make-word [id atts]  (reset! *words-store* (assoc @*words-store* id atts)))
(defn swap-word! [word word-map] (reset! *words-store* (assoc @*words-store* word word-map)))
(defn update-word [id field val]
  (reset! *words-store* (assoc-in @*words-store* [id field] val))
  val)

(defn word-seen? [word] (contains? @*words-store* word))

(defn update-incipient [ref word]
  (reset! ref (assoc @ref :incipients (cons word (:incipients @ref))))
  (:incipients @ref))

(defn cadences-for [type]
  (:cadences (if (question? type) @*question-lexicon* @*answer-lexicon*)))

(defn update-cadence [ref word]
  (reset! ref (assoc @ref :cadences (cons word (:cadences @ref))))
  (:cadences @ref))

(defn explode [thing] (map str (vec (str thing))))

(defn sort-by-last [thing] (sort (fn [x y] (> (last x) (last y))) thing))

(defn message [thing] (when (seq thing) (println "Alice> " (string/join (rest (butlast (str thing)))))))

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

(defn round-it [n] (float (/ (math/round (* n 100)) 100)))

(defn other-lexicon-type
  "returns words from the opposite of its arg sentence type."
  [type]
  (if (question? type) @*answer-lexicon* @*question-lexicon*))

(defn punish
  "punishes the weights with a * statement from user."
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

(defn- find-word [sentence word-type previous-word]
  (when-let [candidate-word (first sentence)]
    (cond
     (or (member? candidate-word (list fact-type question-type positive-type)))
     ()
     (or (member? word-type (explode candidate-word))
         (member? previous-word (list candidate-word))
         (when (empty? (rest sentence))
           (member? previous-word (list (butlast (explode candidate-word))))))
     (let [test (butlast (explode candidate-word))
           new-word (if (= (last test) word-type)
                      (butlast test)
                      (list candidate-word))]
       new-word)
     :else (find-word (rest sentence) word-type previous-word))))

(defn contains-no? [sentence]
  (when-let [no-word (find-word sentence negative-type @*no*)]
    (reset! *no* no-word)))

(defn contains-yes? [sentence]
  (when-let [yes-word (find-word sentence positive-type @*yes*)]
    (reset! *yes* yes-word)))

(defn establish-keywords
  "establishes all of the principal keywords."
  [sentence]
  (let [no-test  (contains-no? sentence)
        yes-test (contains-yes? sentence)
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
  (map (fn [[k v]]
         (list k (reduce (fn [sum [_ weight]] (+ sum weight)) 0 v)))
       (group-by (fn [[name _]] name) associations)))

(defn store-sentence
  "associations are of four types:
  1. keyword found: weight keyword-weight
  2. last word found: weight last-word-weight
  3. next word found in successor: successor-weight
  4. all remaining words found in all-words, weight being backward-chain-weight
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
  (cond
   (not (word-seen? word))
   (when-not (negative? sentence-type)
     (make-word word {:name (list name)
                      :sentence-type (list sentence-type)
                      :sentence (list sentence)
                      :length-of-sentence (list (count sentence))
                      :predecessors (list (:predecessor word-context))
                      :successors (list (:successor word-context))
                      :keywords (list (:keyword word-context))
                      :word-type (list sentence-type)
                      :positions-in-sentence (list (inc (position word sentence)))
                      :associations (build-associations word word-context (all-words))
                      :usage 1
                      :used-before? true}))

   (and (word-seen? word) (not (:used-before? (lookup-word word))))
   (when-not (negative? sentence-type)
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
                         (assoc :associations (build-associations word word-context (all-words)))
                         (assoc :usage 1)
                         (assoc :used-before? true)))))
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
                        (assoc :word-type (cons sentence-type (:word-type word-data)))
                        (assoc :associations (build-associations word word-context (all-words) (:associations word-data)))
                        (assoc :usage (inc (:usage word-data)))
                        (assoc :used-before? true))))))

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

(defn store-words [sentence sentence-type sentence-context name]
  (doseq [[word successor predecessor] (words-with-successor-and-predecessor sentence)]
    (let [word-context
          {:keyword      (:keyword sentence-context)
           :last-word    (:last-word sentence-context)
           :successor    (last successor)
           :predecessor (last predecessor)}]
      (update-or-create-word word word-context sentence sentence-type name)
      (when-not (negative? sentence-type)
        (doseq [item sentence]
          (add-word-to-word-weightlists item
                                        (:keyword word-context)
                                        (:last-word word-context) (all-words)))))))

(defn figure-speac
  "this function sets up parsing structure in sentences for future creation of sentences and
   atn use. important to note that word types are figured contextually based on their current usage
   and thus don't require a separate parse entry in their slots."
  [word]
  (let [count-for-word (frequency word (all-words))
        total-words (count (all-words))]
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
    (update-incipient *question-lexicon* (first sentence))
    (update-incipient *answer-lexicon*   (first sentence))))

(defn define-cadences [sentence sentence-type]
  (when-not (negative? sentence-type)
    (if (question? sentence-type)
      (update-cadence *question-lexicon* (last sentence))
      (update-cadence *answer-lexicon*   (last sentence)))))

(defn all-associations []
  (map (fn [word] {word (:associations (lookup-word word))}) (distinct (all-words))))

(defn print-associations [] (println (reverse (all-associations))))

(defn reduce-weight
  "reduces the weight of each entry in word for all of the words in sentence."
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
   (empty? associations) ()
   (:events (lookup-musical-word (ffirst associations)))
   (cons (first associations)
         (get-music-associations (rest associations)))
   :else (get-music-associations (rest associations))))

(defn get-music-words [words]
  (cond
   (empty? words) ()
   (:events (lookup-musical-word (first words)))
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
   (empty? associations) ()
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

(defn musical-words [current-word current-words type]
  (let [words (get-music-words (cadences-for type))
        test (choose-the-highest-rated-word
              (remove-all-by-ffirst
               (concat current-words words)
               (get-music-associations (:associations (lookup-word current-word)))))]
    (or test
        (choose-one
         (get-music-words (map (fn [association] (first association) ) (:associations (lookup-word current-word))))))))

(defn current-words-list [current-word cadences type]
  (loop [current-word current-word
         current-words []]
    (if (or (nil? current-word) (member? current-word cadences))
      current-words
      (let [musical-words (musical-words current-word current-words type)]
        (recur current-word (cons current-words musical-words))))))

(defn- pick-words [current-word current-words]
  (let [word-words (get-word-words (cadences-for type))
        test (choose-the-highest-rated-word
              (remove-all-by-ffirst
               (concat current-words word-words)
               (get-word-associations (:associations (lookup-word current-word)))))]
    (or test
        (choose-one
         (get-word-words (map (fn [association] (first association)) (:associations (lookup-word current-word))))))))

(defn build-musical-reply [type sentence]
  (let [choices (compound-associations
                 (apply concat
                        (map (fn [word] (get-music-associations (:associations (lookup-word word)))) sentence)))
        incipients (if (question? type)
                     (remove-all (list @*no*)
                                (get-music-words (:incipients @*answer-lexicon*)))
                     (get-music-words (:incipients @*question-lexicon*)))]
    (if (or (empty? choices) (empty? incipients))
      ()
      (let [current-word
            (let [musical-words (get-music-words (cadences-for type))
                  trial (choose-the-highest-rated-word (remove-all-by-ffirst musical-words choices))]
              (or trial (get-music-words (choose-one incipients))))
            cadences (get-music-words (:cadences (other-lexicon-type type)))

            new-sentence (cons current-word (current-words-list current-word cadences type))]
        new-sentence))))

(defn build-reply-sentence [current-word cadences]
  (let [new-words (loop [current-word current-word
                         current-words []]
                    (if (or (nil? current-word) (member? current-word cadences))
                      current-words
                      (let [next-word (pick-words current-word current-words)]
                        (recur next-word (cons next-word current-words)))))
        new-sentence (cons current-word new-words)]
  new-sentence))

(defn pick-start-word [type choices incipients]
  (let [trial (choose-the-highest-rated-word
               (remove-all-by-ffirst
                (get-word-words (cadences-for type))
                choices))]
  (or trial (choose-one (get-word-words incipients)))))

(defn build-a-response [type sentence]
  (let [choices (compound-associations
                 (mapcat (fn [word] (get-word-associations (:associations (lookup-word word)))) sentence))
        incipients (if (or (question? type))
                     (remove-all (list @*no*) (get-word-words (:incipients @*answer-lexicon*)))
                     (get-word-words (:incipients @*question-lexicon*)))]

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

(defn musical-statement? [sentence] (:events (lookup-musical-word (first sentence))))

(defn reply [type sentence]
  (cond
   (contains-no? sentence)
   (process-no)

   (contains-yes? sentence)
   (process-yes)

   (musical-statement? sentence)
   (build-musical-reply type sentence)

   :else (build-a-response type sentence)))

(defn put-sentence-into-database [sentence]
  (let [sentence-context (establish-keywords sentence)
        sentence-type (get-sentence-type sentence)
        name (make-new-name)]
    (store-sentence sentence sentence-type name)
    (store-words sentence sentence-type sentence-context name)

    (parse-sentence sentence name)
    (define-incipients sentence sentence-type)
    (define-cadences sentence sentence-type)

    (print-associations)))

(defn find-music [key] {})

(defn fix-end-of-music-sentences
  "attaches the punctuation to the end of the music sentence."
  [sentence]
  (let [object (nth sentence (- (count sentence) 2))
        the-name (drop (- (count sentence) 2) sentence)]
    (make-word the-name {:name (:name (find-music object))
                         :timing (:timing (find-music object))
                         :destination (:destination (find-music object))
                         :events (:events (find-music object))
                         :usage (:usage (find-music object))})
    (concat (drop-last 2 sentence) (list the-name))))

(defn make-timings [thing] thing)
(defn play-events [events] (println :play events))

(defn- user-input []
  (let [raw-input (read-line)
        clj-input (read-string (str "(" raw-input ")"))
        clj-input (if (:events (find-music (first clj-input)))
                    (fix-end-of-music-sentences clj-input)
                    clj-input)]

    (println :music? (find-music (first clj-input)))

    clj-input))

(defn musical-response? [response]
  (and (not= (first response) negative-type)
       (not= (first response) positive-type)
       (not (nil? (first response)))
       (:events (lookup-word (first response)))))

(defn- apprentice-reply [input]
  (let [input-sentence-type (get-sentence-type input)
        _ (put-sentence-into-database input)
        response (reply input-sentence-type input)]
    (when-not (empty? response)
      (let [name (str "sentence-" @*counter*)
            sentence-type (last (explode (last response)))]
        (make-sentence name {:name 'me
                             :sentence-type sentence-type
                             :sentence (list response)
                             :length-of-sentence (count response)
                             :origination 'apprentice})
        (swap! *counter* inc))
      (print-associations)
      (when (musical-response? response)
        (play-events (apply concat
                            (make-timings
                             (map (fn [w] (:events (lookup-word w))) response))))))

    response))

(defn event-loop
  ([player-fn]
      (loop []
        (print "user> ")
        (flush)
        (let [input (user-input)]
          (cond
           (= play-sentences (str (first input)))
           (player-fn (flatten
                       (map #(:sentence %)
                            (filter #(= 'apprentice (:origination %))
                                    (vals @*sentences-store*)))))
           :else (let [reply (apprentice-reply input)]
                   (message reply)
                   (player-fn [reply]))))
        (recur))))

(defn apprentice
  ([] (apprentice (fn [x] x)))
  ([player-fn]
     (reset-all!)
     (event-loop player-fn)))