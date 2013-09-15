(ns musical-creativity.composers.apprentice
  (:require
   [clojure.math.numeric-tower :as math]))

(def *hori-cons* 50)

(def *sentences* (atom ()))
(def *no-sentences* (atom ()))
(def *yes-sentences* (atom ()))
(def *yes* (atom ()))
(def *no* (atom ()))
(def *keyword* (atom ()))
(def *keywords* (atom ()))
(def *counter* (atom nil))

(def *keyword-weight* (atom nil))
(def *last-word-weight* (atom nil))
(def *successor-weight* (atom nil))
(def *backward-chain-weight* (atom nil))

(def *predecessor* (atom nil))
(def *successor* (atom nil))
(def *last-word* (atom nil))
(def *last-words* (atom ()))
(def *words* (atom ()))
(def *all-words* (atom ()))
(def *input-work* (atom ()))
(def *weight-list* (atom ()))
(def *response* (atom ()))
(def *weight-divisor* (atom nil))
(def *current-words* (atom ()))

(defn make-instance [thing])

(def *question-incipient-lexicon* (make-instance 'incipient-lexicon))
(def *answer-incipient-lexicon*   (make-instance 'incipient-lexicon))
(def *question-cadence-lexicon*   (make-instance 'cadence-lexicon))
(def *answer-cadence-lexicon*     (make-instance 'cadence-lexicon))

(def *dialog-text* (atom ""))

(declare find-no find-yes)

(defn pushnew [x y])
(defn member [x y])
(defn explode [thing])
(defn implode [thing])
(defn my-last [thing])
(defn position [i thing])
(defn my-sort [fun thing])
(defn incf [thing])
(defn boundp [thing])
(defn my-remove [thing])
(defn push [thing])
(defn setf [thing value])
(defn wierd-count [item list])
(defn remove-duplicates [list])
(defn funcall [thing])
(defn set-table-sequence [what])
(defn third [])
(defn listp [thing])
(defn choose-one [t] )
(defn sortcdr [thing])

(defn punish [associations words]
  "Punishes the weights with a * statement from user."
  (if (empty? words) associations
      (let [test (assoc (first words) associations)]
        (if test (punish (cons (list (first test) (round-it (/ (second test) *weight-divisor*)))
                               (remove test associations))
                         (rest words))
            (punish associations (rest words))))))

(defn get-sentence-type [sentence]
  "returns the sentence type of question or statement."
  (my-last (explode (my-last sentence))))

(defn make-new-name []
  "returns a new sentence name."
  (implode (list 'sentence '- *counter*)))

(defn get-keyword [sentence]
  "gets the keyword from its arg."
  (let [test (map (fn [word] (count (explode word))) sentence)]
    (nth (position (first (my-sort '> test)) test) sentence)))

(defn round-it [n]
  "simple utility to limit decimal places."
  (float (/ (math/round (* n 100)) 100)))

(defn recognize-no [sentence]
  "this function finds the first ocurance of the no word (followed by a *) and
   places it in the *no-sentences* listing.
   calling (recognize-no (too bad you cant answer!))
   recognize-no returned nil"
  (if (find-no sentence)
    (pushnew (first *sentences*) *no-sentences*)
    ()))

(defn recognize-yes [sentence]
  "this function finds the first ocurance of the yes word (followed by a ^) and
   places it in the *yes-sentences* listing."
  (if (find-yes sentence)
    (pushnew (first *sentences*) *yes-sentences*)
    ()))

(defn find-yes [sentence]
  "tests the sentence to see if it contains the yes word."
  (cond
   (or (member (first sentence) '(! ? \^)) (empty? sentence)) ()
   (or (member '\^ (explode (first sentence)) :test 'equal)
       (member *yes* (list (first sentence)) :test 'equal)
       (if (empty? (rest sentence))
         (member *yes* (list (implode (butlast (explode (first sentence))))) :test 'equal)))
   (let [test (butlast (explode (first sentence)))]
     (if (= (my-last test) '*)
       (reset! *yes* (butlast (implode test)))
       (reset! *yes* (implode (list (first sentence))))))
   :else (find-yes (rest sentence))))

(defn find-no [sentence]
  "tests the sentence to see if it contains the no word."
  (cond
   (or (member (first sentence) '(! ? \^))(empty? sentence))()
   (or (member '* (explode (first sentence)) :test 'equal)
       (member *no* (list (first sentence)) :test 'equal)
       (if (empty? (rest sentence))
         (member *no* (list (implode (butlast (explode (first sentence))))) :test 'equal)))
   (let [test (butlast (explode (first sentence)))]
     (if (= (my-last test) '*)
       (reset! *no* (butlast (implode test)))
       (reset! *no* (implode (list (first sentence))))))
   :else (find-no (rest sentence))))

(defn establish-keywords [sentence]
  "establishes all of the principal keywords."
  (let [test (recognize-no sentence)
        yes-test (recognize-yes sentence)]
    ;(reset! *rs* (make-random-state t))
    (reset! *predecessor* ())
    (reset! *successor* (second sentence))
    (reset! *last-word* (my-last sentence))
    (when-not (or yes-test test)
      (reset! *keyword* (get-keyword sentence)))
    (when-not (or yes-test test)
      (pushnew *keyword* *keywords*))
    (when-not (or yes-test test)
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

(defn compound-associations [associations]
  "aggregates all of the same word weightings into single entries."
  (cond
   (empty? associations) ()
   (assoc (ffirst associations)(rest associations))
   (let [compound-object (list (ffirst associations)
                               (round-it (+ (second (first associations))
                                            (second (assoc (ffirst associations)(rest associations))))))]
     (compound-associations
      (cons compound-object (remove-object-twice (ffirst associations) associations))))
   :else (cons (first associations)
               (compound-associations (rest associations)))))

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
  (set name
       (make-instance 'sentence
         :name (list name)
         :sentence-type (list sentence-type)
         :sentence (list sentence)
         :length-of-sentence (list (count sentence))
         :parse-it ()
         :origination 'user))
  (pushnew name *sentences*)
  (incf *counter*))

(defn make-weight-list [name weight]
  "a simple cover for double listing."
  (list (list name weight)))

(defn add-word-to-word-weightlists [word]
  "adds new words backchain style to all previous words in the database."
  (map (fn [item]
         (when-not (=  item word)
           (setf (:associations (eval item))
                 (compound-associations
                  (concat (:associations (eval item))
                          (list
                           (cond
                            (=  word *keyword*)
                            (list word (round-it (/ *keyword-weight* 2)))
                            (=  word *last-word*)
                            (list word (round-it (/ *last-word-weight* 2)))
                            :else (list word *backward-chain-weight*))))))))

       @*all-words*))

(defn make-word-objects [sentence sentence-type name]
  "makes the words objects for colaborator."
  (map (fn [word]
        do (cond ((and (not (member word *words*))(not (boundp word))) ;;;in other words not previously used or a music object
                  (do
                    (set word
                         (make-instance 'word
                           :name (list name)
                           :sentence-type (list sentence-type)
                           :sentence (list sentence)
                           :length-of-sentence (list (count sentence))
                           :predecessors (list *predecessor*)
                           :successors (list *successor*)
                           :keywords (list *keyword*)
                           :word-type (list sentence-type)
                           :positions-in-sentence (list (inc (position word sentence)))
                           :associations
                           (compound-associations
                            (concat (if (and *keyword* (not (=  word *keyword*)))
                                      (make-weight-list *keyword* *keyword-weight*))
                                    (if (and *last-word* (not (=  word *last-word*)))
                                      (make-weight-list *last-word* *last-word-weight*))
                                    (if (and *successor* (not (=  word *successor*)))
                                      (make-weight-list *successor* *successor-weight*))
                                    (map (fn [item]
                                           (list item *backward-chain-weight*)) (my-remove (list word) *all-words*))))
                           :usage 1
                           :used-before? true))
                    (if (not (=  sentence-type '*))
                      (push word *all-words*))
                    (reset! *input-work* (rest *input-work*))))
                 ((and (boundp word)(not (used-before? (eval word)))) ;music word but not actually used yet!!!
                  (do
                    (setf (name (eval word))(cons name (name (eval word))))
                    (setf (sentence-type (eval word)) (list sentence-type))
                    (setf (sentence (eval word)) (list sentence))
                    (setf (:length-of-sentence (eval word)) (list (count sentence)))
                    (setf (:predecessors (eval word)) (list *predecessor*))
                    (setf (:successors (eval word)) (list *successor*))
                    (setf (:keywords (eval word)) (list *keyword*))
                    (setf (:positions-in-sentence (eval word)) (list (inc (position word sentence))))
                    (setf (:word-type (eval word)) (list sentence-type))
                    (setf (:associations (eval word))
                          (compound-associations
                           (concat (if (and *keyword* (not (=  word *keyword*)))
                                     (make-weight-list *keyword* *keyword-weight*))
                                   (if (and *last-word* (not (=  word *last-word*)))
                                     (make-weight-list *last-word* *last-word-weight*))
                                   (if (and *successor* (not (=  word *successor*)))
                                     (make-weight-list *successor* *successor-weight*))
                                   (map (fn [item]
                                          (list item *backward-chain-weight*)) (my-remove (list word) *all-words*)))))
                    (setf (:usage (eval word)) 1
                          )
                    (setf (:used-before? (eval word)) true)
                    (if (not (=  sentence-type '*))
                      (push word *all-words*))
                    (reset! *input-work* (rest *input-work*))))
                 :else (do
                         (setf (name (eval word))(cons name (name (eval word))))
                         (setf (:sentence-type (eval word))(cons sentence-type (sentence-type (eval word))))
                         (setf (:sentence (eval word))(cons sentence (sentence (eval word))))
                         (setf (:length-of-sentence (eval word))
                               (cons (count sentence) (:length-of-sentence (eval word))))
                         (setf (:predecessors (eval word))(cons *predecessor* (:predecessors (eval word))))
                         (setf (:successors (eval word))(cons *successor* (:successors (eval word))))
                         (setf (:keywords (eval word))(cons *keyword* (:keywords (eval word))))
                         (setf (:positions-in-sentence (eval word))(cons (inc (position word sentence)) (:positions-in-sentence (eval word))))
                         (setf (:word-type (eval word))(cons sentence-type (:word-type (eval word))))
                         (setf (:associations (eval word))
                               (compound-associations
                                (concat (if (and *keyword* (not (=  word *keyword*)))
                                          (make-weight-list *keyword* *keyword-weight*))
                                        (if (and *last-word* (not (=  word *last-word*)))
                                          (make-weight-list *last-word* *last-word-weight*))
                                        (if (and *successor* (not (=  word *successor*)))
                                          (make-weight-list *successor* *successor-weight*))
                                        (map (fn [item]
                                               (list item *backward-chain-weight*)) (my-remove (list word) *all-words*))
                                        (:associations (eval word)))))
                         (setf (:usage (eval word))
                               (inc (:usage (eval word))))
                         (setf (:used-before? (eval word)) true)))
        (setf *predecessor* word)
        (setf *successor* (nth (+ (position word sentence) 2) sentence))
        (pushnew word *words*)
        (if (not (=  sentence-type '*))
          (doall (map (fn [item] (add-word-to-word-weightlists item) sentence)))))))

(defn figure-speac [word]
  "this function sets up parsing structure in sentences for future creation of sentences and
   atn use. important to note that word types are figured contextually based on their current usage
   and thus don't require a separate parse entry in their slots."
  (let [count (wierd-count word *words*)
        length (count *words*)]
    (cond
     (< count (/ length 10))
     'c
     (< count (/ length 8))
     'e
     (< count (/ length 6))
     'a
     (< count (/ length 4))
     'p
     :else 's)))

(defn parse-sentence [sentence name]
  "parses the sentence fully."
  (setf (:parse-it (eval name))
        (map (fn [word]
               (figure-speac word) ) sentence)))

(defn define-incipients [sentence sentence-type]
  "defines the incipients for the sentence."
  (if (=  sentence-type '?)
    (setf (:incipients (eval *question-incipient-lexicon*))
          (cons (first sentence) (:incipients (eval *question-incipient-lexicon*))))
    (setf (:incipients (eval *answer-incipient-lexicon*))
          (cons (first sentence) (:incipients (eval *answer-incipient-lexicon*))))))

(defn define-cadences [sentence sentence-type]
  "finds and returns its arg's cadences."
  (when-not (=  sentence-type '*)
    (if (=  sentence-type '?)
      (setf (:cadences (eval *question-cadence-lexicon*))
            (cons (my-last sentence) (:cadences (eval *question-cadence-lexicon*))))
      (setf (:cadences (eval *answer-cadence-lexicon*))
            (cons (my-last sentence) (:cadences (eval *answer-cadence-lexicon*)))))))

(defn get-element-from-words [type]
  "this is a test function for getting infor from words. type can be
       predecessors successors keywords word-type positions-in-sentence associations usage music.
       weight is stored in associations."
  (let [words (remove-duplicates *words*)]
    (map (fn [x](list x (funcall type (eval x)))) words)))

(defn new-text []
  "gets the elements from words and sets the table sequence thusly."
  (reset! *weight-list*
          (let [test (get-element-from-words 'associations)]
          (if test test nil)))
  (set-table-sequence *dialog-text* (reverse *weight-list*)))

(defn reduce-weight [word sentence]
  "reduces the weight of each entry  in word for all of the words in sentence."
  (setf (:associations (eval word))
        (punish (:associations (eval word)) sentence)))

(defn reduce-weighting [sentence-1 sentence-2]
  "sentence 1 here is the initiating sentence."
  (map (fn [word]
         (cons word (reduce-weight word sentence-2))) sentence-1))

(defn reward [associations words]
  "rewards the weights with a * statement from user."
  (if (empty? words) associations
      (let [test (assoc (first words) associations)]
        (if test (reward (cons (list (first test) (round-it (* (second test) *weight-divisor*)))
                               (remove test associations))
                         (rest words))
            (reward associations (rest words))))))

(defn add-weight [word sentence]
  "increases the weight of each entry  in word for all of the words in sentence."
  (setf (:associations (eval word))
        (reward (:associations (eval word)) sentence)))

(defn add-weighting [sentence-1 sentence-2]
  "sentence 1 here is the initiating sentence."
  (map (fn [word]
         (cons word (add-weight word sentence-2))) sentence-1))

(defn get-music-associations [associations]
  (cond
   (empty? associations)()
   (:events (eval (ffirst associations)))
   (cons (first associations)
         (get-music-associations (rest associations)))
   :else (get-music-associations (rest associations))))

(defn get-music-words [words]
  (cond
   (empty? words)()
   (:events (eval (first words)))
   (cons (first words)
         (get-music-words (rest words)))
   :else (get-music-words (rest words))))

(defn get-word-words [words]
  (cond
   (empty? words)()
   (:events (eval (first (if (listp (first words))
                           (list (ffirst words))
                           (list (first words))))))
   (get-word-words (rest words))
   :else (cons (first words)
               (get-word-words (rest words)))))

(defn get-word-associations [associations]
  (cond
   (empty? associations)()
   (:events (eval (ffirst associations)))
   (get-word-associations (rest associations))
   :else (cons (first associations)
           (get-word-associations (rest associations)))))

(defn choose-the-highest-rated-word [words]
  "chooses the highest choice from among ties for the honor."
  (first
   (choose-one (let [rated-words (sortcdr '> words)
                     rating (second (first rated-words))]
                 (loop for word in rated-words
                       if (=  (second word) rating)
                       collect word)))))

(defn reply [type sentence]
  "this function creates sentences by using the various associations in each
       word in the sentence argument."
  (cond
   (recognize-no sentence)
   (do (reduce-weighting (first (sentence (eval (third *sentences*))))
                            (first (sentence (eval (second *sentences*)))))
       (list '*))
   (recognize-yes sentence)
   (do (add-weighting (first (sentence (eval (third *sentences*))))
                      (first (sentence (eval (second *sentences*)))))
       (list '\^))

   (:events (eval (first sentence)))
   (let [choices (compound-associations                              ;;;this is a pro-rated list of all of the associations of the sentence argument
                  (apply concat
                         (map (fn [word] (get-music-associations (:associations (eval word)))) sentence)))
         incipients (if (=  type '?)
                      (my-remove (list (eval *no*))
                                 (get-music-words (:incipients *answer-incipient-lexicon*)))
                      (get-music-words (:incipients *question-incipient-lexicon*)))]
     (reset! *current-words* ())
     (if (or (nil? choices)(nil? incipients))
       ()
       (let [current-word                                            ;;;here's where we get a current word - the highest rated word in choices
             (let [trial (choose-the-highest-rated-word
                          (remove-them
                           (get-music-words (cadences (if (=  type '?)
                                                        *question-cadence-lexicon*
                                                        *answer-cadence-lexicon*)) )
                           choices))]
               (if trial trial (get-music-words (choose-one incipients))))            ;;;here is where we resort to incipients if necessary
             cadences (get-music-words (cadences (eval (other-lexicon-type type))))]  ;;;this variable will indicate when we must stop!
         (let [new-sentence                                          ;;;here is where the new sentences is stored
               (cons current-word                                    ;;;current word changes until the "or"
                     (loop until
                       (or (nil? current-word)(member current-word cadences))
                       do (setq test-word current-word)
                       do (setq tester-word (member current-word cadences))
                       do (pushnew current-word *current-words*) ;;;these must be subtracted from options to avoid repeats
                       collect (setf current-word
                                     (let ((test
                                            (choose-the-highest-rated-word
                                             (remove-them
                                              (concat *current-words*
                                                      (get-music-words (cadences
                                                                        (if (=  type '?) *question-cadence-lexicon* *answer-cadence-lexicon*))))
                                              (get-music-associations (associations (eval current-word)))))))
                                       (if test test
                                           (choose-the-one
                                            (get-music-words (loop for association in (associations (eval current-word))
                                                                   collect (first association)))))))))]
           new-sentence))))
   :else (let ((choices (compound-associations                              ;;;this is a pro-rated list of all of the associations of the sentence argument
                         (apply #'append
                                (loop for word in sentence
                                      collect (get-word-associations (associations (eval word)))))))
               (incipients (if (=  type '?)     ;;;this is a just in case choices is nil listing of alternatives sentence incipients
                             (my-remove (list (eval *no*))
                                        (get-word-words (incipients *answer-incipient-lexicon*)))
                             (get-word-words (incipients *question-incipient-lexicon*)))))
           (setq *current-words* ())
           (if (or (nil? choices)(nil? incipients))
             ()
             (let ((current-word                                            ;;;here's where we get a current word - the highest rated word in choices
                    (let ((trial (choose-the-highest-rated-word
                                  (remove-them
                                   (get-word-words (cadences (if (=  type '?)
                                                               *question-cadence-lexicon*
                                                               *answer-cadence-lexicon*)))
                                   choices))))
                      (if trial trial (choose-one (get-word-words incipients)))))           ;;;here is where we resort to incipients if necessary
                   (cadences (cadences (eval (other-lexicon-type type)))))  ;;;this variable will indicate when we must stop!
               (let ((new-sentence                                          ;;;here is where the new sentences is stored
                      (cons current-word                                    ;;;current word changes until the "or"
                            (loop until
                              (or (nil? current-word)(member current-word cadences))
                              do (pushnew current-word *current-words*) ;;;these must be subtracted from options to avoid repeats
                              collect (setf current-word
                                            (let ((test
                                                   (choose-the-highest-rated-word
                                                    (remove-them
                                                     (concat *current-words*
                                                             (get-word-words (cadences
                                                                              (if (=  type '?) *question-cadence-lexicon* *answer-cadence-lexicon*))))
                                                     (get-word-associations (associations (eval current-word)))))))
                                              (if test test
                                                  (choose-the-one
                                                   (get-word-words (loop for association in (associations (eval current-word))
                                                                         collect (first association)))))))))))
                 new-sentence))))))

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
    (display *response*)))

(defn choose-the-one [stuff]
  "simply chooses one object pseudo-randomly from its arg."
  (choose-one stuff))

(defn remove-them [list things]
  "removes its first arg from its second arg."
  (if (empty? list) things
      (remove-them (rest list)(remove-it (first list) things))))

(defn remove-it [thing things]
  "removes its first arg from its second arg."
  (cond ((empty? things)())
        ((=  thing (ffirst things))
         (remove-it thing (rest things)))
        (t (cons (first things)
                 (remove-it thing (rest things))))))

(defn display [response]
  "simple making of list into string."
  (make-list-into-string response))

(defn other-lexicon-type [type]
  "returns words from the opposite of its arg sentence type."
  (if (=  type '?) *answer-cadence-lexicon* *question-cadence-lexicon*))

(defn apprentice []
  "this function runs the program from the menu."
  (if *initiate* (setq *all-words* nil))
  (progn (setq *no* ())(setq *yes* ())
         (loop until (progn (modal-dialog (make-instance 'music-window))
                      *close*)
               do (setq *input* *name-list*)
               do (if (and (boundp (first *name-list*))(events (eval (first *name-list*))))
                    (progn (setq *name-list* (fix-end-of-music-sentences *name-list*))
                           (setq *input* *name-list*)))
               do (let* ((trial (put-sentence-into-database *input*)))                  ;;;this is where reply is!!
                    (if (not (empty? (read-from-string trial)))
                      (progn (let ((name (implode (list 'sentence- *counter*)))
                                   (sentence-type (my-last (explode (my-last *response*)))))
                               (set name (make-instance 'sentence
                                           :name 'me
                                           :sentence-type sentence-type
                                           :sentence (list *response*)
                                           :length-of-sentence (count *response*)
                                           :origination 'apprentice))
                               (pushnew name *sentences*)
                               (incf *counter*)))))
               do (if (not (empty? *response*))
                    (progn (new-text)
                           (if (and (not (=  (first *response*) '*))(not (= (first *response*) '\^))
                                    (not (empty? (first *response*)))
                                    (events (eval (first *response*))))
                             (setq *process* (process-run-function "play" 'play-events (apply #'append (make-timings (mapcar #'(lambda (x)(events (eval x))) *response*))))))
                           (message-dialog (make-list-into-string *response*) :title "apprentice" :position #@(80 160)))
                    (progn (new-text)
                           (message-dialog " ------- " :title "apprentice" :position #@(80 160))))
               do (gc))))

(defn parse-the-sentence [parse cadence]
  "the argument here is the parse found in any sentence parse-it slot."
  (concat (let ((parse-lists (remove-cadences (loop for item in (get-associations (collect-parsings *sentences*)) collect (reverse item)))))
            (loop for element in (butlast parse)
                  collect (second (assoc element (mix parse-lists)))))
          (list (choose-one cadence))))

(defn collect-parsings [sentences]
  "pairs the parsings with the words in the sentences."
  (pair (apply #'append (loop for sentence in sentences
                              collect (first (sentence (eval sentence)))))
        (apply #'append (loop for sentence in sentences
                              collect (parse-it (eval sentence))))))

(defn pair [list-1 list-2]
  "pairs the two list args."
  (loop for item in list-1
        collect (list item (first list-2))
        do (setf list-2 (rest list-2))))

(defn mix [list]
  "pseudo-randomly mixes the list arg."
  (if (empty? list) nil
      (let ((choice (choose-one list)))
        (cons choice (mix (remove choice list :count 1))))))

(defn remove-cadences [choices]
  "removes the cadences from the choices."
  (cond ((empty? choices)())
        ((or (member (second (first choices)) (cadences *answer-cadence-lexicon*))
             (member (second (first choices)) (cadences *question-cadence-lexicon*)))
         (remove-cadences (rest choices)))
        (t (cons (first choices)(remove-cadences (rest choices))))))

(defn get-associations [parsed-words]
  "gets the associations for its arg."
  (let ((relevant-words
         (compound-associations
          (apply #'append
                 (loop for word in (first (sentence (eval (first *sentences*))))
                       collect (associations (eval word)))))))
    (relate-words relevant-words parsed-words)))

(defn relate-words [words associations]
  "relates the words with their speac symbols to words with weightings alone."
  (cond ((empty? words)())
        ((let ((test (assoc (ffirst words) associations)))
           (if test (cons test (relate-words (rest words) associations)))))
        (t (relate-words (rest words) associations))))

(defn get-parse-elements [parse]
  "gets the speac parase elements for the speac sentence."
  (if (empty? parse)()
      (cons (first parse)
            (get-parse-elements (remove (first parse) parse :test #'equal)))))

(require :scrollers)

(defclass scrolling-window (window) ((my-scroller :accessor my-scroller)))

(defmethod initialize-instance ((self scrolling-window) &rest rest &key
                                    (scroller-class 'scroller)
                                    scroll-bar-class h-scroll-class v-scroll-class
                                    track-thumb-p field-size)
  (declare (dynamic-extent rest))
  ; we use the values of these keywords by modifying the rest parameter
  (declare (ignore scroll-bar-class h-scroll-class v-scroll-class
                   track-thumb-p field-size))
  (call-next-method)
  ; leave, in rest, only the four keywords we want to pass to the
  ; make-instance for scroller-class. this allows them to default
  ; as desired by scroll-class.
  (let* ((handle (cons nil rest)))
    (declare (dynamic-extent handle) (type cons handle))
    (do ((tail handle))
        ((empty? (rest tail)) (setq rest (rest handle)))
      (declare (type cons tail))
      (if (memq (cadr tail)
                '(:scroll-bar-class :h-scroll-class :v-scroll-class
                  :track-thumb-p :field-size))
        (setq tail (cddr tail))
        (setf (rest tail) (rest (cddr tail))))))
  (setf (my-scroller self) (apply #'make-instance
                                  scroller-class
                                  :view-container self
                                  :view-size (subtract-points
                                              (view-size self) #@(15 15))
                                  :view-position #@(0 0)
                                  :draw-scroller-outline nil
                                  rest)))

(defn create-the-coordinates [sentences]
  "creates the coordinates for the association graph."
  (apply #'append
         (let* ((horizontal-constant 0)
                (horizontal-cumulative 10) ;horizontal-constant)
                (vertical-constant 60)
                (vertical-cumulative 30))
           (loop for sentence in sentences
                 do (setf horizontal-constant (round (/ (+ (* (count *sentences*)
                                                              (if (any-greater-than? 12 (mapcar #'(lambda (x)(count (explode (first x)))) sentences)) 400 *hori-cons*)) 130)
                                                        (inc (count sentence)))))
                 do (setf horizontal-cumulative horizontal-constant)
                 collect (loop for word in sentence
                               collect (list horizontal-cumulative vertical-cumulative)
                               do (setf horizontal-cumulative (+ horizontal-cumulative horizontal-constant)))
                 do (setf vertical-cumulative (+ vertical-constant vertical-cumulative))))))

(defn any-greater-than? [n list-of-numbers]
  "determines if any in second arg are greater than its first arg."
  (cond ((empty? list-of-numbers)())
        ((> (first list-of-numbers) n) t)
        (t (any-greater-than? n (rest list-of-numbers)))))

(defn play-sentence [sentence]
  "plays a sentence of word objects."
  (play-events (apply #'append (re-time (loop for word in sentence
                                              if (events (eval word))
                                              collect (events (eval word)))))))

(defn re-time [event-lists &optional (current-time 0)]
  "retimes the events lists to begin one after the other."
  (if (empty? event-lists)()
      (cons (loop for event in (set-to-zero (first event-lists))
                  collect (cons (+ (first event) current-time) (rest event)))
            (re-time (rest event-lists) (+ current-time
                                           (get-beat-length
                                            (first event-lists)))))))

(defn set-to-zero [events &optional (subtract (ffirst events))]
  "sets the events to begin on zero."
  (if (empty? events)()
      (cons (cons (- (ffirst events) subtract)
                  (rest (first events)))
            (set-to-zero (rest events) subtract))))

(defn get-beat-length [events]
  "this is used in re-time for setting the new time!
   requires that the first in events be sorted to current time!"
  (let ((time (ffirst events)))
    (first (my-sort #'> (loop for event in events
                              collect (get-note-timing event time))))))

(defn get-note-timing [event time]
  "grunt work for get-beat-length"
  (- (+ (first event)(third event)) time))

(defn reveal-the-hidden-events [events]
  "reveals the events in words."
  (mapcar #'(lambda (x)(events (eval x))) events))

(defn tester-for-hidden-events [sentence]
  "tests to see if arg is a music sentence."
  (and (boundp (first sentence))
       (events (eval (first sentence)))))

(defn return-only-music-sentences [sentence-objects]
  "the arg to this should be *sentences*."
  (let ((real-sentences (apply #'append (mapcar #'(lambda (x)(sentence (eval x))) sentence-objects))))
    (loop for sentence in real-sentences
          if (tester-for-hidden-events sentence)
          collect sentence)))

(defn create-a-work [sentences]
 "the arg to this should be *sentences*"
  (reveal-the-hidden-events (apply #'append (reverse (return-only-music-sentences sentences)))))

(defn fix-end-of-music-sentences [sentence]
  "attaches the punctuation to the end of the music sentence."
 (let ((object (nth (- (count sentence) 2) sentence))
       (the-name (implode (nthcdr (- (count sentence) 2) sentence))))
   (progn (set the-name (make-instance 'word
                          :name (name (eval object))
                          :timing (timing (eval object))
                          :destination (destination (eval object))
                          :events (events (eval object))
                          :usage (usage (eval object))))
          (concat (butlast sentence 2) (list the-name)))))