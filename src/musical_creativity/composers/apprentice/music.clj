(ns musical-creativity.composers.apprentice.music
  (:require [musical-creativity.composers.apprentice :refer :all]
            [musical-creativity.util :refer :all]))

(defn remove-cadences
  "removes the cadences from the choices."
  [choices]
  (cond
   (empty? choices)()
   (or (member? (second (first choices)) (:cadences @*answer-lexicon*))
       (member? (second (first choices)) (:cadences @*question-lexicon*)))
   (remove-cadences (rest choices))
   :else (cons (first choices) (remove-cadences (rest choices)))))

(defn pair [list-1 list-2]
  "pairs the two list args."
  (map vector list-1 list-2))

(defn collect-parsings
  "pairs the parsings with the words in the sentences."
  [sentences]
  (pair (apply concat (map (fn [sentence] (first (:sentence (eval sentence)))) sentences))
        (apply concat (map (fn [sentence] (:parse-it (eval sentence))) sentences))))

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
            (map (fn [element] (second (assoc element (shuffle parse-lists)))) (butlast parse)))
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
          (cons (map (fn [event] (cons (+ (first event) current-time) (rest event))) (set-to-zero (first event-lists)))
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
  (map (fn [word] (:events (lookup-word word))) events))

(defn tester-for-hidden-events
  "test if sentence is a music sentence."
  [sentence]
  (and (sentence-seen? (first sentence))
       (:events (lookup-sentence (first sentence)))))

(defn return-only-music-sentences
  [sentences]
  (let [real-sentences (apply concat (map (fn [x] (:sentence (eval x))) sentences))]
    (remove (fn [sentence] (when-not (tester-for-hidden-events sentence) true)) real-sentences)))

(defn create-a-work
  [sentences]
  (reveal-the-hidden-events (apply concat (reverse (return-only-music-sentences sentences)))))
