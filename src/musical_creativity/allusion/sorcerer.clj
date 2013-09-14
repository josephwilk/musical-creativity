(ns musical-creativity.allusion.sorcerer
  (:require [musical-creativity.util :refer :all]
            [musical-creativity.events :as events]))

(def *choices* '(chopin-1 bach-1 bach-2 bach-3 bach-4 beethoven-1 beethoven-2 boccherini-1 chausson-1 schumann-1))

(def chopin-1 '((0 59 1000 1 64)(1000 64 1500 1 64)(2500 63 500 1 64)(3000 64 500 1 64)
                (3500 67 500 1 64)(4000 71 1000 1 64)(5000 71 1000 1 64)(6000 71 1000 1 64)(7000 76 1000 1 64)
                (8000 75 500 1 64)(8500 76 500 1 64)(9000 78 750 1 64)(9750 76 250 1 64)(10000 76 1000 1 64)
                (11000 75 1000 1 64)))

(def bach-1 '((0 71 500 1 64)(500 74 500 1 64)(1000 78 500 1 64)(1500 83 1000 1 64)
              (2500 81 500 1 64)(3000 83 1000 1 64)))

(def bach-2 '((0 64 250 1 64)(250 67 250 1 64)(500 71 250 1 64)(750 76 250 1 64)
              (1000 75 250 1 64)(1250 76 250 1 64)))

(def bach-3 '((0 69 500 1 64)(500 62 500 1 64)(1000 65 500 1 64)(1500 69 500 1 64)
              (2000 74 500 1 64)(2500 76 500 1 64)))

(def bach-4 '((0 52 500 1 64)(500 57 500 1 64)(1000 60 500 1 64)(1500 64 500 1 64)
              (2000 69 500 1 64)(2500 68 500 1 64)(3000 69 500 1 64)(3500 59 500 1 64)))

(def beethoven-1 '((0 55 750 1 64)(750 55 250 1 64)(1000 55 2000 1 64)(3000 60 750 1 64)
                   (3750 59 250 1 64)(4000 60 750 1 64)(4750 62 250 1 64)))

(def beethoven-2 '((0 77 500 1 64)(500 82 1500 1 64)(2000 81 250 1 64)(2250 82 250 1 64)
                   (2500 84 500 1 64)(3000 82 500 1 64)))

(def boccherini-1 '((0 81 250 1 64)(250 80 250 1 64)(500 81 250 1 64)(750 83 250 1 64)
                    (1000 81 500 1 64)))

(def chausson-1 '((0 58 2000 1 64)(2000 63 1000 1 64)(3000 62 1000 1 64)(4000 63 1000 1 64)
                  (5000 66 1000 1 64)(6000 70 1000 1 64)(7000 70 3000 1 64)(10000 75 1000 1 64)))

(def schumann-1 '((0 60 1000 1 64)(1000 65 2500 1 64)(3500 64 500 1 64)(4000 65 500 1 64)
                  (4500 69 500 1 64)(5000 72 1000 1 64)(6000 77 2000 1 64)(8000 76 1000 1 64)))

(defn get-intervals
 "returns intervals for pitches."
 [events]
 (map (fn [[event1 event2]] (- (second event2) (second event1)))
      (partition 2 1 events)))

(defn count-xs
  "counts the x's in the list."
  ([list] (count-xs list 0))
  ([list n]
      (cond
       (empty? list) ()
       (not= (first list) :x) n
       :else (count-xs (rest list) (inc n)))))

(defn sort-by-first-element [lists]
  (sort (fn [[x & _] [y & _]] (< x y))  lists))

(defn add-timing
  "adds timing to the ontimes of events."
  [timing events]
  (map #(cons (+ timing (first %)) (rest %)) events))

(defn get-timing
  "this gets the timing of the source work."
  [source-events xd-source-list]
  (when-let [test (count-xs xd-source-list)]
    (first (nth source-events test))))

(defn change-channel
  "changes the channel of music to channel."
  [music channel]
  (if (empty? music)
    ()
    (cons (concat (take 3 (first music))
                  (list channel)
                  (drop 4 (first music)))
          (change-channel (rest music) channel))))

(defn match [subject object]
  (cond
   (= subject object) true
   (and (member subject '(1 2)) (member object '(1 2))) true
   (and (member subject '(3 4)) (member object '(3 4))) true
   (and (member subject '(5 6)) (member object '(5 6))) true
   (and (member subject '(8 9)) (member object '(8 9))) true
   (and (member subject '(10 11)) (member object '(10 11))) true
   (and (member subject '(-1 -2)) (member object '(-1 -2))) true
   (and (member subject '(-3 -4)) (member object '(-3 -4))) true
   (and (member subject '(-5 -6)) (member object '(-5 -6))) true
   (and (member subject '(-8 -9)) (member object '(-8 -9))) true
   (and (member subject '(-10 -11)) (member object '(-10 -11))) true
   :else false))

(defn invert
  "inverts the interval mod 23."
  [interval]
  (- interval 12))

(defn pattern-match
  ([target-pattern-list source-pattern-list] (pattern-match target-pattern-list source-pattern-list 0 true))
  ([target-pattern-list source-pattern-list last-interval] (pattern-match target-pattern-list source-pattern-list last-interval true))
  ([target-pattern-list source-pattern-list last-interval initial]
     (cond
      (and (empty? target-pattern-list) (empty? source-pattern-list)) ()
      (empty? target-pattern-list) ()
      (empty? source-pattern-list) (cons :x (pattern-match (rest target-pattern-list) source-pattern-list (first target-pattern-list)))
      (or (match (+ (first target-pattern-list) last-interval) (first source-pattern-list))
          (= (invert (+ (first target-pattern-list) last-interval))(first source-pattern-list)))
      (cons (first source-pattern-list) (pattern-match (rest target-pattern-list) (rest source-pattern-list) 0 nil))
      :else (cons :x (pattern-match (rest target-pattern-list) source-pattern-list
                                    (if initial 0 (+ last-interval (first target-pattern-list)))
                                    initial)))))

(defn find-patterns [target-pattern names-of-database-works database-names]
  (if (empty? names-of-database-works)
    ()
    (if-let [test (pattern-match target-pattern (get-intervals (first names-of-database-works)))]
      (cons (list test (first database-names)) (find-patterns target-pattern (rest names-of-database-works) (rest database-names)))
      (find-patterns target-pattern (rest names-of-database-works) (rest database-names)))))

(defn set-channels [target list-of-sources patterns]
  (loop [patterns patterns
         counter 0
         list-of-sources list-of-sources
         hits []]
    (if (empty? patterns)
      hits
      (let [timing (get-timing target (ffirst patterns))
            thing (if timing
                    (change-channel (add-timing timing (first list-of-sources)) counter)
                    [])]
        (recur (rest patterns) (inc counter) (rest list-of-sources) (cons thing hits))))))

(defn find-the-patterns
  "runs the matcher on intervals rather than pitches."
  [target-work-name names-of-database-works database-names]
  (find-patterns (get-intervals target-work-name) names-of-database-works database-names))

(defn source-it
  "runs the basic grunt functions of the matcher."
  [target list-of-sources database-names]
  (let [patterns (find-the-patterns target list-of-sources database-names)
        results (set-channels target list-of-sources patterns)
        results (apply concat (cons target results))
        results (sort-by-first-element results)]
    (println :patterns patterns)
    (println :results results)
    results))

(defn sorcerer [target-phrase database-works]
  (source-it (eval target-phrase) (map #(eval %) database-works) database-works))
