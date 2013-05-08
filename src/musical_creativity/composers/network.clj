(ns musical-creativity.composers.network
  (:require
   [clojure.math.numeric-tower :as math]
   [clojure.pprint :refer :all]
   [musical-creativity.events :as events]))

(def number-of-outputs (atom 5))
(def number-of-inputs (atom 5))
(def input-patterns (atom '((0.0 0.2 0.1 0.25 0.35)
                            (0.0 0.1 0.2 0.25 0.35)
                            (0.0 0.2 0.25 0.35 0.45)
                            (0.1 0.2 0.35 0.25 0.35)
                            (0.2 0.1 0.2 0.25 0.45)
                            (0.45 0.1 0.2 0.25 0.35)
                            (0.2 0.2 0.1 0.25 0.35)
                            (0.35 0.25 0.2 0.25 0.35)
                            (0.35 0.2 0.1 0.25 0.2)
                            (0.1 0.25 0.2 0.25 0.35)
                            (0.0 0.1 0.2 0.25 0.2)
                            (0.25 0.2 0.1 0.2 0.25)
                            (0.45 0.35 0.25 0.2 0.25)
                            (0.0 0.1 0.2 0.25 0.2)
                            (0.0 0.0 0.1 0.2 0.25))))

(def array-1 (atom (double-array @number-of-inputs)))
(def array-2 (atom (double-array @number-of-inputs)))
(def array-3 (atom (double-array @number-of-inputs)))
(def array-4 (atom (double-array @number-of-inputs)))
(def array-5 (atom (double-array @number-of-inputs)))
(def array-6 (atom (double-array @number-of-inputs)))
(def array-7 (atom (double-array @number-of-inputs)))

(def output-array (atom (double-array @number-of-outputs)))

(def resetval (atom (double-array 1)))
(def y (double-array @number-of-outputs))
(def reset (atom (boolean-array @number-of-outputs false)))

(def reset-counter (atom (int-array @number-of-outputs)))

(def number-of-categories (atom (int-array @number-of-outputs)))

(def wup   (atom (make-array Double/TYPE @number-of-inputs @number-of-outputs)))
(def wdown (atom (make-array Double/TYPE @number-of-inputs @number-of-outputs)))

(def *learned-categories* (atom []))

(def learning-cycle-counter (atom 0))
(def maximum-index (atom nil))

(def skip-reset (atom false))

(def input (atom (double-array @number-of-inputs)))
(def decimals '(0.0 0.05 0.1 0.15 0.2 0.25 0.3 0.35 0.4 0.45 0.5 0.55 0.6 0.65 0.7 0.75
                   0.8 0.85 0.9 0.95 1.0))
(def pitches '(60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75  76 77 78 79 80))

; model constants:
(def a 0.5)
(def b 0.2)
(def c -1.0)
(def d 0.4)
(def e 0.04)
(def alpha 1.0)
(def theta 0.3)
(def vigilance 0.94)
(def reset-threshold 0.05)
(def uplr 0.12)
(def downlr 0.12)

(defn position [item list]
  (.indexOf list item))

(defn random-floating-point
  [low high]
  (let [the-range (- high low)]
    (+ (* (/ (rand-int 1000) 1000.0) the-range) low)))

(defn find-all
  [number lists]
  (filter #(= number (second %)) lists))

(defn count-highest
  "returns the highest occuring pattern in its arg."
  [lists]
  (let [counts (frequencies (map second lists))
        sorted-counts (sort #(> (second %1) (second %2)) counts)
        highest-value (ffirst sorted-counts)]
    (find-all highest-value lists)))

(defn find-the-largest-output
  [array reset]
  (let [array-with-indexes (map-indexed vector array)]
    (first (reduce (fn [[max-pos max-value] [new-position new-item]]
                     (if (and
                          (> new-item max-value)
                          (not (nth reset new-position)))
                       [new-position new-item]
                       [max-pos max-value])) array-with-indexes))))

(defn make-note-decimals [note-patterns]
  (map (fn [patterns]
         (map #(* % 0.01)  patterns))
       note-patterns))

(defn translate-pitches [decimal-numbers]
  (map (fn [decimal]
         (or (nth pitches (position decimal decimals))
             0.69))
       decimal-numbers))

(defn translate-to-pitches [decimal-lists]
  (mapcat translate-pitches decimal-lists))

(defn translate-decimals [pitch-numbers]
  (mapcat (fn [pitch]
            (nth decimals (position pitch pitches))) pitch-numbers))

(defn translate-to-decimals [pitch-lists]
  (mapcat translate-decimals pitch-lists))

(defn make-events
  ([pitch-groupings] (make-events pitch-groupings 0))
  ([pitch-groupings ontime]
     (if (empty? pitch-groupings)
       []
       (concat (list (events/make-event ontime (first pitch-groupings) 1))
               (make-events (rest pitch-groupings)(+ ontime 800))))))

(defn translate-into-events [output-pitch-lists]
  (make-events output-pitch-lists))

(defn l2-norm-of-a-vector
  [vector vector-length]
  (let [total-sum
        (loop [sum 0.0
               length-index 0]
          (if (>= length-index vector-length)
            sum
            (recur (+ sum (* (aget vector length-index)
                             (aget vector length-index)))
                   (+ vector-length 1) )))]
    (+ (math/sqrt total-sum) 0.001)))

(def res (atom 0.0))

(defn check-for-f2-reset []
  (reset! res 0.0)
  (let [n1 (+ (l2-norm-of-a-vector @array-7 @number-of-inputs) e)]
    (if (and
         (> n1 0.2)
         (not @skip-reset))
      (if (> @learning-cycle-counter 1)
        (when (> (aget @output-array (find-the-largest-output @output-array @reset)) 0.25)
          (reset! res (* 3.0 (l2-norm-of-a-vector @array-4 @number-of-inputs))))
        (reset! skip-reset false)))
    (aset @resetval 0 @res)
    (if (> @res (- 1.9 vigilance))
      (do
        (println (str "vigilance reset: " @res "  learning cycle: "  @learning-cycle-counter))
        (reset! maximum-index (find-the-largest-output @output-array @reset))
        (aset @reset @maximum-index true)
        (aset @reset-counter @maximum-index 80))
      (dotimes [output-index (- @number-of-outputs 1)]
        (aset @reset-counter output-index (- (aget @reset-counter output-index) 1))
        (when (< (aget @reset-counter output-index) 0)
          (when (aget @reset output-index)
            (reset! skip-reset true))
          (aset @reset output-index false)))))
  (reset! skip-reset false))

(defn check-array-value
  "returns d if (aref y index) is the largest value in array output-array and (aref output-array index) has not been reset."
  [index array reset]
  (let [maximum-index (find-the-largest-output array reset)]
    (if (and
         (= index maximum-index)
         (not (aget reset maximum-index))
         (> (aget array maximum-index) reset-threshold))
      d
      0.0)))

(defn- wdown-total-sum-fn [input-index]
  (fn [output-index sum]
    (+ sum
       (* (check-array-value output-index @output-array @reset)
          (aget @wdown output-index input-index)))))

(defn sigmoid-threshold-function [test]
  (if (> test theta)
    test
    0.0))

(defn find-maxes-in [a1 a2]
  (loop [input-index 0
         max1 -1000.0
         max2 -1000.0]
    (if (>= input-index @number-of-inputs)
      [max1 max2]
      (do
        (let [new-max1 (if (< max1 (aget a1 input-index)) (aget a1 input-index) max1)
              new-max2 (if (< max2 (aget a2 input-index)) (aget a2 input-index) max1)]
             (recur (+ 1 input-index)
                    new-max1
                    new-max2))))))

(defn update-f1-stm-arrays []
  (doall
   (map (fn [input-index]
          (let [total-sum
                (reduce (wdown-total-sum-fn input-index) (range 0 (- @number-of-outputs 1)))]
            (aset @array-7 input-index (+ (aget @array-5 input-index) total-sum))))
        (range 0 (- @number-of-inputs 1))))
  (let [norm (+ (l2-norm-of-a-vector @array-7 @number-of-inputs) e)]
    (doall (map (fn [input-index]
                  (aset @array-6 input-index (/ (aget @array-7 input-index) norm)))
                (range 0 (- @number-of-inputs 1)))))

  (let [norm (l2-norm-of-a-vector @array-3 @number-of-inputs)]
    (doall (map (fn [input-index]
                  (aset @array-5 input-index (/ (aget @array-3 input-index) norm)))
                (range 0 (- @number-of-inputs 1))))

    (dotimes [input-index (- @number-of-inputs 1)]
      (let [new-value (sigmoid-threshold-function (+ (aget @array-2 input-index)
                                                     (* b (sigmoid-threshold-function (aget @array-6 input-index)))))]
        (aset @array-3 input-index new-value)))

    (dotimes [input-index (- @number-of-inputs 1)]
      (aset @array-2 input-index (/ (aget @array-1 input-index) norm))))

  ; update array-2 using eq. 9:
  (let [norm (+ (l2-norm-of-a-vector @array-1 @number-of-inputs) e)]
    (dotimes [input-index (- @number-of-inputs 1)]
      (aset @array-2 input-index (/ (aget @array-1 input-index) norm))))

  (let [[max1 max2] (find-maxes-in @array-5 @array-7)
        max1 (+ max1 0.001)
        max2 (+ max2 0.001)]
    (dotimes [input-index (- @number-of-inputs 1)]
      (aset @array-4 input-index
            (- (/ (aget @array-5 input-index) max1)
               (/ (aget @array-7 input-index) max2))))))

(declare run-one-full-cycle)

(defn update-f2-stm-storage []
  (loop [output-index 0]
    (when (< output-index @number-of-outputs)
      (loop [input-index 0
             sum 0.0]
        (if (< input-index @number-of-inputs)
          (recur (+ 1 input-index)
                 (+ sum (* (aget @array-7 input-index)
                           (aget @wup input-index output-index))))
          (aset @output-array output-index sum)))

      (when (aget @reset output-index)
        (aset @output-array output-index -0.1))
      (recur (+ 1 output-index)))))

(defn update-weights []
  (let [largest-output (find-the-largest-output @output-array @reset)]
    (if (> (check-array-value largest-output @output-array @reset) 0.02)
      (dotimes [increment (- @number-of-inputs 1)]

        (aset @wdown largest-output increment
              (+
               (aget @wdown largest-output increment)
               (* downlr d
                  (- (aget @array-7 increment) (aget @wdown largest-output increment)))))

        (aset @wup increment largest-output
              (+
               (aget @wup increment largest-output)
               (* uplr d
                  (- (aget @array-7 increment) (aget @wup increment largest-output)))))))))

(defn competitive-learning-at-f2 []
  (let [largest-output (find-the-largest-output @output-array @reset)]
    (if (> (aget @output-array largest-output) reset-threshold)
      (dotimes [output-index (- @number-of-outputs 1)]
        (if-not (= output-index largest-output)
          (aset @output-array output-index 0.0))))))

(defn run-one-full-cycle []
  (update-f1-stm-arrays)
  (check-for-f2-reset)
  (competitive-learning-at-f2)
  (update-f2-stm-storage)
  (update-weights)
  (competitive-learning-at-f2))

(defn zero-activations []
  (dotimes [input-index (- @number-of-inputs 1)]
    (aset @array-1 input-index 0.0)
    (aset @array-2 input-index 0.0)
    (aset @array-3 input-index 0.0)
    (aset @array-4 input-index 0.0)
    (aset @array-5 input-index 0.0)
    (aset @array-6 input-index 0.0)
    (aset @array-7 input-index 0.0))
  (dotimes [output-index (- @number-of-outputs 1)]
    (aset @output-array output-index 0.0)
    (aset @reset output-index true)
    (aset @reset-counter output-index 0)))

(defn set-learning-pattern
  "sets up a learning pattern in the input neurons."
  [input-pattern]
  (let [length (count input-pattern)]
    (if-not (= length @number-of-inputs)
      (print (list "error in set-learning-pattern input:" input-pattern))
      (do
        (reset! learning-cycle-counter 0)
        (zero-activations)
        (doall
         (map-indexed (fn [index item]
                        (aset @input index (+ item (random-floating-point -0.08 0.08))))
                      input-pattern))))))

(defn learn-fn [number]
  (fn [input-pattern]
    (set-learning-pattern input-pattern)
    (dotimes [n number]
      (reset! learning-cycle-counter (+ 1 @learning-cycle-counter))
      (run-one-full-cycle))

    (let [largest-output (find-the-largest-output @output-array @reset)
          new-category (list @array-7 largest-output)]

      (swap! *learned-categories* conj new-category))))

(defn learn-the-patterns
  "cycles through all training patterns once."
  [input-patterns number]
  (doall (map (learn-fn number) input-patterns))
  @*learned-categories*)

(defn initialize-the-network []
  (zero-activations)
  (dotimes [output-index (- @number-of-outputs 1)]
    (dotimes [input-index (- @number-of-inputs 1)]
      (aset @wup input-index output-index (random-floating-point 0.05 0.1))
      (aset @wdown output-index input-index (random-floating-point 0.01 0.03)))
    (aset @number-of-categories output-index 0)))

(defn reset-all! [number-inputs number-outputs]
  (reset! number-of-inputs number-inputs)
  (reset! number-of-outputs number-outputs)

  (reset! input (double-array number-inputs))
  (reset! array-1 (double-array number-inputs))
  (reset! array-2 (double-array number-inputs))
  (reset! array-3 (double-array number-inputs))
  (reset! array-4 (double-array number-inputs))
  (reset! array-5 (double-array number-inputs))
  (reset! array-6 (double-array number-inputs))
  (reset! array-7 (double-array number-inputs))

  (reset! resetval (double-array 1))

  (reset! output-array (double-array number-outputs))

  (reset! reset (boolean-array number-outputs false))

  (reset! reset-counter (int-array number-outputs))

  (reset! number-of-categories (int-array number-outputs))

  (reset! wup (make-array Double/TYPE number-inputs number-outputs))
  (reset! wdown (make-array Double/TYPE number-outputs number-inputs))
  (reset! *learned-categories* nil))

(defn initialize-network
  ([number-inputs number-outputs] (initialize-network number-inputs number-outputs nil))
  ([number-inputs number-outputs training-patterns]

      (if training-patterns
        (if (= number-inputs (count (first training-patterns)))
          (reset! @input-patterns training-patterns)
          (println (str "error: bad input to initialize-network. number-inputs should have been" (count (first training-patterns)))))

        (if-not (= number-inputs (count (first @input-patterns)))
          (println  (str "error: bad input to initialize-network. number-inputs should have been" (count (first @input-patterns))))
          (reset-all! number-inputs number-outputs)))))

(defn run-neural-net [input-patterns]
  (initialize-network 5 5)
  (initialize-the-network)

  (let [learned-categories (learn-the-patterns input-patterns 50)
        input-and-categories-pair (map vector input-patterns (map second learned-categories))
        highest-5 (take 5 (count-highest input-and-categories-pair))]
    (print "Learned categories: ")
    (pprint (map second learned-categories))
    (println highest-5)

    (translate-to-pitches (map first highest-5))))

(defn compose []
  (translate-into-events (run-neural-net @input-patterns)))