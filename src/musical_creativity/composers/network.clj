(ns musical-creativity.composers.network
  (:require
   [clojure.math.numeric-tower :as math]
   [clojure.pprint :refer :all]
   [musical-creativity.events :as events]))

(def number-of-outputs (atom 5))
(def number-of-inputs (atom 5))
(def input-patterns (atom '((0.0 0.2 0.1 0.25 0.35) (0.0 0.1 0.2 0.25 0.35) (0.0 0.2 0.25 0.35 0.45)
                           (0.1 0.2 0.35 0.25 0.35) (0.2 0.1 0.2 0.25 0.45) (0.45 0.1 0.2 0.25 0.35) (0.2 0.2 0.1 0.25 0.35)
                           (0.35 0.25 0.2 0.25 0.35) (0.35 0.2 0.1 0.25 0.2) (0.1 0.25 0.2 0.25 0.35)
                           (0.0 0.1 0.2 0.25 0.2) (0.25 0.2 0.1 0.2 0.25) (0.45 0.35 0.25 0.2 0.25) (0.0 0.1 0.2 0.25 0.2)
                           (0.0 0.0 0.1 0.2 0.25))))

(def array-1 (atom (double-array @number-of-inputs)))
(def array-2 (atom (double-array @number-of-inputs)))
(def array-3 (atom (double-array @number-of-inputs)))
(def array-4 (atom (double-array @number-of-inputs)))
(def array-5 (atom (double-array @number-of-inputs)))
(def array-6 (atom (double-array @number-of-inputs)))
(def array-7 (atom (double-array @number-of-inputs)))

(def array-8 (atom (double-array @number-of-outputs)))

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

(def skipreset (atom nil))

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

(defn find-the-largest-output
  "finds the largest output."
  [array reset]
  (let [array-with-indexes (map-indexed vector array)]
    (first (reduce (fn [[max-pos max-value] [new-position new-item]]
                     (if (and
                          (> new-item max-value)
                          (not (nth reset new-position)))
                       [new-position new-item]
                       [max-pos max-value])) array-with-indexes))))

(defn vector-l2-norm
  "l2 norm of a vector."
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

(defn check-for-f2-reset []
  "check for an f2 reset condition."
  (let [res 0.0
        n1 (+ (vector-l2-norm @array-7 @number-of-inputs) e)]
    (if (and
         (> n1 0.2)
         (not skipreset))
      (if (> @learning-cycle-counter 1)
        (if (> (aget @array-8 (find-the-largest-output @array-8 @reset)) 0.25)
          (reset! res (* 3.0 (vector-l2-norm @array-4 @number-of-inputs))))  ; was 3.0
        (reset! skipreset nil)))
    (aset @resetval 0 res)
    (if (> res (- 1.9 vigilance))
      (do
        (print (list "vigilance reset =" res "  learning cycle ="
                     @learning-cycle-counter))
        (reset! maximum-index (find-the-largest-output @array-8 @reset))
        (aset @reset @maximum-index true)
        (aset @reset-counter @maximum-index 80))
      (dotimes [output-index (- @number-of-outputs 1)]
        (aset @reset-counter output-index (- (aget @reset-counter output-index) 1))
        (if (< (aget @reset-counter output-index) 0)
          (do
            (if (aget @reset output-index)  (reset! skipreset true))
            (aset @reset output-index false))))))
  (reset! skipreset nil))

(defn zero-activations []
  "zero activations."
  (dotimes [input-index (- @number-of-inputs 1)]
    (aset @array-1 input-index 0.0)
    (aset @array-2 input-index 0.0)
    (aset @array-3 input-index 0.0)
    (aset @array-4 input-index 0.0)
    (aset @array-5 input-index 0.0)
    (aset @array-6 input-index 0.0)
    (aset @array-7 input-index 0.0))
  (dotimes [output-index (- @number-of-outputs 1)]
    (aset @array-8 output-index 0.0)
    (aset @reset output-index true)
    (aset @reset-counter output-index 0)))

(defn floating-point-random
  "floating point random numbers."
  [low high]
  (let [the-range (- high low)]
    (+ (* (/ (rand-int 1000) 1000.0) the-range) low)))

(defn set-learning-pattern
  "sets up a learning pattern in the input neurons."
  [input-pattern]
  (let [length (count input-pattern)]
    (if (not (= length @number-of-inputs))
      (print (list "error in set-learning-pattern input:" input-pattern))
      (do
        (reset! learning-cycle-counter 0)
        (zero-activations)
        (doall
         (map-indexed (fn [index item]
                        (aset @input index (+ item (floating-point-random -0.08 0.08))))
                      input-pattern))))))

(defn initialize-the-network []
  "initialize the network."
  (zero-activations)
  (dotimes [output-index (- @number-of-outputs 1)]
    (dotimes [input-index (- @number-of-inputs 1)]
      (aset @wup input-index output-index (floating-point-random 0.05 0.1))
      (aset @wdown output-index input-index (floating-point-random 0.01 0.03)))
    (aset @number-of-categories output-index 0)))

(defn check-array-value
  "returns d if (aref y index) is the largest value in array array-8 and (aref array-8 index) has not been reset."
  [index]
  (let [maximum-index (find-the-largest-output @array-8 @reset)]
    (if (and
         (= index maximum-index)
         (not (aget @reset maximum-index))
         (> (aget @array-8 maximum-index) reset-threshold))
      d
      0.0)))

(defn- wdown-total-sum-fn [input-index]
  (fn [output-index sum]
    (+ sum
       (* (check-array-value output-index)
          (aget @wdown output-index input-index)))))

(defn sigmoid-threshold-function [test]
  "threshold function."
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
  (doall (map (fn [input-index]
                (let [total-sum
                      (reduce (wdown-total-sum-fn input-index) (range 0 (- @number-of-outputs 1)))]
                  (aset @array-7 input-index (+ (aget @array-5 input-index) total-sum))))
              (range 0 (- @number-of-inputs 1))))

  (let [norm (+ (vector-l2-norm @array-7 @number-of-inputs) e)]
    (doall (map (fn [input-index]
                  (aset @array-6 input-index (/ (aget @array-7 input-index) norm)))
                (range 0 (- @number-of-inputs 1)))))

  (let [norm (vector-l2-norm @array-3 @number-of-inputs)]
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
  (let [norm (+ (vector-l2-norm @array-1 @number-of-inputs) e)]
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

(defn learn-the-patterns
  "cycles through all training patterns once."
  [number]
  (doall (map (fn [input-pattern]
                 (set-learning-pattern input-pattern)
                 (dotimes [n number]
                   (reset! learning-cycle-counter (+ 1 @learning-cycle-counter))
                   (run-one-full-cycle))
                 (reset! *learned-categories*
                         (cons (list @array-7 (find-the-largest-output @array-8 @reset))
                               @*learned-categories*))) @input-patterns)))

(defn pair
  "pairs the two args together."
  [one two]
  (if (or
       (empty? one)
       (empty? two))
    []
    (cons (list (first one) (first two))
          (pair (rest one) (rest two)))))

(defn make-note-decimals [note-patterns]
  "transforms note patterns into decimal patterns."
  (if (nil? note-patterns)()
      (cons (map (fn [x] (* x 0.01)) (first note-patterns))
            (make-note-decimals (rest note-patterns)))))

(defn firstn [number list]
  "returns the first n of list."
 (if (< (count list) number)(firstn (- number 1) list)
     (drop-last (- (count list) number) list)))

(defn initialize-network
  "initializes the neural network."
  ([number-inputs number-outputs] (initialize-network number-inputs number-outputs nil))
  ([number-inputs number-outputs training-patterns]
                                        ; check for specified training patterns:
      (if training-patterns
                                        ; make sure the number of input neurons agrees with
                                        ; the size of the training patterns:
        (if (= (count (first training-patterns)) number-inputs)
          (reset! @input-patterns training-patterns)
          (print
           (list
            "error: bad input to initialize-network. number-inputs should have been"
            (count (first training-patterns)))))
                                        ; no specified training patterns: use the default set
                                        ; defined in this package:
        (if (not (= (count (first @input-patterns)) number-inputs))
          (print
           (list
            "error: bad input to initialize-network. number-inputs should have been"
            (count (first @input-patterns))))
                                        ; specified number of input neurons agrees with
                                        ; the size of the default training patterns defined
                                        ; in this package; proceed with defining network data:
          (do
                                        ; resets the network
                                        ; define the network size:
            (reset! number-of-inputs number-inputs)
            (reset! number-of-outputs number-outputs)
                                        ; array storage allocation:
            (reset! input (double-array @number-of-inputs))
            (reset! array-1 (double-array @number-of-inputs))
            (reset! array-2 (double-array @number-of-inputs))
            (reset! array-3 (double-array @number-of-inputs))
            (reset! array-4 (double-array @number-of-inputs))
            (reset! array-5 (double-array @number-of-inputs))
            (reset! array-6 (double-array @number-of-inputs))
            (reset! array-7 (double-array @number-of-inputs))

            (reset! resetval (double-array 1))

            (reset! array-8 (double-array @number-of-outputs))

            (reset! reset (boolean-array @number-of-outputs false))

            (reset! reset-counter (int-array @number-of-outputs))

            (reset! number-of-categories (int-array @number-of-outputs))

            (reset! wup (make-array Double/TYPE @number-of-inputs @number-of-outputs))
            (reset! wdown (make-array Double/TYPE @number-of-outputs @number-of-inputs))
                                        ; global variable to remember input patterns and
                                        ; their associated output category code for plotting
                                        ; by function art2-postprocess:
            (reset! *learned-categories* nil))))))

(defn update-f2-stm-storage
  "updates f2 stm storage."
  []
  (loop [output-index 0]
    (when (< output-index @number-of-outputs)
      (loop [input-index 0
             sum 0.0]
        (if (< input-index @number-of-inputs)
          (recur (+ 1 input-index)
                 (+ sum (* (aget @array-7 input-index)
                           (aget @wup input-index output-index))))
          (aset @array-8 output-index sum)))

      (when (aget @reset output-index) (aset @array-8 output-index -0.1))
      (recur (+ 1 output-index)))))

(defn update-weights []
  "updates the weights."
  (let [largest-output (find-the-largest-output @array-8 @reset)]
    (if (> (check-array-value largest-output) 0.02)
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
  "competitive learning at slab f2."
  (let [largest-output (find-the-largest-output @array-8 @reset)]
    (if (> (aget @array-8 largest-output) reset-threshold)
      (dotimes [output-index (- @number-of-outputs 1)]
        (if-not (= output-index largest-output)
          (aset @array-8 output-index 0.0))))))

(defn translate-pitches [decimal-numbers]
  "helps transform decimal patterns into note patterns."
  (map (fn [decimal]
         (or (nth pitches (position decimal decimals))
             0.69))
       decimal-numbers))

(defn translate-to-pitches [decimal-lists]
  "transforms decimal patterns into note patterns."
  (mapcat #(translate-pitches %) decimal-lists))

(defn translate-decimals [pitch-numbers]
  "changes pitches into 0.0 - 1.0 range decimals."
  (mapcat (fn [pitch]
            (nth decimals (position pitch pitches))) pitch-numbers))

(defn translate-to-decimals [pitch-lists]
  "changes lists of pitches into lists of 0.0 - 1.0 range decimals."
  (mapcat #(translate-decimals %) pitch-lists))

(defn make-events
  "makes consecutive events out of the pairs of pitches in its arg."
  ([pitch-groupings] (make-events pitch-groupings 0))
  ([pitch-groupings ontime]
     (if (empty? pitch-groupings)
       []
       (concat (list (events/make-event ontime (first pitch-groupings) 1))
               (make-events (rest pitch-groupings)(+ ontime 800))))))

(defn translate-into-events [output-pitch-lists]
  "returns sontiguous events from its pitch-lists arg."
  (make-events output-pitch-lists))

(defn find-all
  "returns all of the patterns associated by cdr with number."
  [number lists]
  (filter #(= number (second %)) lists))

(defn count-them [singles numbers]
  "returns the counts of its first arg in its second arg."
  (map (fn [single] (count (filter #{single} numbers))) singles))

(defn count-highest
  "returns the highest occuring pattern in its arg."
  [lists]
  (let [sorted-numbers (sort < (map second lists))
        numbers-only (distinct sorted-numbers)
        counts (count-them numbers-only sorted-numbers)
        highest-count (first (sort > counts))
        highest-position (position highest-count counts)
        highest-value (nth numbers-only highest-position)]
    (find-all highest-value lists)))

(defn run-one-full-cycle []
  "run one full cycle."
  (update-f1-stm-arrays)
  (check-for-f2-reset)
  (competitive-learning-at-f2)
  (update-f2-stm-storage)
  (update-weights)
  (competitive-learning-at-f2))

(defn run-neural-net []
  "assumes patterns are in decimal form."
  (initialize-network 5 5)
  (initialize-the-network)
  (learn-the-patterns 50)

  (print "Learned categories: ")
  (pprint (map second @*learned-categories*))

  (translate-to-pitches (map first
                             (firstn 5
                                     (count-highest
                                      (pair @input-patterns (map second @*learned-categories*)))))))

(defn compose []
  (translate-into-events (run-neural-net)))