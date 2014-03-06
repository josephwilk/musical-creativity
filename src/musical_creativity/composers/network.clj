(ns musical-creativity.composers.network
  (:require
   [clojure.math.numeric-tower :as math]
   [clojure.pprint :refer :all]
   [musical-creativity.events :as events]
   [musical-creativity.util :refer :all]))

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

(def input (atom (double-array @number-of-inputs)))

(def layer-1 (atom (double-array @number-of-inputs)))
(def layer-2 (atom (double-array @number-of-inputs)))
(def layer-3 (atom (double-array @number-of-inputs)))
(def layer-4 (atom (double-array @number-of-inputs)))
(def layer-5 (atom (double-array @number-of-inputs)))
(def layer-6 (atom (double-array @number-of-inputs)))
(def layer-7 (atom (double-array @number-of-inputs)))

(def output (atom (double-array @number-of-outputs)))

(def reset-val (atom (double-array 1)))
(def y (double-array @number-of-outputs))

(def reset (atom (boolean-array @number-of-outputs false)))
(def reset-counter (atom (int-array @number-of-outputs)))

(def number-of-categories (atom (int-array @number-of-outputs)))

(def weights-up   (atom (make-array Double/TYPE @number-of-inputs @number-of-outputs)))
(def weights-down (atom (make-array Double/TYPE @number-of-inputs @number-of-outputs)))

(def learning-cycle-counter (atom 0))
(def maximum-index (atom nil))

(def skip-reset (atom false))

(def decimals '(0.0 0.05 0.1 0.15 0.2 0.25 0.3 0.35 0.4 0.45 0.5 0.55 0.6 0.65 0.7 0.75 0.8 0.85 0.9 0.95 1.0))
(def pitches (range 60 81))

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

(defn random-floating-point
  [low high]
  (let [the-range (- high low)]
    (+ (* (/ (rand-int 1000) 1000.0) the-range) low)))

(defn find-all
  [number lists]
  (filter #(= number (second %)) lists))

(defn highest-occuring-pattern
  "returns the highest occuring pattern in its arg."
  [lists]
  (let [counts (frequencies (map second lists))
        sorted-counts (sort #(> (second %1) (second %2)) counts)
        highest-value (ffirst sorted-counts)]
    (find-all highest-value lists)))

(defn find-the-largest-output
  [array reset]
  (let [layer-with-indexes (map-indexed vector array)]
    (first (reduce (fn [[max-pos max-value] [new-position new-item]]
                     (if (and
                          (> new-item max-value)
                          (not (nth reset new-position)))
                       [new-position new-item]
                       [max-pos max-value])) layer-with-indexes))))

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

(defn total-sum [vector]
  (reduce (fn [sum x] (+ sum x)) 0 vector))

(defn total-sum-squared [vector]
  (reduce (fn [sum x] (+ sum (* x x))) 0 vector))

(defn l2-norm-of-a-vector [vector]
  (+ (math/sqrt (total-sum-squared vector)) 0.001))

(defn calculate-ref [_]
  (* 3.0 (l2-norm-of-a-vector @layer-4)))

(defn check-for-f2-reset []
  (let [res (ref 0.0)
        n1 (+ (l2-norm-of-a-vector @layer-7) e)]
    (if (and
         (> n1 0.2)
         (not @skip-reset))
      (if (> @learning-cycle-counter 1)
        (when (> (aget @output (find-the-largest-output @output @reset)) 0.25)
          (dosync (alter res calculate-ref)))
        (reset! skip-reset false)))
    (aset @reset-val 0 @res)
    (if (> @res (- 1.9 vigilance))
      (do
        (println (str "vigilance reset: " @res "  learning cycle: "  @learning-cycle-counter))
        (reset! maximum-index (find-the-largest-output @output @reset))
        (aset @reset @maximum-index true)
        (aset @reset-counter @maximum-index 80))
      (dotimes [output-index (dec @number-of-outputs)]
        (aset @reset-counter output-index (dec (aget @reset-counter output-index)))
        (when (neg? (aget @reset-counter output-index))
          (when (aget @reset output-index)
            (reset! skip-reset true))
          (aset @reset output-index false))))
    (reset! skip-reset false)))

(defn check-layer-value
  [index array reset]
  (let [maximum-index (find-the-largest-output array reset)]
    (if (and
         (= index maximum-index)
         (not (aget reset maximum-index))
         (> (aget array maximum-index) reset-threshold))
      d
      0.0)))

(defn- weights-down-total-sum-fn [input-index]
  (fn [output-index sum]
    (+ sum
       (* (check-layer-value output-index @output @reset)
          (aget @weights-down output-index input-index)))))

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
      (let [new-max1 (if (< max1 (aget a1 input-index)) (aget a1 input-index) max1)
            new-max2 (if (< max2 (aget a2 input-index)) (aget a2 input-index) max2)]
        (recur (inc input-index)
               new-max1
               new-max2)))))

(defn update-f1-stm-arrays []
  (doall
   (map (fn [input-index]
          (let [total-sum
                (reduce (weights-down-total-sum-fn input-index) (range 0 (dec @number-of-outputs)))]
            (aset @layer-7 input-index (+ (aget @layer-5 input-index) total-sum))))
        (range 0 (dec @number-of-inputs))))
  (let [norm (+ (l2-norm-of-a-vector @layer-7) e)]
    (doall (map (fn [input-index]
                  (aset @layer-6 input-index (/ (aget @layer-7 input-index) norm)))
                (range 0 (dec @number-of-inputs)))))

  (let [norm (l2-norm-of-a-vector @layer-3)]
    (doall (map (fn [input-index]
                  (aset @layer-5 input-index (/ (aget @layer-3 input-index) norm)))
                (range 0 (dec @number-of-inputs))))

    (dotimes [input-index (dec @number-of-inputs)]
      (let [new-value (sigmoid-threshold-function (+ (aget @layer-2 input-index)
                                                     (* b (sigmoid-threshold-function (aget @layer-6 input-index)))))]
        (aset @layer-3 input-index new-value)))

    (dotimes [input-index (dec @number-of-inputs)]
      (aset @layer-2 input-index (/ (aget @layer-1 input-index) norm))))

  (let [norm (+ (l2-norm-of-a-vector @layer-1) e)]
    (dotimes [input-index (dec @number-of-inputs)]
      (aset @layer-2 input-index (/ (aget @layer-1 input-index) norm))))

  (let [[max1 max2] (find-maxes-in @layer-5 @layer-7)
        max1 (+ max1 0.001)
        max2 (+ max2 0.001)]
    (dotimes [input-index (dec @number-of-inputs)]
      (aset @layer-4 input-index
            (- (/ (aget @layer-5 input-index) max1)
               (/ (aget @layer-7 input-index) max2))))))

(defn update-f2-stm-storage []
  (loop [output-index 0]
    (when (< output-index @number-of-outputs)
      (loop [input-index 0
             sum 0.0]
        (if (< input-index @number-of-inputs)
          (recur (inc input-index)
                 (+ sum (* (aget @layer-7 input-index)
                           (aget @weights-up input-index output-index))))
          (aset @output output-index sum)))

      (when (aget @reset output-index)
        (aset @output output-index -0.1))
      (recur (inc output-index)))))

(defn update-weights []
  (let [largest-output (find-the-largest-output @output @reset)]
    (if (> (check-layer-value largest-output @output @reset) 0.02)
      (dotimes [increment (dec @number-of-inputs)]
        (aset @weights-down largest-output increment
              (+
               (aget @weights-down largest-output increment)
               (* downlr d
                  (- (aget @layer-7 increment) (aget @weights-down largest-output increment)))))

        (aset @weights-up increment largest-output
              (+
               (aget @weights-up increment largest-output)
               (* uplr d
                  (- (aget @layer-7 increment) (aget @weights-up increment largest-output)))))))))

(defn competitive-learning-at-f2 []
  (let [largest-output (find-the-largest-output @output @reset)]
    (if (> (aget @output largest-output) reset-threshold)
      (dotimes [output-index (dec @number-of-outputs)]
        (if-not (= output-index largest-output)
          (aset @output output-index 0.0))))))

(defn run-one-full-cycle []
  (update-f1-stm-arrays)
  (check-for-f2-reset)
  (competitive-learning-at-f2)
  (update-f2-stm-storage)
  (update-weights)
  (competitive-learning-at-f2))

(defn zero-activations []
  (dotimes [input-index (dec @number-of-inputs)]
    (aset @layer-1 input-index 0.0)
    (aset @layer-2 input-index 0.0)
    (aset @layer-3 input-index 0.0)
    (aset @layer-4 input-index 0.0)
    (aset @layer-5 input-index 0.0)
    (aset @layer-6 input-index 0.0)
    (aset @layer-7 input-index 0.0))
  (dotimes [output-index (dec @number-of-outputs)]
    (aset @output output-index 0.0)
    (aset @reset output-index true)
    (aset @reset-counter output-index 0)))

(defn set-learning-pattern
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
      (reset! learning-cycle-counter (inc @learning-cycle-counter))
      (run-one-full-cycle))

    (let [largest-output (find-the-largest-output @output @reset)
          new-category (list (vec @layer-7) largest-output)]
      new-category)))

(defn learn-the-patterns
  "cycles through all training patterns once."
  [input-patterns number]
  (doall
    (map (learn-fn number) input-patterns)))

(defn initialize-the-network []
  (zero-activations)
  (dotimes [output-index (dec @number-of-outputs)]
    (dotimes [input-index (dec @number-of-inputs)]
      (aset @weights-up input-index output-index (random-floating-point 0.05 0.1))
      (aset @weights-down output-index input-index (random-floating-point 0.01 0.03)))
    (aset @number-of-categories output-index 0)))

(defn reset-all! [number-inputs number-outputs]
  (reset! number-of-inputs number-inputs)
  (reset! number-of-outputs number-outputs)

  (reset! input (double-array number-inputs))
  (reset! layer-1 (double-array number-inputs))
  (reset! layer-2 (double-array number-inputs))
  (reset! layer-3 (double-array number-inputs))
  (reset! layer-4 (double-array number-inputs))
  (reset! layer-5 (double-array number-inputs))
  (reset! layer-6 (double-array number-inputs))
  (reset! layer-7 (double-array number-inputs))

  (reset! reset-val (double-array 1))

  (reset! output (double-array number-outputs))

  (reset! reset (boolean-array number-outputs false))

  (reset! reset-counter (int-array number-outputs))

  (reset! number-of-categories (int-array number-outputs))

  (reset! weights-up (make-array Double/TYPE number-inputs number-outputs))
  (reset! weights-down (make-array Double/TYPE number-outputs number-inputs)))

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
        highest-5 (take 5 (highest-occuring-pattern input-and-categories-pair))]
    (println highest-5)
    (translate-to-pitches (map first highest-5))))

(defn compose []
  (let [pitch-groupings (run-neural-net @input-patterns)]
    (events/make pitch-groupings 0 800)))
