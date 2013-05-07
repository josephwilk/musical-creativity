(ns musical-creativity.composers.network
  (:require
   [clojure.math.numeric-tower :as math]))

(def number-of-outputs 5)
(def number-of-inputs 5)
(def input-patterns '((0.0 0.2 0.1 0.25 0.35) (0.0 0.1 0.2 0.25 0.35) (0.0 0.2 0.25 0.35 0.45)
                            (0.1 0.2 0.35 0.25 0.35) (0.2 0.1 0.2 0.25 0.45) (0.45 0.1 0.2 0.25 0.35) (0.2 0.2 0.1 0.25 0.35)
                            (0.35 0.25 0.2 0.25 0.35) (0.35 0.2 0.1 0.25 0.2) (0.1 0.25 0.2 0.25 0.35)
                            (0.0 0.1 0.2 0.25 0.2) (0.25 0.2 0.1 0.2 0.25) (0.45 0.35 0.25 0.2 0.25) (0.0 0.1 0.2 0.25 0.2)
                            (0.0 0.0 0.1 0.2 0.25)))

(def array-1 (atom (make-array Double/TYPE number-of-inputs)))
(def array-2 (atom (make-array Double/TYPE number-of-inputs)))
(def array-3 (atom (make-array Double/TYPE number-of-inputs)))
(def array-4 (atom (make-array Double/TYPE number-of-inputs)))
(def array-5 (atom (make-array Double/TYPE number-of-inputs)))
(def array-6 (atom (make-array Double/TYPE number-of-inputs)))
(def array-7 (atom (make-array Double/TYPE number-of-inputs)))
(def array-8 (atom (make-array Double/TYPE number-of-outputs)))

(def resetval (make-array Double/TYPE 1))
(def y (make-array Double/TYPE number-of-outputs))
(def reset (make-array Double/TYPE number-of-outputs))
(def reset-counter (make-array Double/TYPE number-of-outputs))
(def number-of-categories (make-array Double/TYPE number-of-outputs))

(def wup (make-array Double/TYPE number-of-inputs number-of-outputs))
(def wdown (make-array Double/TYPE number-of-inputs number-of-outputs))
(def *learned-categories* ())
(def learning-cycle-counter 0)
(def maximum-index ())
(def skipreset ())
(def input (make-array Double/TYPE number-of-inputs))
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

(defn initialize-network
  "initializes the neural network."
  ([number-inputs number-outputs] (initialize-network number-inputs number-outputs nil))
  ([number-inputs number-outputs &optional training-patterns]
                                        ; check for specified training patterns:
      (if training-patterns
                                        ; make sure the number of input neurons agrees with
                                        ; the size of the training patterns:
        (if (= (count (first training-patterns)) number-inputs)
          (reset! input-patterns training-patterns)
          (print
           (list
            "error: bad input to initialize-network. number-inputs should have been"
            (count (first training-patterns)))))
                                        ; no specified training patterns: use the default set
                                        ; defined in this package:
        (if (not (= (count (first input-patterns)) number-inputs))
          (print
           (list
            "error: bad input to initialize-network. number-inputs should have been"
            (count (first input-patterns))))
                                        ; specified number of input neurons agrees with
                                        ; the size of the default training patterns defined
                                        ; in this package; proceed with defining network data:
          (do
                                        ; resets the network
                                        ; define the network size:
            (reset! number-of-inputs number-inputs)
            (reset! number-of-outputs number-outputs)
                                        ; array storage allocation:
           (reset! input (make-array (list number-of-inputs)))
           (reset! array-1 (make-array (list number-of-inputs)))
           (reset! array-2 (make-array (list number-of-inputs)))
           (reset! array-3 (make-array (list number-of-inputs)))
           (reset! array-4 (make-array (list number-of-inputs)))
           (reset! array-5 (make-array (list number-of-inputs)))
           (reset! array-6 (make-array (list number-of-inputs)))
           (reset! array-7 (make-array (list number-of-inputs)))
           (reset! resetval (make-array (list 1)))
           (reset! array-8 (make-array (list number-of-outputs)))
           (reset! reset (make-array (list number-of-outputs)))
           (reset! reset-counter (make-array (list number-of-outputs)))
           (reset! number-of-categories (make-array (list number-of-outputs)))
           (reset! wup (make-array (list number-of-inputs number-of-outputs)))
           (reset! wdown (make-array (list number-of-outputs number-of-inputs)))
                                        ; global variable to remember input patterns and
                                        ; their associated output category code for plotting
                                        ; by function art2-postprocess:
           (reset! *learned-categories* nil))))))

(defn floating-point-random
  "floating point random numbers."
  [low high]
  (let [range (- high low)]
    (+ (* (/ (rand-int 1000) 1000.0) range) low)))

(defn find-the-largest-output
  "finds the largest output."
  [array]
  (let [array-with-indexes (map-indexed vector array)]
    (first (reduce (fn [[position item] [max-pos max-value]]
                     (if (and
                          (> item max-value)
                          (not (nth reset position)))
                       [position item]
                       [max-pos max-value])) [nil nil] array-with-indexes))))

(defn check-array-value
  "returns d if (aref y index) is the largest value in array array-8 and (aref array-8 index) has not been reset."
  [index]
  (let [maximum-index (find-the-largest-output array-8)]
    (if (and
         (= index maximum-index)
         (not (aget reset maximum-index))
         (> (aget array-8 maximum-index) reset-threshold))
      d
      0.0)))

(defn sigmoid-threshold-function [test]
  "threshold function."
  (if (> test theta)
    test
    0.0))

(defn vector-l2-norm
  "l2 norm of a vector."
  [vector vector-length]
  (let [total-sum
        (loop [sum 0.0
               length-index 0]
          (if (>= length-index vector-length)
            sum
            (recur (+ sum (* (aget vector length-index) (aget vector length-index))) (+ vector-length 1) )))]
    (+ (math/sqrt total-sum) 0.001)))


(defn- wdown-total-sum-fn [input-index]
  (fn [output-index sum]
    (+ sum
       (* (check-array-value output-index)
          (aget wdown output-index input-index)))))

(defn update-f1-stm-arrays [&aux sum norm max1 max2]
  "update f1 stm arrays."

  ; calculate array-7 from array-5 input and backwards feed back:
  (map (fn [input-index]
         (let [total-sum
               (reduce (wdown-total-sum-fn input-index) (range 0 (- number-of-outputs 1)))]
           (reset! array-7 (assoc @array-7 input-index (+ (aget @array-5 input-index) total-sum)))))
       (range 0 (- number-of-inputs 1)))

  ; update array-6 using eq. 5
  (reset! norm (+ (vector-l2-norm array-7 number-of-inputs) e))
  (loop for input-number-index from 0 to (- number-of-inputs 1)
        (setf (aget array-6 input-number-index) (/ (aget array-7 input-number-index) norm)))

  ; update array-5 using eq. 6:
  (reset! norm (vector-l2-norm array-3 number-of-inputs))
  (loop for input-number-index from 0 to (- number-of-inputs 1)
        (setf (aget array-5 input-number-index) (/ (aget array-3 input-number-index) norm)))

  ; update array-3 using eq. 7:
  (loop for input-number-index from 0 to (- number-of-inputs 1)
        (setf (aget array-3 input-number-index) (sigmoid-threshold-function (+ (aget array-2 input-number-index) (* b (sigmoid-threshold-function (aget array-6 input-number-index)))))))

  ; update w using eq. 8:
  (loop for input-number-index from 0 to (- number-of-inputs 1)
        (setf (aget array-2 input-number-index) (/ (aget array-1 input-number-index) norm)))

  ; update array-2 using eq. 9:
  (reset! norm (+ (vector-l2-norm array-1 number-of-inputs) e))
  (loop for input-number-index from 0 to (- number-of-inputs 1)
        (setf (aget array-2 input-number-index) (/ (aget array-1 input-number-index) norm)))

  ; calculate reset array-4 from eq. 20:
  (reset! max1 -1000.0 max2 -1000.0)
  (loop for input-number-index from 0 to (- number-of-inputs 1)
        (do
          (if (< max1 (aget array-5 input-number-index)) (reset! max1 (aget array-5 input-number-index)))
          (if (< max2 (aget array-7 input-number-index)) (reset! max2 (aget array-7 input-number-index)))))
  (reset! max1 (+ max1 0.001))
  (reset! max2 (+ max2 0.001))
  (loop for input-number-index from 0 to (- number-of-inputs 1)
        (setf
         (aget array-4 input-number-index)
         (- (/ (aget array-5 input-number-index) max1) (/ (aget array-7 input-number-index) max2)))))

(defn update-f2-stm-storage [&aux sum]
  "updates f2 stm storage."
  (loop for output-number-index from 0 to (- number-of-outputs 1)
        (do
          (reset! sum 0.0)
          (loop for input-number-index from 0 to (- number-of-inputs 1)
                (reset! sum (+ sum (* (aget array-7 input-number-index) (aget wup input-number-index output-number-index)))))
          (setf (aget array-8 output-number-index) sum)
          (if (aget reset output-number-index) (setf (aget array-8 output-number-index) -0.1)))))

(defn update-weights [&aux (largest-output (find-the-largest-output array-8))]
  "updates the weights."
  (if (> (check-array-value largest-output) 0.02)
    (loop for increment from 0 to (- number-of-inputs 1)
          (setf
           (aget wdown largest-output increment)
           (+ (aget wdown largest-output increment)
              (*
               downlr
               d
               (- (aget array-7 increment) (aget wdown largest-output increment)))))
          (setf
           (aget wup increment largest-output)
           (+
            (aget wup increment largest-output)
            (*
             uplr
             d
             (- (aget array-7 increment) (aget wup increment largest-output))))))))

(defn competitive-learning-at-f2 [&aux (largest-output (find-the-largest-output array-8))]
  "competitive learning at slab f2."
  (if (> (aget array-8 largest-output) reset-threshold)
    (loop for output-number-index from 0 to (- number-of-outputs 1)
          (if (not (= output-number-index largest-output))
            (setf (aget array-8 output-number-index) 0.0)))))

(defn run-one-full-cycle []
  "run one full cycle."
  (update-f1-stm-arrays)
  (check-for-f2-reset)
  (competitive-learning-at-f2)
  (update-f2-stm-storage)
  (update-weights)
  (competitive-learning-at-f2))

(defn check-for-f2-reset [&aux (res 0.0)(n1 (+ (vector-l2-norm array-7 number-of-inputs) e))]
  "check for an f2 reset condition."
  (if (and
       (> n1 0.2)
       (not skipreset))
    (if (> learning-cycle-counter 1)
      (if (> (aget array-8 (find-the-largest-output array-8)) 0.25)
        (reset! res (* 3.0 (vector-l2-norm array-4 number-of-inputs))))  ; was 3.0
      (reset! skipreset nil)))
  (setf (aget resetval 0) res)
  (if (> res (- 1.9 vigilance))  ;; 11/14/91 change
    (do
      (print (list "vigilance reset =" res "  learning cycle ="
                   learning-cycle-counter))
      (reset! maximum-index (find-the-largest-output array-8))
      (setf (aget reset maximum-index) 1)
      (setf (aget reset-counter maximum-index) 80))
    (loop for output-number-index from 0 to (- number-of-outputs 1)
          (setf (aget reset-counter output-number-index) (- (aget reset-counter output-number-index) 1))
          (if (< (aget reset-counter output-number-index) 0)
               (do
                 (if (aget reset output-number-index)  (reset! skipreset t))
                 (setf (aget reset output-number-index) nil)))))
  (reset! skipreset nil))

(defn zero-activations []
  "zero activations."
  (loop for input-number-index from 0 to (- number-of-inputs 1)
        (do (setf (aget array-1 input-number-index) 0.0)
                  (setf (aget array-2 input-number-index) 0.0)
                  (setf (aget array-3 input-number-index) 0.0)
                  (setf (aget array-4 input-number-index) 0.0)
                  (setf (aget array-5 input-number-index) 0.0)
                  (setf (aget array-6 input-number-index) 0.0)
                  (setf (aget array-7 input-number-index) 0.0)))
  (loop for output-number-index from 0 to (- number-of-outputs 1)
        (do (setf (aget array-8 output-number-index) 0)
                  (setf (aget reset output-number-index) 0)
                  (setf (aget reset-counter output-number-index) 0))))

(defn set-learning-pattern [input-pattern &aux (count (count input-pattern))]
  "sets up a learning pattern in the input neurons."
  (if (not (= length number-of-inputs))
    (print (list "error in set-learning-pattern input:" input-pattern))
    (do
      (reset! learning-cycle-counter 0)
      (zero-activations)
      (loop for number from 0 to (- length 1)
            (setf (aget input number) (+ (pop input-pattern) (floating-point-random -0.08 0.08)))))))

(defn initialize-the-network []
  "initialize the network."
  (zero-activations)
  (loop for output-number-index from 0 to (- number-of-outputs 1)
        (do
           (loop for input-number-index from 0 to (- number-of-inputs 1)
                 (setf
                  (aget wup input-number-index output-number-index) (floating-point-random 0.05 0.1)
                  (aget wdown output-number-index input-number-index) (floating-point-random 0.01 0.03)))
           (setf (aget number-of-categories output-number-index) 0))))

(defn learn-the-patterns [number]
  "cycles through all training patterns once."
  (loop for input-pattern in input-patterns
        (set-learning-pattern input-pattern)
        (dotimes (n number)
          (reset! learning-cycle-counter (+ 1 learning-cycle-counter))
          (run-one-full-cycle))
        (reset! *learned-categories*
                (cons (list array-7 (find-the-largest-output array-8))
                      *learned-categories*))))

(defn pair [one two]
  "pairs the two args together."
  (if (or (null one)(null two))()
      (cons (list (first one)(first two))
            (pair (rest one)(rest two)))))

(defn make-note-decimals [note-patterns]
  "transforms note patterns into decimal patterns."
  (if (null note-patterns)()
      (cons (map (fn [x] (* x .01)) (first note-patterns))
            (make-note-decimals (rest note-patterns)))))

(defn firstn [number list]
  "returns the first n of list."
 (if (< (count list) number)(firstn (- number 1) list)
     (butlast list (- (count list) number))))

(defn translate-to-pitches [decimal-lists]
  "transforms decimal patterns into note patterns."
  (if (null decimal-lists)()
      (cons (translate-pitches (first decimal-lists))
            (translate-to-pitches (rest decimal-lists)))))

(defn translate-pitches [decimal-numbers]
  "helps transform decimal patterns into note patterns."
  (if (null decimal-numbers)()
      (let ((test (nth (position (first decimal-numbers) decimals) pitches)))
        (cons (if (null test) .69 test)
            (translate-pitches (rest decimal-numbers))))))

(defn translate-to-decimals [pitch-lists]
  "changes lists of pitches into lists of 0.0 - 1.0 range decimals."
    (if (null pitch-lists)()
      (cons (translate-decimals (first pitch-lists))
            (translate-to-decimals (rest pitch-lists)))))

(defn translate-decimals [pitch-numbers]
  "changes pitches into 0.0 - 1.0 range decimals."
  (if (null pitch-numbers)()
      (cons (nth (position (first pitch-numbers) pitches) decimals)
            (translate-decimals (rest pitch-numbers)))))

(defn translate-into-events [output-pitch-lists]
  "returns sontiguous events from its pitch-lists arg."
  (make-events (apply 'append output-pitch-lists)))

(defn make-events [pitch-groupings &optional (ontime 0)]
  "makes consecutive events out of the pairs of pitches in its arg."
  (if (null pitch-groupings) ()
      (append (list (make-event ontime (first pitch-groupings) 1))
              (make-events (rest pitch-groupings)(+ ontime 1000)))))

(defn make-event [ontime pitch channel]
  "creates an event based on args."
  (list ontime
        (if (symbolp pitch) (eval pitch) pitch)
        1000
        channel
        90))

(defn count-highest [lists]
  "returns the highest occuring pattern in its arg."
  (let [sorted-numbers (my-sort #'< (mapcar #'second lists)) ;;;(1 1 2 2 2 3 3 3 )
        numbers-only (remove-duplicates sorted-numbers)      ;;;(1 2 3)
        counts (count-them numbers-only sorted-numbers)]     ;;;(5 2 3)
    (find-all (nth (position (first (my-sort #'> counts)) counts) numbers-only)  lists)))

(defn count-them [singles numbers]
  "returns the counts of its first arg in its second arg."
  (if (null singles)()
      (cons (count (first singles) numbers)
            (count-them (rest singles) numbers))))

(defn find-all [number lists]
  "returns all of the patterns associated by cdr with number."
  (cond (null lists)()
        (= (second (first lists)) number)
        (cons (first lists)
              (find-all number (rest lists)))
        :else (find-all number (rest lists))))

(defn my-sort [function lists]
  "non-destructive sort function."
  (loop for item in (sort (loop for array-2 in lists
                                collect (list array-2))  function :key #'car)
        collect (first item)))

(defn run-neural-net []
  "assumes patterns are in decimal form."
  (initialize-network 5 5)
  (initialize-the-network)
  (learn-the-patterns 50)
  (translate-into-events
   (translate-to-pitches (map first
                              (firstn 5
                                      (count-highest
                                       (pair input-patterns (map second *learned-categories*))))))))
