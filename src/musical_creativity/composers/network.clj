(ns musical-creativity.composers.network)

(def number-of-outputs 5)
(def number-of-inputs 5)
(def input-patterns '((0.0 0.2 0.1 0.25 0.35) (0.0 0.1 0.2 0.25 0.35) (0.0 0.2 0.25 0.35 0.45) 
                            (0.1 0.2 0.35 0.25 0.35) (0.2 0.1 0.2 0.25 0.45) (0.45 0.1 0.2 0.25 0.35) (0.2 0.2 0.1 0.25 0.35)
                            (0.35 0.25 0.2 0.25 0.35) (0.35 0.2 0.1 0.25 0.2) (0.1 0.25 0.2 0.25 0.35) 
                            (0.0 0.1 0.2 0.25 0.2) (0.25 0.2 0.1 0.2 0.25) (0.45 0.35 0.25 0.2 0.25) (0.0 0.1 0.2 0.25 0.2)
                            (0.0 0.0 0.1 0.2 0.25)))
(def array-1 (make-array (list number-of-inputs)))
(def array-2 (make-array (list number-of-inputs)))
(def array-3 (make-array (list number-of-inputs)))
(def array-4 (make-array (list number-of-inputs)))
(def array-5 (make-array (list number-of-inputs)))
(def array-6 (make-array (list number-of-inputs)))
(def array-7 (make-array (list number-of-inputs)))
(def array-8 (make-array (list number-of-outputs)))

(def resetval (make-array (list 1)))
(def y (make-array (list number-of-outputs)))
(def reset (make-array (list number-of-outputs)))
(def reset-counter (make-array (list number-of-outputs)))
(def number-of-categories (make-array (list number-of-outputs)))

(def wup (make-array (list number-of-inputs number-of-outputs)))
(def wdown (make-array (list number-of-inputs number-of-outputs)))
(def *learned-categories* ())
(def learning-cycle-counter 0)
(def maximum-index ())
(def skipreset ())
(def input (make-array (list number-of-inputs)))
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

(defn run-neural-net []
  "assumes patterns are in decimal form."
  (initialize-network 5 5)
  (initialize-the-network)
  (learn-the-patterns 50)
  (translate-into-events
   (translate-to-pitches (mapcar #'first 
                                      (firstn 5 
                                              (count-highest 
                                               (pair input-patterns (mapcar #'second *learned-categories*))))))))

(defn initialize-network (numinputs numoutputs &optional trainingpatterns)
  "initializes the neural network."
  ; check for specified training patterns:
  (if trainingpatterns
    ; make sure the number of input neurons agrees with
    ; the size of the training patterns:
    (if (equal (length (car trainingpatterns)) numinputs)
      (reset! input-patterns trainingpatterns)
      (print
       (list
        "error: bad input to initialize-network. numinputs should have been"
        (length (car trainingpatterns)))))
    ; no specified training patterns: use the default set
    ; defined in this package:
    (if (not (equal (length (car input-patterns)) numinputs))
      (print
       (list
        "error: bad input to initialize-network. numinputs should have been"
        (length (car input-patterns))))
      ; specified number of input neurons agrees with
      ; the size of the default training patterns defined
      ; in this package; proceed with defining network data:
      (progn
        ; resets the network
        ; define the network size:
        (reset! number-of-inputs numinputs)
        (reset! number-of-outputs numoutputs)
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
        (reset! *learned-categories* nil)))))

(defn floating-point-random (low high)
  "floating point random numbers."
  (let ((range (- high low)))
    (+ (* (/ (random 1000) 1000.0) range) low)))

(defn find-the-largest-output (&aux (maximum-index 0) (mval (aref array-8 0)))
  "finds the largest output."
  (loop for output-number-index from 0 to (1- number-of-outputs)
    do (if (and
         (> (aref array-8 output-number-index) mval)
         (not (aref reset output-number-index)))
      (reset! mval (aref array-8 output-number-index)
            maximum-index output-number-index)))
  maximum-index)

(defn check-array-value (index &aux mval (maximum-index (find-the-largest-output)))
  "returns d if (aref y index) is the largest value in array array-8 
   and (aref array-8 index) has not been reset."
  mval
  (if (and
       (equal index maximum-index)
       (not (aref reset maximum-index))
       (> (aref array-8 maximum-index) reset-threshold))
    d
    0.0))

(defn sigmoid-threshold-function (test)
  "threshold function."
  (if (> test theta)
    test
    0.0))

(defn vector-l2-norm (vector vector-length &aux (sum 0.0))
  "l2 norm of a vector."
  (loop for length-index from 0 to (1- vector-length)
        do (reset! sum (+ sum (* (aref vector length-index) (aref vector length-index)))))
  (+ (sqrt sum) 0.001))

(defn update-f1-stm-arrays (&aux sum norm max1 max2)
  "update f1 stm arrays."
  ; calculate array-7 from array-5 input and backwards feed back:
  (loop for input-number-index from 0 to (1- number-of-inputs)
        do (progn
             (reset! sum 0.0)
             (loop for output-number-index from 0 to (1- number-of-outputs)
                   do (reset! sum (+ sum (* (check-array-value output-number-index) (aref wdown output-number-index input-number-index)))))
             (setf (aref array-7 input-number-index) (+ (aref array-5 input-number-index) sum))))
  ; update array-6 using eq. 5
  (reset! norm (+ (vector-l2-norm array-7 number-of-inputs) e))
  (loop for input-number-index from 0 to (1- number-of-inputs)
        do  (setf (aref array-6 input-number-index) (/ (aref array-7 input-number-index) norm)))
  ; update array-5 using eq. 6:
  (reset! norm (vector-l2-norm array-3 number-of-inputs))
  (loop for input-number-index from 0 to (1- number-of-inputs)
        do (setf (aref array-5 input-number-index) (/ (aref array-3 input-number-index) norm)))
  ; update array-3 using eq. 7:
  (loop for input-number-index from 0 to (1- number-of-inputs)
        do (setf (aref array-3 input-number-index) (sigmoid-threshold-function (+ (aref array-2 input-number-index) (* b (sigmoid-threshold-function (aref array-6 input-number-index)))))))
  ; update w using eq. 8:
  (loop for input-number-index from 0 to (1- number-of-inputs)
        do (setf (aref array-2 input-number-index) (/ (aref array-1 input-number-index) norm)))
  ; update array-2 using eq. 9:
  (reset! norm (+ (vector-l2-norm array-1 number-of-inputs) e))
  (loop for input-number-index from 0 to (1- number-of-inputs)
        do (setf (aref array-2 input-number-index) (/ (aref array-1 input-number-index) norm)))
  ; calculate reset array-4 from eq. 20:
  (reset! max1 -1000.0 max2 -1000.0)
  (loop for input-number-index from 0 to (1- number-of-inputs)
        do (progn
             (if (< max1 (aref array-5 input-number-index)) (reset! max1 (aref array-5 input-number-index)))
             (if (< max2 (aref array-7 input-number-index)) (reset! max2 (aref array-7 input-number-index)))))
  (reset! max1 (+ max1 0.001))
  (reset! max2 (+ max2 0.001))
  (loop for input-number-index from 0 to (1- number-of-inputs)
        do (setf
            (aref array-4 input-number-index)
            (- (/ (aref array-5 input-number-index) max1) (/ (aref array-7 input-number-index) max2)))))

(defn update-f2-stm-storage (&aux sum)
  "updates f2 stm storage."
  (loop for output-number-index from 0 to (1- number-of-outputs)
        do (progn
             (reset! sum 0.0)
             (loop for input-number-index from 0 to (1- number-of-inputs)
                   do (reset! sum (+ sum (* (aref array-7 input-number-index) (aref wup input-number-index output-number-index)))))
             (setf (aref array-8 output-number-index) sum)
             (if (aref reset output-number-index) (setf (aref array-8 output-number-index) -0.1)))))

(defn update-weights (&aux (largest-output (find-the-largest-output)))
  "updates the weights."
  (if (> (check-array-value largest-output) 0.02)
    (loop for increment from 0 to (1- number-of-inputs)
      do (setf
          (aref wdown largest-output increment)
          (+ (aref wdown largest-output increment)
             (*
              downlr
              d
              (- (aref array-7 increment) (aref wdown largest-output increment)))))
      do (setf
          (aref wup increment largest-output)
          (+
           (aref wup increment largest-output)
           (*
            uplr
            d
            (- (aref array-7 increment) (aref wup increment largest-output))))))))

(defn competitive-learning-at-f2 (&aux (largest-output (find-the-largest-output)))
  "competitive learning at slab f2."
  (if (> (aref array-8 largest-output) reset-threshold)
    (loop for output-number-index from 0 to (1- number-of-outputs)
          do (if (not (equal output-number-index largest-output))
               (setf (aref array-8 output-number-index) 0.0)))))

(defn run-one-full-cycle ()
  "run one full cycle."
  (update-f1-stm-arrays)
  (check-for-f2-reset)
  (competitive-learning-at-f2)
  (update-f2-stm-storage)
  (update-weights)
  (competitive-learning-at-f2))

(defn check-for-f2-reset (&aux (res 0.0)(n1 (+ (vector-l2-norm array-7 number-of-inputs) e)))
  "check for an f2 reset condition."
  (if (and
       (> n1 0.2)
       (not skipreset))
    (if (> learning-cycle-counter 1)
      (if (> (aref array-8 (find-the-largest-output)) 0.25)
        (reset! res (* 3.0 (vector-l2-norm array-4 number-of-inputs))))  ; was 3.0
      (reset! skipreset nil)))
  (setf (aref resetval 0) res)
  (if (> res (- 1.9 vigilance))  ;; 11/14/91 change
    (progn
      (print (list "vigilance reset =" res "  learning cycle ="
                   learning-cycle-counter))
      (reset! maximum-index (find-the-largest-output))
      (setf (aref reset maximum-index) 1)
      (setf (aref reset-counter maximum-index) 80))
    (loop for output-number-index from 0 to (1- number-of-outputs)
          do (setf (aref reset-counter output-number-index) (- (aref reset-counter output-number-index) 1))
          do (if (< (aref reset-counter output-number-index) 0)
               (progn
                 (if (aref reset output-number-index)  (reset! skipreset t))
                 (setf (aref reset output-number-index) nil)))))
  (reset! skipreset nil))

(defn zero-activations ()
  "zero activations."
  (loop for input-number-index from 0 to (1- number-of-inputs)
        do (do (setf (aref array-1 input-number-index) 0.0)
                  (setf (aref array-2 input-number-index) 0.0)
                  (setf (aref array-3 input-number-index) 0.0)
                  (setf (aref array-4 input-number-index) 0.0)
                  (setf (aref array-5 input-number-index) 0.0)
                  (setf (aref array-6 input-number-index) 0.0)
                  (setf (aref array-7 input-number-index) 0.0)))
  (loop for output-number-index from 0 to (1- number-of-outputs)
        do (do (setf (aref array-8 output-number-index) 0)
                  (setf (aref reset output-number-index) 0)
                  (setf (aref reset-counter output-number-index) 0))))

(defn set-learning-pattern (input-pattern &aux (length (length input-pattern)))
  "sets up a learning pattern in the input neurons."
  (if (not (equal length number-of-inputs))
    (print (list "error in set-learning-pattern input:" input-pattern))
    (progn
      (reset! learning-cycle-counter 0)
      (zero-activations)
      (loop for number from 0 to (1- length)
            do (setf (aref input number) (+ (pop input-pattern) (floating-point-random -0.08 0.08)))))))

(defn initialize-the-network ()
  "initialize the network."
  (zero-activations)
  (loop for output-number-index from 0 to (1- number-of-outputs)
      do (progn
           (loop for input-number-index from 0 to (1- number-of-inputs)
                 do (setf
                     (aref wup input-number-index output-number-index) (floating-point-random 0.05 0.1)
                     (aref wdown output-number-index input-number-index) (floating-point-random 0.01 0.03)))
           (setf (aref number-of-categories output-number-index) 0))))

(defn learn-the-patterns (number)
  "cycles through all training patterns once."
  (loop for input-pattern in input-patterns
    do (set-learning-pattern input-pattern)
    do (dotimes (n number)
         (reset! learning-cycle-counter (1+ learning-cycle-counter))
         (run-one-full-cycle))
    do (reset! *learned-categories*
          (cons (list array-7 (find-the-largest-output))
                *learned-categories*))))

(defn pair (one two)
  "pairs the two args together."
  (if (or (null one)(null two))()
      (cons (list (first one)(first two))
            (pair (rest one)(rest two)))))

(defn make-note-decimals (note-patterns)
  "transforms note patterns into decimal patterns."
  (if (null note-patterns)()
      (cons (mapcar #'(lambda (x)(* x .01)) (first note-patterns))
            (make-note-decimals (rest note-patterns)))))

(defn firstn (number list)
  "returns the first n of list."
 (if (< (length list) number)(firstn (1- number) list)
     (butlast list (- (length list) number))))

(defn translate-to-pitches (decimal-lists)
  "transforms decimal patterns into note patterns."
  (if (null decimal-lists)()
      (cons (translate-pitches (first decimal-lists))
            (translate-to-pitches (rest decimal-lists)))))

(defn translate-pitches (decimal-numbers)
  "helps transform decimal patterns into note patterns."
  (if (null decimal-numbers)()
      (let ((test (nth (position (first decimal-numbers) decimals) pitches)))
        (cons (if (null test) .69 test)
            (translate-pitches (rest decimal-numbers))))))

(defn translate-to-decimals (pitch-lists)
  "changes lists of pitches into lists of 0.0 - 1.0 range decimals."
    (if (null pitch-lists)()
      (cons (translate-decimals (first pitch-lists))
            (translate-to-decimals (rest pitch-lists)))))

(defn translate-decimals (pitch-numbers)
  "changes pitches into 0.0 - 1.0 range decimals."
  (if (null pitch-numbers)()
      (cons (nth (position (first pitch-numbers) pitches) decimals)
            (translate-decimals (rest pitch-numbers)))))

(defn translate-into-events (output-pitch-lists)
  "returns sontiguous events from its pitch-lists arg."
  (make-events (apply 'append output-pitch-lists)))

(defn make-events (pitch-groupings &optional (ontime 0))
  "makes consecutive events out of the pairs of pitches in its arg."
  (if (null pitch-groupings) ()
      (append (list (make-event ontime (first pitch-groupings) 1))
              (make-events (rest pitch-groupings)(+ ontime 1000)))))

(defn make-event (ontime pitch channel)
  "creates an event based on args."
  (list ontime
        (if (symbolp pitch) (eval pitch) pitch)
        1000
        channel
        90))

(defn count-highest (lists)
  "returns the highest occuring pattern in its arg."
  (let* ((sorted-numbers (my-sort #'< (mapcar #'second lists))) ;;;(1 1 2 2 2 3 3 3 )
         (numbers-only (remove-duplicates sorted-numbers))      ;;;(1 2 3)
         (counts (count-them numbers-only sorted-numbers)))     ;;;(5 2 3)
    (find-all (nth (position (first (my-sort #'> counts)) counts) numbers-only)  lists)))

(defn count-them (singles numbers)
  "returns the counts of its first arg in its second arg."
  (if (null singles)()
      (cons (count (first singles) numbers)
            (count-them (rest singles) numbers))))
   
(defn find-all (number lists)
  "returns all of the patterns associated by cdr with number."
  (cond ((null lists)())
        ((equal (second (first lists)) number)
         (cons (first lists)
               (find-all number (rest lists))))
        (t (find-all number (rest lists)))))
      
(defn my-sort (function lists)
  "non-destructive sort function."
  (loop for item in (sort (loop for array-2 in lists
                                collect (list array-2))  function :key #'car)
        collect (first item)))