(ns computer-models-of-musical-creativity.markov)

(def events
  [[0 60 1000 1 127]
   [0 60 1000 1 127]
   [1000 62 1000 1 127]
   [2000 64 1000 1 127]
   [3000 65 1000 1 127]
   [4000 67 1000 1 127]
   [5000 65 1000 1 127]
   [6000 69 1000 1 127]
   [7000 71 1000 1 127]
   [8000 72 1000 1 127]
   [9000 60 1000 1 127]])


(def empty-state-transition-matrix
  {60 []
   62 []
   64 []
   65 []
   67 []
   69 []
   71 []
   72 []})

(def state-transition-matrix (atom empty-state-transition-matrix))

(def rs [make-random-state t])

(defn compose-new-music-based-on-markovian-probabilities [start length events]
  (clear-matrix!)
  (reset! state-transition-matrix (put-probabilities-into-state-transition-matrix (get-pitches events) @state-transition-matrix))
  (compose-markov start length))

(defn put-probabilities-into-state-transition-matrix [pitches state-transition-matrix]
  (if (nil? (rest pitches)) state-transition-matrix
      (do
        (reset! state-transition-matrix (put-probabilities (firstn 2 pitches) @state-transition-matrix))
        (put-probabilities-into-state-transition-matrix (rest pitches) @state-transition-matrix))))

(defn put-probabilities [two-pitches state-transition-matrix]
  (cond
   (nil? state-transition-matrix) []

   (equal (first two-pitches) (first (first state-transition-matrix)))
   (cons (list (first (first state-transition-matrix))
               (cons (second two-pitches) (second (first state-transition-matrix))))
         (put-probabilities two-pitches (rest state-transition-matrix)))

   :else
   (t (cons (first state-transition-matrix)
            (put-probabilities two-pitches (rest state-transition-matrix))))))


(defn compose-markov [start length]
  (make-events (compose-m start length)))

(defn compose-m [start length]
  (if (= length 0) []
      (let [test (choose-one (second (assoc start @state-transition-matrix)))]
        (cons test
              (compose-m test (- length 1))))))

(defn make-events [list-of-pitches & [time]]
  (if (nil? list-of-pitches) []
      (cons [(or time 0)
             (first list-of-pitches)
             250
             1
             127]
            (make-events (rest list-of-pitches) (+ time 250)))))

(defn choose-one [list]
  (nth (random (length list) *rs*)
       list))

(defn firstn [number list]
  (if (< (length list) number) (firstn (1- number) list)
      (butlast list (- (length list) number))))

(defn get-pitches [events]
 (if (nil? events) []
     (cons (second (first events))
           (get-pitches (rest events)))))

(defn clear-matrix! []
  (reset! state-transition-matrix empty-state-transition-matrix))
