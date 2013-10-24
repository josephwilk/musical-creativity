(ns musical-creativity.composers.markov
  (:require [musical-creativity.events :as events]))

(load-file "data/bach.clj")

(def default-events
  [{:pitch 60 :time 0}
   {:pitch 62 :time 1000}
   {:pitch 64 :time 2000}
   {:pitch 65 :time 3000}
   {:pitch 67 :time 4000}
   {:pitch 65 :time 5000}
   {:pitch 69 :time 6000}
   {:pitch 71 :time 7000}
   {:pitch 72 :time 8000}
   {:pitch 60 :time 9000}])

(def defaults
  {:start [60]
   :events default-events
   :length 50
   :phrase-length 1
   :depth 1})

(defn pick-pitches-fn [depth stm]
  (fn [pitches]
    (let [last-chunk (take-last depth pitches)
          candidates (stm last-chunk)
          picked (rand-nth candidates)]
      (if picked
        (concat pitches picked)
        pitches))))

(defn- compose-pitches [start length depth stm]
  (nth (iterate (pick-pitches-fn depth stm) start) length))

(defn probabilities-for [depth stm chunk]
  (let [prefix  (take depth chunk)
        suffix  (drop depth chunk)
        stm-row (or (stm prefix) [])]
    (assoc stm prefix (conj stm-row suffix))))

(defn- state-transition-matrix-probabilities
  ([pitches depth phrase-length]
     (state-transition-matrix-probabilities {} pitches depth phrase-length))
  ([seed-stm pitches depth phrase-length]
     (let [pitch-chunks (partition (+ depth phrase-length) 1 pitches)]
       (reduce (partial probabilities-for depth) seed-stm pitch-chunks)))
  ([seed-stm pitches depth phrase-length phrase-overlap]
     (let [pitch-chunks (partition (+ depth phrase-length) phrase-overlap pitches)]
       (reduce (partial probabilities-for depth) seed-stm pitch-chunks))))

(defn- compose-markov [start length depth stm]
  (events/make (flatten (compose-pitches start length depth stm)) 0 350))

(defn compose
  ([] (compose (:events defaults) (:start defaults) (:phrase-length defaults) (:length defaults)))
  ([events start phrase-length length]
     (assert (zero? (mod length phrase-length))
             (format "length %s is no multiple of phrase-length %s" length phrase-length))
     (let [depth (count start)]
       (if (= (count start) depth)
         (let [pitches (map :pitch events)
               stm (state-transition-matrix-probabilities pitches depth phrase-length)]
           (compose-markov start (/ length phrase-length) depth stm))
         (println (str "Error: Start sequence must be same as depth: start:" (count start) " depth:" depth))))))


(defn transition-source
  "wraps the whole configuration plumming and returns successive closures that accept additional parameters:
 prepare-fn: function used to achieve the voice separation
 depth: markov model order
 phrase-length: target phrase length used for mapping
 phrase-shift: when training the model, how far should the phrase window be shifted on each iteration."
  [prepare-fn depth phrase-length phrase-shift voice-index]
  (fn [raw-events dimension]
    (let [model (reduce
                 (fn [stm events]
                   (state-transition-matrix-probabilities stm
                                                          (map dimension events)
                                                          depth
                                                          phrase-length
                                                          phrase-shift))
                 {}
                 (if (= :all voice-index)
                   (prepare-fn raw-events)
                   [(nth (prepare-fn raw-events) voice-index)]))]
      (fn [start-seq length]
        ;; start-seq isn't used atm, instead, a random starting point
        ;; is chosen directly from the model
        (compose-pitches (rand-nth (keys model))
                         (quot length phrase-length)
                         depth
                         model)))))

(defn constant-source
  "acts like a transition source, but will always return a constant value. useful for debugging."
  [const-value]
  (fn [raw-events dimension]
    (fn [start-seq length]
      (repeatedly length (constantly const-value)))))

(defn- harmonics-pairs [pitches]
  (for [index (range 1 (count pitches))]
    {(set (take index pitches)) [(first (drop index pitches))]}))

(defn harmonic-source
  "similar to transition-source, but will yield a model that, given a pitch from the lead-voice and a desired number of harmonics will generate dependent harmonic pitches observed in the source material.
  - prepare-fn: function to extract voices from the raw-material
  - lead-voice-index: index of the voice that is to be considered the lead
  - harmony-index: seq of indexes that are to be used (in this order) as the harmonics."
  [prepare-fn lead-voice-index harmony-indexes]
  (fn [raw-events]
    (let [voices            (map #(map :pitch %) (prepare-fn raw-events))
          harmonics-count   (count harmony-indexes)
          harmony-voices    (map #(nth voices %) harmony-indexes)
          harmonic-mappings (reduce (fn [harmonics pitches]
                                      (map #(merge-with concat %1 %2)
                                           harmonics
                                           (harmonics-pairs pitches)))
                                    (repeatedly harmonics-count hash-map)
                                    (apply map list (cons (nth voices lead-voice-index)
                                                          harmony-voices)))]
      (fn [lead-pitch voice-count-to-gen]
        (reduce #(if-let [harmony (rand-nth (get %2 %1))]
                   (conj %1 harmony)
                   %1)
                #{lead-pitch}
                (take voice-count-to-gen harmonic-mappings))))))


(defn- compose-markov-multi [start-seq length models]
  (let [{:keys [pitch duration velocity]} models]
    (events/make-from-maps (map (fn [p d v]
                                  {:pitch p
                                   :duration d
                                   :velocity v})
                                (flatten (pitch    start-seq length))
                                (flatten (duration start-seq length))
                                (flatten (velocity start-seq length)))
                           0)))

(defn compose-multi-model
  "generate a composition based on multiple markov models for different aspects of the composition.
   - raw-events: the events used for composition (ie the source piece)
   - length: desired output length. (can't be guaranteed)
   - dimension-model-configs: a mapping from dimension (:pitch, :duration, :velocity, :harmonics, :voices) to the transition-source/harmonic-source/constant-source."
  [raw-events length & dimension-model-configs]
  (let [processors         (into {} (map vec (partition 2 dimension-model-configs)))
        harmonizer         ((get processors :harmonics) raw-events)
        voices             ((get processors :voices) raw-events count)
        param-gens         (->> (select-keys processors
                                             [:pitch :duration :velocity])
                                (map (fn [[dimension f]]
                                       [dimension (f raw-events dimension)]))
                                (into {}))
        start-seq         [(rand-nth raw-events)]]
    (mapcat (fn [e v] (map #(assoc e :pitch %) (harmonizer (get e :pitch) v)))
            (compose-markov-multi start-seq length param-gens)
            (voices start-seq length))))
