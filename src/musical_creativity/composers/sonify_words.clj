(ns musical-creativity.composers.sonify-words
  (:require
    [musical-creativity.events :as events]))

(def transpose 10)
(def numbers [50, 53, 56, 59, 62, 65, 68, 71, 74, 77])
(def punctuation-numbers [46, 44, 39, 33, 63, 40, 41, 91, 93, 123, 125, 35, 61, 95, 60, 62, 34, 45, 43, 47, 92])

(defn find-pitch [character-code]
  (cond
    (and (>= character-code 65) (<= character-code 90))
    (+ character-code transpose)
  
    (and (>= character-code 97) (<= character-code 122))
    (+ transpose (- character-code 47))
  
    (= character-code 32)
    32
    
    (and (>= character-code 48) (<= character-code 57))
    (+ transpose (nth character-code numbers))
    
    (some #{character-code} punctuation-numbers)
    (60)))

(defn sonify-character [character]
  (let [character-code (int character)
        pitch (find-pitch character-code)]
    {:pitch pitch}))

(defn sonify [string]
  (map sonify-character string))

(defn compose [words]
  (events/make (sonify words)))