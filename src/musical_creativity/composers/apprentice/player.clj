(ns musical-creativity.composers.apprentice.player
  (:require
   [overtone.live :as overtone]
   [overtone.synth.sampled-piano :as piano]
   [overtone.music.pitch :as pitch]))

(defn- explode [thing] (map str (vec (str thing))))

(defn play-sentence-notes [sentence]
  (let [sentence (remove #(or (= % "!") (= % "?") (nil? %)) (flatten (map explode (flatten sentence))))
        sentence (map (fn [x] (str x "4")) sentence)]
    (println sentence)

    (doseq [note sentence]
      (piano/sampled-piano :note (pitch/note note)) (Thread/sleep 300))))
