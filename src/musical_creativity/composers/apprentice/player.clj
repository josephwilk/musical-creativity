(ns musical-creativity.composers.apprentice.player
  (:require
   [overtone.live :as overtone]
   [overtone.synth.sampled-piano :as piano]
   [overtone.music.pitch :as pitch]

   [musical-creativity.musician :as musician]
   [musical-creativity.instruments :refer :all]
   [musical-creativity.composers.sonify-words :as sonify]))

(defn- explode [thing] (map str (vec (str thing))))

(defn play-grouped [sentence]
  (when (seq sentence)
    (doall (map (fn [group]
                  (doseq [note (vec (str group))]
                    (when-not (or (= (str note) "?") (= (str note) "!"))
                      (piano/sampled-piano {:note (pitch/note (str note "4"))})))
                  (Thread/sleep 400)) (first sentence)))))

(defn play-all [sentence]
  (let [sentence (remove #(or (= % "!") (= % "?") (nil? %)) (flatten (map explode (flatten sentence))))
        sentence (map (fn [x] (str x "4")) sentence)]
    (doseq [note sentence]
      (piano/sampled-piano {:note (pitch/note note)}) (Thread/sleep 300))))

(defn play-sonified [sentence]
  (let [sentence (flatten sentence)]
    (when (seq sentence)
      (println :sentence sentence)
      (musician/play (sonify/compose (mapcat (fn [word] (str word)) sentence)) piano))))