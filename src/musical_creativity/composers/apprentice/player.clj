(ns musical-creativity.composers.apprentice.player
  (:require
   [overtone.live :as overtone]
   [overtone.synth.sampled-piano :as piano]
   [overtone.music.pitch :as pitch]))

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
