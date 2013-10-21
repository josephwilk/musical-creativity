(ns musical-creativity.analyzer)

(defn group-by-time
  "takes a sequence of events and groups them by time in chronological fashion: returns a sequence of vectors, each of which contains the pitches played simultaneously. this is a rough approximation of the underlying chord progression."
  [events]
  (->> events
       (group-by :time)
       (sort-by first)
       (map last)))


(defn extract-voices-random
  "takes a sequence of grouped events and returns a sequence of voices extracted from the groups. determines the number of voices from the maximum number of elements in the groups. returns a sequence of voices (each a sequence of events) for further processing.

this is only a very random approach to the concept of voice: it chooses a random member of the group and assigns it to the current voice. the last element of a group will be retained until all other groups are down to one element."
  [grouped-events]
  (loop [acc      []
         material (map set grouped-events)]
    (if (every? #(= 1 %) (map count material))
      (conj acc (map first material))
      (let [picked (map (comp rand-nth vec) material)]
        (recur (conj acc picked)
               (map #(if (= 1 (count %1)) %1 (disj %1 %2))
                    material
                    picked))))))


(defn- choose-min-pitch-delta [picks cands]
  (if-let [last-pitch (:pitch (last picks))]
    (first (sort-by #(Math/abs (- last-pitch (:pitch %))) cands))
    (rand-nth (vec cands))))

(defn extract-voices-min-distance
  "takes a sequence of grouped events and returns a sequence of voices extracted from the groups. determines the number of voices from the maximum number of simultaneous events in the groups. in order to assign events to the voices, the (again, rather naive) assumption is, that the pitch distances between events will be smaller within a voice than between voices, ie given the last pitch in the voice, it will pick the pitch with the smallest delta from the current group."
  [grouped-events]
  (loop [acc      []
         material (map set grouped-events)]
    (if (every? #(= 1 %) (map count material))
      (conj acc (map first material))
      (let [picked (reduce #(conj %1 (choose-min-pitch-delta %1 %2)) [] material)]
        (recur (conj acc picked)
               (map #(if (= 1 (count %1)) %1 (disj %1 %2))
                    material
                    picked))))))
