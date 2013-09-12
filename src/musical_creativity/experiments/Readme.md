## Analogies and Inference

```clojure
(use 'overtone.live)
(use 'overtone.synth.sampled-piano)
(use 'overtone.music.pitch)

(reset! fact-store '((g-b-d*c-e-g are dominant*tonic)
                     (dominant*tonic are resolutions)
                     (f-a-c*c-e-g are subdominant*tonic)
                     (subdominant*tonic are resolutions)))

(analogy 'g-b-d*c-e-g)

(map #(do (sampled-piano :note (overtone.music.pitch/note %)) (Thread/sleep 400)) [:g4 :b4 :d4])
(map #(do (sampled-piano :note (overtone.music.pitch/note %)) (Thread/sleep 400)) [:c4 :e4 :g4])

(map #(do (sampled-piano :note (overtone.music.pitch/note %)) (Thread/sleep 400)) [:f4 :a4 :c4])
(map #(do (sampled-piano :note (overtone.music.pitch/note %)) (Thread/sleep 400)) [:c4 :e4 :g4])

(map #(do (sampled-piano :note (overtone.music.pitch/note %)) (Thread/sleep 400)) [:g4 :e4 :g4 :f4 :a4 :c4])
```
