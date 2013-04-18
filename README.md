#Computer models of musical creativity

[![Build Status](https://travis-ci.org/josephwilk/musical-creativity.png?branch=master)](https://travis-ci.org/josephwilk/musical-creativity)

Experiments with models for computers generating music.

##Example

```clojure
(require '[musical-creativity.musician :as musician])
(use '[musical-creativity.instruments])

;Markov chains
(require '[musical-creativity.composers.markov :as markov])
(musician/play (markov/compose) piano)
(musician/play (markov/compose) ping)

(musician/play (markov/compose {:events data.bach/bach1}) piano)

;Sonify data
(require '[musical-creativity.composers.sonify-data :as sonify-data])
(musician/play (sonify-data/compose) piano)

;Sonify text
(require '[musical-creativity.composers.sonify-words :as sonify-words])
(musician/play (sonify-words/compose "potato potato potata potatd potate potatk") piano)

;Cosine
(require '[musical-creativity.composers.cosine :as cosine])
(musician/play (cosine/compose) piano)

;Cellular automata
(require '[musical-creativity.composers.cellular-automata :as ca])
(musician/play (ca/compose) piano)
(musician/play (ca/compose ca/rule-22) piano)

;Minuet
(require '[musical-creativity.composers.minuet :as minuet])
(musician/play (minuet/compose) piano)

;Fuzzy
(require '[musical-creativity.composers.fuzzy :as fuzzy])
(musician/play (fuzzy/compose) piano)
```

##Book

Based on exercises from:

![Computer Models of Musical Creativity](http://mitpress.mit.edu/covers/9780262033381.jpg)

Computer Models of Musical Creativity http://mitpress.mit.edu/books/computer-models-musical-creativity
