#Computer models of musical creativity

[![Build Status](https://travis-ci.org/josephwilk/musical-creativity.png?branch=master)](https://travis-ci.org/josephwilk/musical-creativity)

Experiments with models for computers generating music.

##Example

```clojure
(require '[musical-creativity.musician :as musician])
(require '[musical-creativity.events :as events])
(require '[musical-creativity.composers.markov :as markov])

(musician/play (markov/compose) play-piano)
(musician/play (markov/compose) play-ping)

(require '[musical-creativity.composers.sonify-data :as sonify-data])
(musician/play (sonify-data/compose) play-piano)

(require '[musical-creativity.composers.sonify-words :as sonify-words])
(musician/play (sonify-words/compose "potato potato potata potatd potate potatk") musician/play-piano)
```

##Book

Based on exercises from:

![Computer Models of Musical Creativity](http://mitpress.mit.edu/covers/9780262033381.jpg)

Computer Models of Musical Creativity http://mitpress.mit.edu/books/computer-models-musical-creativity
