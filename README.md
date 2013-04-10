#Computer models of musical creativity

[![Build Status](https://travis-ci.org/josephwilk/musical-creativity.png?branch=master)](https://travis-ci.org/josephwilk/musical-creativity)

Models for computers generating music.

##Example

```clojure
(require '[musical-creativity.musician :as musician])
(require '[musical-creativity.composers.markov :as markov])

(musician/play (markov/compose))
```

##Book

Based on exercises from:

![Computer Models of Musical Creativity](http://mitpress.mit.edu/covers/9780262033381.jpg)

Computer Models of Musical Creativity http://mitpress.mit.edu/books/computer-models-musical-creativity
