(defproject musical-creativity "0.1.0-SNAPSHOT"
  :description "Examples from David Copes Computer Models of Musical Creativity book"
  :url "http://github.com/josephwilk/musical-creativity"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [overtone "0.8.1"]
                 [org.clojure/math.numeric-tower "0.0.2"]
                 [cheshire "5.2.0"]
                 [clj-http "0.7.6"]
                 [quil "1.6.0"]
                 [aysylu/loom "0.3.1"]
                 [midi-mash "0.1.0-SNAPSHOT"]]
  :profiles {:dev {:dependencies [[midje "1.5.1"]]
                   :plugins [[lein-kibit "0.0.8"]
                             [lein-midje "3.0.0"]]}})
