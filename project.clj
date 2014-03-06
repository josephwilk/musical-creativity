(defproject musical-creativity "0.1.0-SNAPSHOT"
  :description "Examples from David Copes Computer Models of Musical Creativity book"
  :url "http://github.com/josephwilk/musical-creativity"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [overtone "0.9.1"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [cheshire "5.3.1"]
                 [clj-http "0.9.0"]
                 [quil "1.7.0"]
                 [aysylu/loom "0.4.2"]
                 [midi-mash "0.1.0-SNAPSHOT"]]
  :profiles {:dev {:dependencies [[midje "1.6.2"]]
                   :plugins [[lein-kibit "0.0.8"]
                             [lein-midje "3.1.3"]]}})
