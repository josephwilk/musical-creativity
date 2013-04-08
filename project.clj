(defproject computer-models-of-musical-creativity "0.1.0-SNAPSHOT"
  :description "Examples from David Copes Computer Models of Musical Creativity book"
  :url "http://github.com/josephwilk/computer-models-of-musical-creativity"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [overtone "0.8.1"]
                 [org.clojure/math.numeric-tower "0.0.2"]]
                 :profiles {:dev {:dependencies [[midje "1.5.1"]
                                                 [jonase/kibit "0.0.3"]
                                                 [jonase/eastwood "0.0.2"]]
                                  :plugins [[lein-midje "3.0.0"]]}})
