(defproject cljfreechart "0.1.0-SNAPSHOT"
  :description "A modern JFreeChart wrapper in Clojure."
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure  "1.9.0"]
                 [org.clojure/test.check "0.9.0"]
                 [org.jfree/jfreechart "1.5.0"]
                 [metosin/spec-tools "0.7.0"]
                 [joinr/incanter "1.9.3-SNAPSHOT" :exclude [org.jfree/jfreechart]]
                 ])
