(defproject hazel "0.1-SNAPSHOT"
  :description "Datomic support library."
  :license {:name "Eclipse Public License - v 1.0"
            :url "http://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [com.datomic/datomic-free "0.9.4324"]]
  :profiles {:dev {:dependencies [[midje "1.5.0"]]}})