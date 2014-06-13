(defproject cloclogo "0.1.0"
  :description "Indiana Jones and the Crystal Skull, except Clojure"
  :url "https://github.com/devn/cloclogo"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/tools.analyzer.jvm "0.2.2"]
                 [org.clojure/tools.logging "0.3.0"]
                 [org.clojure/core.incubator "0.1.3"]
                 [cheshire "5.3.1"]
                 [clojail "1.0.6"]]
  :jvm-opts ["-Djava.security.manager" "-Djava.security.policy=java.policy"])
