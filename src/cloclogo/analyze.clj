(ns cloclogo.analyze
  (:require [clojure.tools.analyzer.jvm :as analyzer]
            [clojail.core :as jail]
            [clojail.testers :as testers]))

(def tester testers/secure-tester)
(def sandbox (jail/sandbox tester :timeout 3000))

(comment
  "...Messing around..."
  (require '[cloclogo.dev :refer (pp ppc)])

  (analyzer/analyze (+ 1 1))
  (analyzer/analyze (jail/safe-read "(+ 1 1)"))
  (analyzer/analyze (sandbox (jail/safe-read "(+ 1 1)")))

  (let [sexp-str "(defn foo [x] (inc x))"]
    (analyzer/analyze
     (try (sandbox (jail/safe-read sexp-str))
          (catch SecurityException se
            '())
          (finally (println sexp-str)))))
  )
