(ns cloclogo.dev
  (:require [clojure.pprint :refer (pprint
                                    code-dispatch
                                    with-pprint-dispatch)]))

(defn pp [x]
  (pprint x))

(defn ppc [x]
  (with-pprint-dispatch code-dispatch
    (pprint x)))
