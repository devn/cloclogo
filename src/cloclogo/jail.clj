(ns cloclogo.jail
  (:require [clojail.core :as jail]
            [clojail.testers :as testers]))

(def tester testers/secure-tester)
(def sb (jail/sandbox tester :timeout 5000))
