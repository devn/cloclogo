(ns cloclogo.clojuresphere
  (:require [cemerick.pomegranate :as pom]
            [org.httpkit.client :as http]
            [cheshire.core :as json]))

(def api-base "http://www.clojuresphere.com/api/projects")

(defn project-query-string
  [query opt-map]
  (format "?query=%s" query))

(defn projects
  "Search project listings"
  [query]
  (-> (str api-base (project-query-string query {:sort "ascending"}))
      (http/get)
      deref
      :body
      (json/parse-string true)))
