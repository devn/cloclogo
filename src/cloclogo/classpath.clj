(ns cloclogo.classpath)

; This Source Code Form is subject to the terms of the Mozilla Public License,
; v. 2.0. If a copy of the MPL was not distributed with this file, You can
; obtain one at http://mozilla.org/MPL/2.0/.
 
(ns cloclogo.classpath
  (:import (java.net URI) java.io.File))
 
(defn- find-clojure-jar
  "Returns a file referring to the clojure jar on the classpath."
  []
  {:post [% (.exists %)]}
  (let [clojure-classfile (-> (class #())
                            .getClassLoader
                            (.getResource "clojure/lang/RT.class"))
        _ (assert clojure-classfile)
        jar (-> clojure-classfile
              str
              (subs 4)
              (str "/../../..")
              java.net.URI.
              java.io.File.
              .getCanonicalFile
              str)]
    (java.io.File. (subs jar 0 (dec (count jar))))))
 
(def class-re #"^(clojure/lang/[^\.]+)\.class")
; we don't care about:
(def exclusion-patterns [#"\d" ;; anonymous inner classes (^\d)
                         #"LispReader\$" ;; LispReader inner classes
                         ])
 
(defn- find-all-clojure-classes
  "Returns a seq of symbols of the public Clojure Java classes."
  []
  {:post [(> (count %) 100)]}
  (with-open [f (java.util.zip.ZipFile. (find-clojure-jar))]
    (doall (for [e (enumeration-seq (.entries f))
                 :let [name (.getName e)]
          :when (and (re-matches class-re name)
                     (not (some #(re-find % name) exclusion-patterns)))]
      (-> (re-seq class-re (.getName e))
        first
        last
        (.replace \/ \.)
        symbol)))))
 
(def all-classnames (find-all-clojure-classes))
(def all-classes (map eval all-classnames))
 
(defmacro import-all-clojure-classes
  []
  (cons 'do
    (for [cn all-classnames]
      `(import ~cn))))
 
(defn implementations-of
  [cls]
  (->> all-classes
    (filter #(.isAssignableFrom cls %))
    ; including java.lang.Object just makes the graph a distaster
    (remove #{cls Object})))
