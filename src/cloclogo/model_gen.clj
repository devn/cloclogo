; This Source Code Form is subject to the terms of the Mozilla Public License,
; v. 2.0. If a copy of the MPL was not distributed with this file, You can
; obtain one at http://mozilla.org/MPL/2.0/.

(ns cloclogo.model-gen
  (:use
    [cloclogo.ontology :only (relations) :as ontology]
    [cloclogo.classpath :only (implementations-of)]
    [clojure.core.incubator :only (-?>)])
  (:require clojure.repl
            [cheshire.core :as json]))

(comment
  (set! *print-level* 10)
  (set! *print-length* 100)
)

(defn as-collection
  "If v is a collection, returns it, else returns a collection containing v."
  [v]
  (if (or (coll? v) (instance? java.util.Collection v)) v [v]))

(def ^{:dynamic true} *debug* false)

(defn- expand-var-or-fn
  [meta x]
  (let [subtype (cond
                  (:macro meta) :macro
                  (:arglists meta) :fn
                  :else :var)
        ns (-> meta :ns .name str)
        name (-> meta :name str)]
    {:type :var
     :subtype subtype
     :name (str ns \/ name)
     :id (str ns \/ name)
     :info {:ns ns
            :name name
            :args (-?> meta :arglists str)
            :added (-?> meta :added str)
            :doc (-?> meta :doc str)
            :source (clojure.repl/source-fn (symbol ns name))}}))

(defn- lift-map-info
  [{:keys [notes] :as m}]
  (if notes
    (-> (dissoc m :notes)
      (assoc-in [:info :notes] notes))
    m))

(defn- kwstr [k]
  (if (keyword? k)
    (-> k str (subs 1))
    k))

(defn- expand-object
  [m x]
  (let [meta (meta x)]
    (condp instance? x
      Class (let [interface? (.isInterface x)
                  type (if interface? :interface :class)]
              {:type type
               :name (.getName x)
               :id (.getName x)})
      clojure.lang.Keyword (or (x m) {:type "concept"
                                      :name (-> x kwstr (.replace "-" " "))
                                      :id x})
      clojure.lang.Var (expand-var-or-fn meta x)
      clojure.lang.Fn (expand-var-or-fn meta x)
      clojure.lang.IPersistentMap (lift-map-info x)
      String {:type :string
              :name x
              :id x})))

(defn- generate-relationships
  [x]
  (condp instance? x
    Class (let [bases (remove #{Object} (bases x))
                interfaces (set (filter #(.isInterface %) bases))
                classes (set (remove interfaces bases))]
            (if (.isInterface x)
              [:implements interfaces]
              [:implements interfaces
               :bases classes]))
    nil))

(defn- into-set
  [to from]
  (if (seq to)
    (into to from)
    (set from)))

(defn- combine-rels
  [kv-pairs]
  (reduce
    (fn [rels [predicate objids]]
      (update-in rels [predicate] into-set (as-collection objids)))
    {} kv-pairs))

(defn- add-subject
  "Add subject/object x to the given map using (:id x) as the key;
   or, if the subject's id is already present, intelligently merge
   the contents of the subjects' :rels sets and merge the rest of
   subjects' keys, preferring the values of x."
  [m {:keys [xrels] :as x}]
  (if-let [{:keys [rels] :as existing} (m (:id x))]
    (->> (combine-rels (concat rels xrels))
      (hash-map :rels)
      (merge existing x)
      (assoc m (:id x)))
    (assoc m (:id x) x)))

(defn- expand
  [m [subject & rels]]
  (let [rels (combine-rels (partition 2 rels)) ;; doing this manually so we can accidentially repeat a rel key in a subject vector without getting spit at
        {sid :id :as ex-subject} (expand-object m subject)]
    (reduce
      (fn [m [predicate object]]
        (let [objects (as-collection object)
              m (reduce expand m (for [obj objects]
                                   (cons obj (generate-relationships obj))))]
          (update-in m
                     [sid :rels predicate]
                     into-set
                     (map (comp :id (partial expand-object m)) objects))))
      (add-subject m ex-subject)
      (->> (generate-relationships subject)
        (partition 2)
        (concat rels)))))

(defn- make-rels-bidirectional
  [m]
  (reduce
    (fn [m {:keys [id rels]}]
      (reduce
        (fn [m [predicate sids]]
          (if (-> predicate name (.startsWith "-"))
            m
            (reduce
              (fn [m sid]
                (update-in m [sid :rels (->> predicate name (str "-") keyword)] into-set [id]))
              m sids)))
        m
        rels))
    m
    (vals m)))

(defn- retain-namespaced-keywords
  [m]
  (into {} (for [[id {:keys [id rels] :as x}] m]
             [(kwstr id) (assoc x
                                :id (kwstr id)
                                :rels (into {} (for [[pred ids] rels
                                                     ;; elide rels with empty object sets while we're here...
                                                     :when (seq ids)]
                                                 [pred (set (map kwstr ids))])))])))

(defn expanded-ontology
  [ontology & {:keys [debug]}]
  (binding [*debug* debug]
    (-> (reduce
          expand
          {}
          ontology)
      make-rels-bidirectional
      retain-namespaced-keywords)))

(defn write-updated-ontology
  [ontology path clojure-version]
  (let [ontology (expanded-ontology ontology)]
    (when (seq ontology/orphans)
      (println (format "%s orphaned vars in %s:" (count ontology/orphans) clojure-version))
      (doseq [v ontology/orphans] (println v)))
    (spit path
          (str
            (format "function clojureVersion () { return %s; };" (pr-str clojure-version))
            (format "function buildOntology () { \nreturn (%s); };" (json/generate-string ontology {:pretty true})))
          :enc "UTF-8")
    (println "Wrote ontology to" path)))
