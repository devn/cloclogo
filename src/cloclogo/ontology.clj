;; This Source Code Form is subject to the terms of the Mozilla Public License,
;; v. 2.0. If a copy of the MPL was not distributed with this file, You can
;; obtain one at http://mozilla.org/MPL/2.0/.

(ns cloclogo.ontology
  (:use [cloclogo.classpath :only (all-classes import-all-clojure-classes implementations-of)]))

(import-all-clojure-classes)

(def duals
  ;; TODO some horrible names here
  {:creates :created-by
   :accepts :can-be-used-with
   :isa     :is-the-type-of})

(def namespaces '[clojure.core
                  clojure.set
                  clojure.test
                  clojure.uuid
                  clojure.pprint
                  clojure.walk
                  clojure.inspector
                  clojure.main
                  clojure.java.shell
                  clojure.zip
                  clojure.java.browse
                  clojure.repl
                  clojure.stacktrace
                  clojure.java.io
                  clojure.string
                  clojure.core.protocols
                  clojure.instant
                  clojure.xml
                  clojure.java.javadoc
                  clojure.template])

(def publics (->> namespaces
               (map #(try
                       (require %) %
                       (catch java.io.FileNotFoundException e)))
               (remove nil?)
               (map find-ns)
               (remove nil?)
               (mapcat ns-publics)
               (map second)))

(def publics-symbols (->> publics
                       (map meta)
                       (map (juxt (comp #(.getName %) :ns) :name))
                       (map (fn [[ns name]] (symbol (str ns) (str name))))))

(def relations
  {:creates "creates %ss"
   :typeof "types of %s"
   :accepts "accepts arguments of type %s"
   :isa "%ss"
   :implementedby "implemented by %s"
   :relatedto "%s"})

(defn- resolve-var
  [s]
  (->> (map ns-resolve (remove nil? (map find-ns namespaces)) (repeat s))
    (remove nil?)
    first))

(defmacro defontology
  [name triples]
  `(def ~name ~(into [] (let [clojure-version (clojure-version)]
                          (for [[s :as triple] triples
                                :let [s (if (symbol? s)
                                          (resolve-var s)
                                          s)
                                      version (or (-> triple meta :version) "1.0.0")]
                                :when (and s
                                           (not (neg? (compare clojure-version version))))]
                            (assoc triple 0 s))))))

(defontology ontology
  [[{:type :root :id :clojure/root :name "Clojure"}]

   [#'clojure-version]

   [{:type :concept :id :concept/special-forms :name "Special Forms"}
    :relatedto :clojure/root]

   [{:type :special-form :id :specials/var :name "var"}
    :relatedto :ref/vars
    :typeof :concept/special-forms]
   [{:type :special-form :id :specials/def :name "def"}
    :creates :ref/vars
    :typeof :concept/special-forms]
   [{:type :special-form :id :specials/let :name "let"}
    :relatedto :concept/local-bindings
    :typeof :concept/special-forms]
   [#'let :implementedby :specials/let]
   [{:type :concept :id :concept/host-interop :name "Host interop"}
    :implementedby :specials/interop
    :relatedto :clojure/root]
   [{:type :special-form :id :specials/interop :name "."}
    :relatedto :concept/host-interop
    :isa :concept/special-forms]
   ^{:version "1.4.0"}
   [{:type :special-form :id :specials/field-access :name ".-"
     :notes "This form of host interop works only for accessing public
fields of records and types, and is exactly equivalent to the
standard `.` special form.  So, given this:

  => (deftype A [x y])
  user.A
  => (def a (A. 1 2))
  #'user/a

These two expressions are equivalent:

  (.x a)
  (.-x a)

.- exists solely to make it possible for the same code to run properly
on Clojure and ClojureScript (as host field access in ClojureScript
cannot be done completely via `.`)."}
    :relatedto #{:specials/interop :concept/host-interop}
    :isa :concept/special-forms]

   [#'.. :relatedto :concept/host-interop]
   [#'doto :relatedto :concept/host-interop] ;; TODO can also be used with mutable types, reference types

   [{:type :concept :id :concept/error-handling :name "Error handling"} :relatedto :clojure/root]
   [{:type :special-form :id :specials/try :name "try"}
    :relatedto :concept/error-handling
    :isa :concept/special-forms]
   [{:type :special-form :id :specials/throw :name "throw"}
    :relatedto :concept/error-handling
    :isa :concept/special-forms]
   [ex-data :relatedto :concept/error-handling]
   [ex-info :relatedto :concept/error-handling]

   [{:type :concept :id :concept/local-bindings :name "Local bindings"}]

   [{:type :concept :id :concept/impl-detail :name "Implementation Details"}]

   [{:type :concept :id :concept/reader-syntax :name "Reader syntax"}
    :relatedto :clojure/root]
   [#'char-escape-string :isa :concept/impl-detail :relatedto :concept/reader-syntax]

   ^{:version "1.4.0"}
   [{:type :concept :id :concept/tagged-literals :name "Tagged literals"}
    :typeof :concept/reader-syntax]
   [clojure.core/*data-readers* :relatedto :concept/tagged-literals]
   [clojure.core/default-data-readers :relatedto :concept/tagged-literals]
   ^{:version "1.4.0"}
   [{:type :syntax :id :tagged-literal/uuid :name "#uuid \"f721ca83-2051â€¦\""
     :notes "UUID literal."}
    :creates java.util.UUID
    :typeof :concept/tagged-literals]
   ^{:version "1.4.0"}
   [{:type :syntax :id :tagged-literal/instant :name "#inst \"2012-04-24T13:05:20.697-00:00\""
     :notes "Date (instant) literal, manifested as java.util.Date by default."}
    :creates java.util.Date
    :typeof :concept/tagged-literals]
   [clojure.instant/read-instant-calendar :relatedto :tagged-literal/instant]
   [clojure.instant/read-instant-date :relatedto :tagged-literal/instant]
   [clojure.instant/read-instant-timestamp :relatedto :tagged-literal/instant]

   ;; TODO need types concept

   [{:type :concept :id :concept/nil :name "nil"}]
   [{:type :concept :id :types/boolean :name "Booleans"}
    :implementedby Boolean]

   [{:type :concept :id :types/chars :name "Characters"}
    :implementedby Character]
   [{:type :concept :id :concept/comments :name "Comments"}]

   [{:type :concept :id :types/strings :name "Strings"}
    :implementedby String]
   [#'str :creates :types/strings]
   [#'subs :creates :types/strings :accepts :types/strings]

   [{:type :concept :id :concept/symbols :name "Symbols"}
    :implementedby Symbol]
   [{:type :concept :id :concept/keywords :name "Keywords"}
    :implementedby Keyword
    :relatedto :concept/symbols]
   [#'symbol :creates :concept/symbols]
   [#'gensym :creates :concept/symbols :relatedto :concept/macros]
   [#'keyword :creates :concept/keywords]
   [find-keyword :relatedto #{#'keyword}]

   [{:type :concept :id :concept/repl-binding :name "REPL Bindings"}]
   [#'*3 :isa :concept/repl-binding]
   [#'*2 :isa :concept/repl-binding]
   [#'*1 :isa :concept/repl-binding]
   [#'*e :isa :concept/repl-binding]
   [#'*ns* :isa :concept/repl-binding]


   [{:type :concept :id :concept/conditional-forms :name "Conditional forms"}]
   [#'case :isa :concept/conditional-forms]
   [#'condp :isa :concept/conditional-forms]
   [#'cond :isa :concept/conditional-forms]
   [#'when-let :isa :concept/conditional-forms :relatedto :concept/local-bindings]
   [#'when-first :isa :concept/conditional-forms :relatedto :concept/local-bindings]
   [#'when-not :isa :concept/conditional-forms]
   [#'when :isa :concept/conditional-forms]
   [#'if-let :isa :concept/conditional-forms :relatedto :concept/local-bindings]
   [#'if-not :isa :concept/conditional-forms]



   [{:type :concept :id :concept/boolean-ops :name "Boolean Operators"}]
   [#'not :isa :concept/boolean-ops]
   [#'or :isa :concept/boolean-ops]
   [#'and :isa :concept/boolean-ops]

   [{:type :concept :id :concept/bit-twiddling :name "Bit-twiddling Operators"}]
   [#'bit-shift-left :isa :concept/bit-twiddling]
   [#'bit-shift-right :isa :concept/bit-twiddling]
   [#'bit-not :isa :concept/bit-twiddling]
   [#'bit-and :isa :concept/bit-twiddling]
   [#'bit-flip :isa :concept/bit-twiddling]
   [#'bit-xor :isa :concept/bit-twiddling]
   [#'bit-set :isa :concept/bit-twiddling]
   [#'bit-or :isa :concept/bit-twiddling]
   [#'bit-test :isa :concept/bit-twiddling]
   [#'bit-and-not :isa :concept/bit-twiddling]
   [#'bit-clear :isa :concept/bit-twiddling]

   [#'name :accepts #{String Named}]
   [#'namespace :accepts #{Named}]
   [#'symbol? :isa #{:predicate}
    :relatedto :concept/symbols]
   [#'keyword? :isa #{:predicate}
    :relatedto :concept/keywords]
   #_[:predicate :isa :function]

   ;; collections
   [{:type :concept :id :ds/collections :name "Collections"}
    :relatedto :clojure/root
    :implementedby IPersistentCollection]

   [#'empty? :accepts :ds/collections]
   [#'empty :accepts :ds/collections]
   [#'not-empty :accepts :ds/collections]
   [#'flatten :accepts :ds/collections]

   [{:type :concept :id :ds/seqs :name "Seqs"}
    :implementedby ISeq]
   [{:type :concept :id :ds/cons :name "Cons"}
    :implementedby Cons
    :typeof :ds/seqs]

   ;; sets, clojure.set
   [{:type :concept :id :ds/sets :name "Sets"}
    :implementedby IPersistentSet
    :typeof :ds/collections]
   [#'disj :returns :ds/sets]
   [#'set :creates :ds/sets]

   [{:type :concept :id :ds/sorted-collections :name "Sorted Collections"}
    :typeof :ds/collection
    :implementedby Sorted]

   [{:type :concept :id :ds/sorted-sets :name "Sorted sets"}
    :typeof #{:ds/sets :ds/sorted-collections}]
   [{:type :concept :id :ds/lists :name "Lists"}
    :implementedby IPersistentList
    :typeof #{:ds/collections :ds/stacks}]
   [{:type :concept :id :ds/vectors :name "Vectors"}
    :implementedby IPersistentVector
    :typeof #{:ds/collections :ds/stacks :ds/associative}]

   [#'vector :creates :ds/vectors]
   [#'subvec :creates :ds/vectors]
   [#'vector? :relatedto :ds/vectors]
   [#'vector :creates :ds/vectors]
   [#'vec :creates :ds/vectors]
   [#'vector-of :creates :ds/vectors]

   [#'list :creates :ds/lists]
   [#'list? :relatedto :ds/lists]
   [#'list* :creates :ds/lists]
   [#'cons :creates :ds/cons :isa :concept/accumulation]

   ;; TODO queue data structure

   [{:type :concept :id :ds/stacks :name "Stacks"}
    :implementedby IPersistentStack
    :typeof :ds/collections]
   [#'pop :accepts :ds/stacks]
   [#'peek :accepts :ds/stacks]

   [{:type :concept :id :ds/associative :name "Associative"}
    :definedby Associative]

   [{:type :concept :id :ds/maps :name "Maps"}
    :relatedto :ds/collections
    :typeof :ds/associative
    :definedby #{IPersistentMap java.util.Map}]


   [#'select-keys :relatedto :ds/maps]
   [#'keys :accepts :ds/maps :returns :ds/seqs]
   [#'vals :accepts :ds/maps :returns :ds/seqs]
   [#'val :relatedto :ds/maps]
   [#'key :relatedto :ds/maps]

   [#'group-by :creates :ds/maps]
   [#'contains? :accepts #{:ds/associative :ds/sets :ds/maps :types/strings :ds/arrays}]
   [#'find :accepts :ds/associative]
   [#'get :accepts :ds/associative]

   [#'merge :returns :ds/maps :isa :concept/accumulation]
   [#'merge-with :accepts :ds/maps :relatedto #'merge :isa :concept/accumulation]
   ;;
   [#'update-in :returns :ds/associative]
   [#'get-in :accepts :ds/associative]
   [#'dissoc :returns :ds/maps]
   [#'assoc :returns :ds/associative :isa :concept/accumulation]
   [#'assoc-in :returns :ds/associative :isa :concept/accumulation]

   [#'sorted-map :creates :ds/sorted-map]
   [#'sorted-set :creates :ds/sorted-sets]
   [#'sorted-set-by :creates :ds/sorted-sets]
   [#'sorted-map-by :creates :ds/sorted-map]
   [#'sorted? :relatedto Sorted]

   [{:type :concept :id :ds/hash-map :name "Hash Maps"}
    :typeof :ds/maps]
   [#'hash-map :creates :ds/hash-map]

   [{:type :concept :id :ds/array-map :name "Array Maps"}
    :typeof :ds/maps]
   [#'array-map :creates :ds/array-map]

   [#'hash-set :creates :ds/hash-set]

   [{:type :concept :id :concept/looping :name "Looping Forms"}]
   [#'doseq :isa #{:concept/looping :concept/side-effecting}]
   [#'dotimes :isa #{:concept/looping :concept/side-effecting}]
   ;;
   [#'for :isa :concept/looping :returns :ds/lazy-seqs]
   ;;
   [#'loop :isa #{:concept/looping :concept/special-forms}]
   [{:type :special-form :id :specials/recur :name "recur"}
    :relatedto #'loop :typeof :concept/special-forms]
   [#'while :isa #{:concept/looping :concept/side-effecting}]
   ;;
   [#'dorun :relatedto #{:concept/side-effecting :ds/lazy-seqs}]
   [#'doall :relatedto #{:concept/side-effecting :ds/lazy-seqs}]

   ;; transients
   [{:type :concept :id :ds/transients :name "Transients"}
    :typeof :ds/collections]
   [#'pop! :relatedto :ds/transients]
   [#'disj! :relatedto :ds/transients]
   [#'persistent! :accepts :ds/transients]
   [#'dissoc! :relatedto :ds/transients]
   [#'conj! :relatedto :ds/transients :isa :concept/accumulation]
   [#'assoc! :relatedto :ds/transients :isa :concept/accumulation]
   [#'transient :creates :ds/transients]

   [{:type :syntax :id :reader/anon-fn :name "#(fn literal)"}
     :typeof :concept/reader-syntax
     :creates :concept/functions
     :replaces #'memfn]
   [{:type :syntax :id :reader/static-interop :name "Classname/staticMember"}
    :relatedto :concept/host-interop
    :typeof :concept/reader-syntax]
   [{:type :syntax :id :reader/host-interop :name ".instanceMember"}
    :relatedto :concept/host-interop
    :typeof :concept/reader-syntax]
   [{:type :syntax :id :reader/maps :name "{k1 v1 k2 v2 â€¦}"}
    :creates :ds/maps
    :typeof :concept/reader-syntax]
   [{:type :syntax :id :reader/vectors :name "[x y z â€¦]"}
    :creates :ds/vectors
    :typeof :concept/reader-syntax]
   [{:type :syntax :id :reader/sets :name "#{x y z â€¦}"}
    :creates :ds/sets
    :typeof :concept/reader-syntax]
   [{:type :syntax :id :reader/lists :name "(x y z â€¦)"}
    :creates :ds/lists
    :typeof :concept/reader-syntax]
   [{:type :syntax :id :reader/gensyms :name "name#"}
    :creates :concept/symbols
    :equivalentto #'gensym
    :typeof :concept/reader-syntax
    :relatedto :concept/macros]
   [{:type :syntax :id :reader/characters :name "\\c"}
    :creates :types/chars
    :typeof :concept/reader-syntax]
   [{:type :syntax :id :reader/line-comment :name "; line commentâ€¦"}
    :creates :concept/comments
    :typeof :concept/reader-syntax]
   [{:type :syntax :id :reader/form-elide :name "#_(ignored formâ€¦)"}
    :creates :concept/comments
    :typeof :concept/reader-syntax]
   [{:type :syntax :id :reader/reader-eval :name "#=(read-time-eval)"}
    :relatedto #{#'eval #'print-dup}
    :typeof :concept/reader-syntax]
   [{:type :syntax :id :reader/nil :name "nil"}
    :relatedto :concept/nil
    :typeof :concept/reader-syntax]
   [{:type :syntax :id :reader/true :name "true"}
    :relatedto :types/boolean
    :typeof :concept/reader-syntax]
   [{:type :syntax :id :reader/false :name "false"}
    :relatedto :types/boolean
    :typeof :concept/reader-syntax]
   [{:type :syntax :id :reader/strings :name "\"string\""}
    :creates :types/strings
    :typeof :concept/reader-syntax]
   [{:type :syntax :id :reader/keywords :name ":ns/keyword"}
    :creates :concept/keywords
    :typeof :concept/reader-syntax]
   [{:type :syntax :id :reader/deref :name "@reference-type"}
     :relatedto #'deref
    :typeof :concept/reader-syntax]
   [{:type :syntax :id :reader/metadata :name "^{:meta :data}"}
     :defines :concept/metadata
    :typeof :concept/reader-syntax]
   ;; TODO talk about ^booleans and ^"[L", etc
   [{:type :syntax :id :reader/type-hints :name "^TypeHint"}
     :defines #{:concept/metadata :concept/type-hints}
    :typeof :concept/reader-syntax]
   [{:type :syntax :id :reader/true-metadata :name "^:true-metadata"}
     :defines #{:concept/metadata}
    :typeof :concept/reader-syntax]

   [{:type :concept :id :concept/metadata :name "Metadata"}
    :relatedto :clojure/root]
   [#'with-meta :relatedto :concept/metadata]
   [#'vary-meta :relatedto :concept/metadata]
   [#'meta :relatedto :concept/metadata]
   [#'reset-meta! :relatedto :concept/metadata]
   [#'alter-meta! :relatedto :concept/metadata]

   [{:type :concept :id :concept/type-hints :name "Type Hinting"
     :notes "Type hints provide a way for you to avoid reflection when calling
Java methods or referring to fields in Java classes."}
    :relatedto :concept/host-interop]


   ;; numbers
   [{:type :concept :id :concept/numbers :name "Numbers"}]

   [{:type :concept :id :concept/rationals :name "Rational Numbers"}
    :typeof :concept/numbers
    :implementedby Ratio]
   [{:type :syntax :id :reader/rationals :name "22/7 (rationals)"}
    :creates :concept/rationals
    :typeof :concept/reader-syntax]
   [#'rationalize :returns :concept/rationals]
   [#'denominator :accepts :concept/rationals]
   [#'numerator :accepts :concept/rationals]

   [#'ratio? :relatedto :concept/rationals]
   [#'rational? :relatedto #{:concept/rationals :concept/numbers}]
   [#'number? :relatedto :concept/numbers]
   [#'integer? :relatedto :concept/numbers]
   [#'float? :relatedto :concept/numbers]
   [#'decimal? :relatedto :concept/numbers]

   [#'with-precision :relatedto :types/decimals]
   [#'*math-context* :relatedto #{#'with-precision :types/decimals java.math.MathContext}]

   [{:type :concept :id :concept/numeric-comparisons :name "Numeric Comparisons"}
    :relatedto :concept/numbers]
   [#'zero? :typeof :concept/numeric-comparisons]
   [#'odd? :typeof :concept/numeric-comparisons]
   [#'< :typeof :concept/numeric-comparisons]
   [#'>= :typeof :concept/numeric-comparisons]
   [#'<= :typeof :concept/numeric-comparisons]
   [#'> :typeof :concept/numeric-comparisons]
   [#'even? :typeof :concept/numeric-comparisons]
   [#'pos? :typeof :concept/numeric-comparisons]
   [#'neg? :typeof :concept/numeric-comparisons]

   ;; arithmetic
   [{:type :concept :id :concept/arithmetic-ops :name "Arithmetic operators"}
    :relatedto #{:concept/arithmetic :concept/numbers}]
   [#'dec :isa :concept/arithmetic-ops]
   [#'inc :isa :concept/arithmetic-ops]
   [#'+ :isa :concept/arithmetic-ops]
   [#'/ :isa :concept/arithmetic-ops]
   [#'* :isa :concept/arithmetic-ops]
   [#'- :isa :concept/arithmetic-ops]
   [#'rem :isa :concept/arithmetic-ops]
   [#'quot :isa :concept/arithmetic-ops :relatedto #'rem]
   [#'mod :isa :concept/arithmetic-ops]
   [#'min :isa :concept/arithmetic-ops]
   [#'max :isa :concept/arithmetic-ops]

   [{:type :concept :id :concept/unchecked-arithmetic-ops :name "Unchecked arithmetic operators"}
    :relatedto #{:concept/arithmetic :concept/numbers}]
   [clojure.core/*unchecked-math* :relatedto :concept/unchecked-arithmetic-ops]
   [unchecked-remainder :isa :concept/unchecked-arithmetic-ops]     ;; clojure 1.2-only
   [unchecked-remainder-int :isa :concept/unchecked-arithmetic-ops] ;; clojure 1.3+
   [#'unchecked-subtract :isa :concept/unchecked-arithmetic-ops]
   [#'unchecked-add :isa :concept/unchecked-arithmetic-ops]
   [#'unchecked-negate :isa :concept/unchecked-arithmetic-ops]
   [#'unchecked-inc :isa :concept/unchecked-arithmetic-ops]
   [#'unchecked-multiply :isa :concept/unchecked-arithmetic-ops]
   [#'unchecked-dec :isa :concept/unchecked-arithmetic-ops]
   [unchecked-divide :isa :concept/unchecked-arithmetic-ops]        ;; clojure 1.2 only
   [unchecked-divide-int :isa :concept/unchecked-arithmetic-ops]    ;; clojure 1.3+ only

   [unchecked-add-int :isa :concept/unchecked-arithmetic-ops]
   [unchecked-byte :isa :concept/unchecked-arithmetic-ops]
   [unchecked-char :isa :concept/unchecked-arithmetic-ops]
   [unchecked-dec-int :isa :concept/unchecked-arithmetic-ops]
   [unchecked-double :isa :concept/unchecked-arithmetic-ops]
   [unchecked-float :isa :concept/unchecked-arithmetic-ops]
   [unchecked-inc-int :isa :concept/unchecked-arithmetic-ops]
   [unchecked-int :isa :concept/unchecked-arithmetic-ops]
   [unchecked-long :isa :concept/unchecked-arithmetic-ops]
   [unchecked-multiply-int :isa :concept/unchecked-arithmetic-ops]
   [unchecked-negate-int :isa :concept/unchecked-arithmetic-ops]
   [unchecked-short :isa :concept/unchecked-arithmetic-ops]
   [unchecked-subtract-int :isa :concept/unchecked-arithmetic-ops]

   ;; equality
   [{:type :concept :id :concept/equality :name "Equality"}
    :relatedto :clojure/root]
   [#'identical? :relatedto :concept/equality]
   [#'== :relatedto :concept/equality :accepts :concept/numbers
     :typeof :concept/numeric-comparisons]
   [#'= :relatedto :concept/equality]
   [#'not= :relatedto :concept/equality :complements #'=]
   [#'distinct? :relatedto :concept/equality]

   ;; regexes
   [{:type :concept :id :concept/regex :name "Regular Expressions"}
    :implementedby java.util.regex.Pattern]
   [{:type :syntax :id :reader/regex :name "#\"regex pattern\""}
    :creates :concept/regex :typeof :concept/reader-syntax]
   [#'re-groups :relatedto :concept/regex]
   [#'re-find :relatedto :concept/regex]
   [#'re-pattern :relatedto :concept/regex]
   [#'re-matches :relatedto :concept/regex]
   [#'re-matcher :relatedto :concept/regex]
   [#'re-seq :relatedto :concept/regex :isa :concept/x-to-seq]


   [#'clojure.string/replace :relatedto :concept/regex]

   [{:type :concept :id :concept/random-ops :name "Randomness"}]
   [#'rand :relatedto :concept/random-ops :returns :types/float]
   [#'rand-int :relatedto :concept/random-ops :returns :types/int]
   [#'rand-nth :relatedto #{:concept/random-ops #'nth}
    ;; TODO not true, takes only sequential collections
    :accepts :ds/collections]
   [#'shuffle :relatedto :concept/random-ops :accepts :ds/collections]

   [#'comment :relatedto :concept/comments]

   [{:type :concept :id :ds/arrays :name "Arrays"}
    :relatedto :ds/collections]
   [#'boolean-array :creates :ds/arrays]
   [#'object-array :creates :ds/arrays]
   [#'to-array :creates :ds/arrays]
   [#'make-array :creates :ds/arrays]
   [#'byte-array :creates :ds/arrays]
   [#'to-array-2d :creates :ds/arrays]
   [#'double-array :creates :ds/arrays]
   [#'int-array :creates :ds/arrays]
   [#'float-array :creates :ds/arrays]
   [#'into-array :creates :ds/arrays]
   [#'char-array :creates :ds/arrays]
   [#'long-array :creates :ds/arrays]
   [#'short-array :creates :ds/arrays]


   [#'amap :relatedto #'map :accepts :ds/arrays]
   [#'areduce :relatedto #'reduce :accepts :ds/arrays]

   [#'aclone :accepts :ds/arrays]
   [#'alength :accepts :ds/arrays :relatedto #'count]
   [#'aget :accepts :ds/arrays :relatedto #'nth]
   ;;
   [#'aset :accepts :ds/arrays]
   [#'aset-double :accepts :ds/arrays]
   [#'aset-boolean :accepts :ds/arrays]
   [#'aset-short :accepts :ds/arrays]
   [#'aset-char :accepts :ds/arrays]
   [#'aset-float :accepts :ds/arrays]
   [#'aset-byte :accepts :ds/arrays]
   [#'aset-long :accepts :ds/arrays]
   [#'aset-int :accepts :ds/arrays]

   [{:type :concept :id :concept/concurrency :name "Concurrency Facilities"}
    :relatedto :clojure/root]
   [{:type :concept :id :concept/reference-types :name "Reference Types"}
    :relatedto :concept/concurrency
    :relatedto :clojure/root]

   [#'set-validator! :relatedto :concept/reference-types]
   [#'get-validator :relatedto :concept/reference-types]
   [#'deref :relatedto :concept/reference-types :implementedby clojure.lang.IDeref]
   [#'remove-watch :relatedto :concept/reference-types]
   [#'add-watch :relatedto :concept/reference-types]

   [{:type :concept :id :ref/atoms :name "Atoms"}
    :typeof :concept/reference-types
    :implementedby clojure.lang.Atom]
   [#'atom :creates :ref/atoms]
   [#'reset! :relatedto :ref/atoms]
   [#'swap! :relatedto :ref/atoms]
   [#'compare-and-set! :relatedto :ref/atoms]


   [{:type :concept :id :ref/agents :name "Agents"}
    :typeof :concept/reference-types
    :implementedby clojure.lang.Agent]
   [#'release-pending-sends :relatedto :ref/agents]
   [#'send :relatedto :ref/agents]
   [#'send-off :relatedto :ref/agents]
   [#'agent :creates :ref/agents]
   [#'*agent* :relatedto :ref/agents]
   [#'shutdown-agents :relatedto :ref/agents]
   [#'agent-errors :relatedto :ref/agents]
   [#'restart-agent :relatedto :ref/agents]
   [#'agent-error :relatedto :ref/agents]
   [#'clear-agent-errors :relatedto :ref/agents]
   [#'await :relatedto :ref/agents]
   [#'await-for :relatedto :ref/agents]
   [#'await1 :relatedto :ref/agents] ;; TODO include?
   [#'set-error-handler! :relatedto :ref/agents]
   [#'error-handler :relatedto :ref/agents]
   [#'set-error-mode! :relatedto :ref/agents]
   [#'error-mode :relatedto :ref/agents]

   [{:type :concept :id :ref/refs :name "Refs"}
    :typeof :concept/reference-types
    :implementedby clojure.lang.Ref]
   [#'commute :relatedto :ref/refs]
   [#'ref-min-history :relatedto :ref/refs]
   [#'ref :creates :ref/refs]
   [#'ref-history-count :relatedto :ref/refs]
   [#'ref-set :relatedto :ref/refs]
   [#'ref-max-history :relatedto :ref/refs]
   [#'dosync :relatedto :ref/refs]
   [#'sync :relatedto :ref/refs]
   [#'io! :relatedto :ref/refs]
   [#'ensure :relatedto :ref/refs]
   [#'alter :relatedto :ref/refs]



   [{:type :concept :id :ref/vars :name "Vars"}
    :typeof :concept/reference-types
    :implementedby clojure.lang.Var]
   [#'resolve :returns :ref/vars]
   [#'ns-resolve :returns :ref/vars]
   [#'var-set :relatedto :ref/vars]
   [#'with-local-vars :creates :ref/vars :defines :concept/threadlocal-bindings]
   [#'var? :relatedto :ref/vars]
   [#'find-var :returns :ref/vars]
   [#'alter-var-root :relatedto :ref/vars]
   [#'var-get :relatedto :ref/vars
     ;; TODO :relatedto #'deref ?
     ]
   [#'thread-bound? :relatedto :ref/vars]
   [#'bound? :relatedto :ref/vars]

   [with-redefs :relatedto :ref/vars]
   [with-redefs-fn :relatedto :ref/vars]

   [{:type :concept :id :concept/threadlocal-bindings :name "Threadlocal Bindings"}
    :relatedto :ref/vars]
   [#'push-thread-bindings :defines :concept/threadlocal-bindings]
   [#'get-thread-bindings :relatedto :concept/threadlocal-bindings]
   [#'pop-thread-bindings :deletes :concept/threadlocal-bindings]
   [#'with-bindings :defines :concept/threadlocal-bindings]
   [#'with-bindings* :related-to #'with-bindings]
   [#'binding :defines :concept/threadlocal-bindings]
   [#'bound-fn :relatedto :concept/threadlocal-bindings]
   [#'bound-fn* :relatedto :concept/threadlocal-bindings]



   [{:type :concept :id :ref/futures :name "Futures"}
    :isa :concept/concurrency
    :relatedto #{:concept/evaluation java.util.concurrent.Future}]
   [#'future-cancelled? :relatedto :ref/futures]
   [#'future-cancel :relatedto :ref/futures]
   [#'future? :relatedto :ref/futures]
   [#'future-call :relatedto :ref/futures]
   [#'future :creates :ref/futures]
   [#'future-done? :relatedto :ref/futures]

   [{:type :concept :id :concept/delay :name "Delays"}
    :relatedto :concept/evaluation
    :implementedby clojure.lang.Delay]
   [#'delay :creates :concept/delay]
   [#'delay? :relatedto :concept/delay]
   [#'force :relatedto :concept/delay]

   [{:type :concept :id :concept/promise :name "Promises"}
    :isa :concept/concurrency
    :relatedto :concept/evaluation]
   [#'promise :creates :concept/promise]
   [#'deliver :relatedto :concept/promise]

   [realized? :relatedto #{:concept/promise :concept/delay :ref/futures :ds/lazy-seqs}
    :implementedby clojure.lang.IPending]

   [#'pmap :relatedto #{#'map :concept/concurrency} :relatedto :ds/lazy-seqs]
   [#'pcalls :relatedto :concept/concurrency :returns :ds/lazy-seqs]
   [#'pvalues :relatedto :concept/concurrency :returns :ds/lazy-seqs]
   [#'seque :relatedto :ds/lazy-seqs]

   [{:type :concept :id :concept/locks :name "Primitive Locks"}]
   [#'locking :relatedto #{:concept/locks :concept/concurrency}
    :implementedby #{:specials/monitor-enter :specials/monitor-exit}]

   [{:type :special-form :id :specials/monitor-enter :name "monitor-enter"}
    :relatedto :concept/locks :typeof :concept/special-forms]
   [{:type :special-form :id :specials/monitor-exit :name "monitor-exit"}
    :relatedto :concept/locks :typeof :concept/special-forms]

   [{:type :concept :id :concept/doc-utils :name "Documentation Utilities"}
    :typeof :concept/repl-utils]
   [print-special-doc :isa :concept/doc-utils]
   [print-doc :isa :concept/doc-utils]
   [find-doc :isa :concept/doc-utils]
   [print-namespace-doc :isa :concept/doc-utils]
   [doc :isa :concept/doc-utils]
   [#'add-classpath :is :concept/deprecated :isa :concept/doc-utils]

   [#'bean :relatedto :concept/host-interop]
   [#'cast :relatedto :concept/host-interop]
   [#'hash :relatedto :concept/host-interop]
   [#'namespace-munge :relatedto :concept/host-interop]

   [{:type :concept :id :concept/type-coercions :name "Type Coercions"}
    :isa #{:concept/array-coercions :concept/primitive-coercions :concept/numeric-coercions}]

   [{:type :concept :id :concept/array-coercions :name "Array Coercions"}]
   [#'floats :isa :concept/array-coercions]
   [#'doubles :isa :concept/array-coercions]
   [#'booleans :isa :concept/array-coercions]
   [#'bytes :isa :concept/array-coercions]
   [#'chars :isa :concept/array-coercions]
   [#'ints :isa :concept/array-coercions]
   [#'longs :isa :concept/array-coercions]
   [#'shorts :isa :concept/array-coercions]

   [{:type :concept :id :concept/numeric-coercions :name "Numeric Coercions"}]
   [#'num :isa :concept/numeric-coercions]
   [#'bigint :isa :concept/numeric-coercions]
   [biginteger :isa :concept/numeric-coercions]
   [#'bigdec :isa :concept/numeric-coercions]

   [{:type :concept :id :concept/primitive-coercions :name "Primitive Coercions"}]
   [#'boolean :isa :concept/primitive-coercions]
   [#'float :isa #{:concept/numeric-coercions :concept/primitive-coercions}]
   [#'int :isa #{:concept/numeric-coercions :concept/primitive-coercions}]
   [#'char :isa :concept/primitive-coercions]
   [#'long :isa #{:concept/numeric-coercions :concept/primitive-coercions}]
   [#'byte :isa :concept/primitive-coercions]
   [#'short :isa #{:concept/numeric-coercions :concept/primitive-coercions}]
   [#'double :isa #{:concept/numeric-coercions :concept/primitive-coercions}]

   [{:type :concept :id :concept/impl-interfaces :name "Implementing Interfaces"}
    :relatedto #{:concept/host-interop :concept/interfaces}]
   [{:type :concept :id :concept/subclassing :name "Subclassing"}
    :relatedto #{:concept/host-interop :concept/class}]
   [#'reify :provides :concept/impl-interfaces]
   ;;
   [#'update-proxy :relatedto #'proxy]
   [#'proxy-mappings :relatedto #'proxy]
   [#'get-proxy-class :relatedto #'proxy]
   [#'proxy-name :relatedto #'proxy]
   [#'construct-proxy :relatedto #'proxy]
   [#'proxy-super :relatedto #'proxy]
   [#'proxy-call-with-super :relatedto #'proxy]
   [#'init-proxy :relatedto #'proxy]
   [#'proxy :provides #{:concept/impl-interfaces :concept/subclassing}]
   ;;
   [#'gen-class :defines :concept/class :provides #{:concept/impl-interfaces :concept/subclassing}]

   [{:type :concept :id :concept/interfaces :name "Interfaces"}]
   [#'gen-interface :creates :concept/interfaces :relatedto :concept/host-interop]
   [#'definterface :relatedto :concept/host-interop :creates :concept/interfaces]


   [#'class :relatedto :concept/host-interop :returns Class]
   [#'type :relatedto :concept/metadata]
   [#'instance? :accepts Class :relatedto :concept/host-interop]

   [{:type :concept :id :concept/protocols :name "Protocols"}
    :relatedto :concept/host-interop]
   [#'find-protocol-method :relatedto :concept/protocols]
   [#'find-protocol-impl :relatedto :concept/protocols]
   [#'defprotocol :creates :concept/protocols]
   [#'extend-protocol :relatedto :concept/protocols]
   [#'-cache-protocol-fn :relatedto :concept/protocols :isa :concept/impl-detail]
   [#'extend-type :relatedto :concept/protocols]
   [#'extenders :relatedto :concept/protocols]
   [#'extends? :relatedto :concept/protocols]
   [#'extend :relatedto :concept/protocols]
   [#'satisfies? :relatedto :concept/protocols]
   [#'-reset-methods :relatedto :concept/protocols :isa :concept/impl-detail]

   [{:type :concept :id :concept/records :name "Records"}
    :relatedto #{:concept/host-interop :concept/protocols}
    :replaces :concept/structs
    :typeof :ds/maps]
   [#'defrecord :creates :concept/records]

   [{:type :concept :id :concept/deftype :name "Types"}
    :relatedto #{:concept/host-interop :concept/protocols :concept/records}]
   [#'deftype :creates :concept/deftype]


   [{:type :concept :id :concept/structs :name "Structs"}
    :typeof :ds/maps
    :implementedby clojure.lang.PersistentStructMap]
   [#'create-struct :defines :concept/structs]
   [#'defstruct :defines :concept/structs]
   [#'struct :creates :concept/structs]
   [#'struct-map :creates :concept/structs]
   [#'accessor :relatedto :concept/structs :creates :concept/function]

   [{:type :concept :id :concept/functions :name "Functions"}
    :relatedto :clojure/root
    :implementedby #{clojure.lang.IFn clojure.lang.AFn}]
   [#'defn :creates :concept/functions :defines :ref/vars]
   [#'defn- :creates :concept/functions :defines :ref/vars]
   [#'memfn :creates :concept/functions]
   [#'fn :creates :concept/functions]
   [#'letfn :creates :concept/functions :relatedto :concept/local-bindings]

   [#'defonce :defines :ref/vars]
   [#'declare :creates :ref/vars]
   [#'intern :returns :ref/vars]

   [{:type :concept :id :concept/HOFs :name "Higher Order Functions"}
    :relatedto :concept/functions]
   [#'partial :isa :concept/HOFs]
   [#'comp :isa :concept/HOFs]
   [#'memoize :isa :concept/HOFs]
   [#'apply :isa :concept/HOFs]
   [#'juxt :isa :concept/HOFs]
   [#'complement :isa :concept/HOFs]
   [#'fnil :isa :concept/HOFs]
   [#'trampoline :isa :concept/HOFs]
   [every-pred :isa :concept/HOFs]
   [some-fn :isa :concept/HOFs]

   [#'identity]
   [#'constantly]

   [{:type :concept :id :concept/macros :name "Macros"}
    :relatedto :clojure/root]
   [#'macroexpand-1 :relatedto :concept/macros]
   [#'macroexpand :relatedto :concept/macros]
   [#'defmacro :defines :concept/macros]
   [#'definline :relatedto :concept/macros :creates :concept/functions]
   [#'unquote-splicing :isa :concept/impl-detail :relatedto :concept/macros]
   [#'unquote :isa :concept/impl-detail :relatedto :concept/macros]

   [{:type :syntax :id :reader/unquote :name "~unquote"}
     :typeof :concept/reader-syntax
     :relatedto :concept/macros
     :implementedby #'unquote]
   [{:type :syntax :id :reader/unquote-splicing :name "~@unquote-splicing"}
     :typeof :concept/reader-syntax
     :relatedto :concept/macros
     :implementedby #'unquote-splicing]

   [{:type :concept :id :concept/multimethods :name "Multimethods"}
    :relatedto :clojure/root
    :dependsupon :concept/hierarchies
    :implementedby clojure.lang.MultiFn]
   [{:type :concept :id :concept/hierarchies :name "Ad-hoc Hierarchies"}]
   [#'make-hierarchy :creates :concept/hierarchies]
   [#'isa? :relatedto #{:concept/hierarchies :concept/multimethods}]
   [#'derive :relatedto :concept/hierarchies]
   [#'descendants :relatedto :concept/hierarchies]
   [#'underive :relatedto :concept/hierarchies]
   [#'ancestors :relatedto :concept/hierarchies]
   [#'parents :relatedto :concept/hierarchies]
   [#'supers :relatedto #{:concept/hierarchies :concept/host-interop}
    :accepts Class]
   [#'bases :relatedto #{:concept/hierarchies :concept/host-interop}
    :accepts Class]
   ;
   [#'prefer-method :relatedto :concept/multimethods]
   [#'prefers :relatedto :concept/multimethods]
   [#'defmulti :creates :concept/multimethods]
   [#'remove-all-methods :relatedto :concept/multimethods]
   [#'defmethod :creates :concept/multimethods]
   [#'remove-method :deletes :concept/multimethods]
   [#'get-method :relatedto :concept/multimethods]
   [#'methods :relatedto :concept/multimethods]

   [{:type :concept :id :concept/assertion :name "Assertions"}]
   [#'assert :defines :concept/assertion]
   [*assert* :relatedto :concept/assertion]
   [{:type :concept :id :concept/fn-conditions :name "Pre- and Post-conditions"}
    :typeof :concept/assertion]
   ;; TODO this isn't really syntax...
   [{:type :syntax :id :concept/fn-conditions-impl
     :name "{:pre [pre-expr*], :post [post-expr*]}"}
    :defines :concept/fn-conditions]

   [{:type :concept :id :concept/repl-utils :name "REPL Utilities"}
    :relatedto :clojure/root]
   [#'time :isa :concept/repl-utils :relatedto :concept/printing]

   [{:type :concept :id :concept/namespaces :name "Namespaces"}
    :relatedto :clojure/root]
   [#'remove-ns :deletes :concept/namespaces :isa :concept/repl-utils]
   [#'all-ns :relatedto :concept/namespaces :isa :concept/repl-utils]
   [#'the-ns :relatedto :concept/namespaces :isa :concept/repl-utils]
   [#'in-ns :relatedto #{:concept/namespaces #'*ns*} :isa :concept/repl-utils]
   [#'find-ns :returns :concept/namespaces :isa :concept/repl-utils]
   [#'ns :defines :concept/namespaces :isa :concept/repl-utils]
   [#'create-ns :creates :concept/namespaces :isa :concept/repl-utils]
   ;
   [#'ns-aliases :isa :concept/repl-utils :relatedto #{:concept/namespaces #'alias}]
   [#'ns-unalias :isa :concept/repl-utils :relatedto #{:concept/namespaces #'alias}]
   [#'ns-publics :isa :concept/repl-utils :relatedto :concept/namespaces]
   [#'ns-unmap :isa :concept/repl-utils :relatedto :concept/namespaces]
   [#'ns-refers :isa :concept/repl-utils :relatedto #{:concept/namespaces #'refer}]
   [#'ns-map :isa :concept/repl-utils :relatedto :concept/namespaces]
   [#'ns-imports :isa :concept/repl-utils :relatedto #{:concept/namespaces #'import}]
   [#'ns-name :isa :concept/repl-utils :relatedto :concept/namespaces]
   [#'ns-interns :isa :concept/repl-utils :relatedto :concept/namespaces]

   [#'loaded-libs]
   [#'use :relatedto #{:concept/namespaces #'ns #'loaded-libs} :isa :concept/repl-utils]
   [#'refer :relatedto #{:concept/namespaces #'ns} :isa :concept/repl-utils]
   [#'refer-clojure :relatedto #{:concept/namespaces #'ns} :isa :concept/repl-utils]
   [#'require :relatedto #{:concept/namespaces #'ns #'loaded-libs} :isa :concept/repl-utils]
   [#'import :relatedto #{:concept/namespaces #'ns} :isa :concept/repl-utils]
   [#'alias :relatedto #{:concept/namespaces #'ns} :isa :concept/repl-utils]
   ;;

   ;;
   [#'char-name-string]
   ;;
   [{:type :concept :id :concept/aot :name "Ahead-of-time compilation (AOT)"}
    :implementedby #'compile :relatedto :concept/evaluation]
   [#'compile :relatedto #'*compile-files*]
   [#'*compile-files*]
   [#'*compile-path* :relatedto :concept/aot]
   [#'*warn-on-reflection* :relatedto #{:concept/aot :concept/evaluation}]
   ;;
   [{:type :concept :id :concept/evaluation :name "Evaluation"}
    :implementedby #'eval]
   [#'eval]
   [#'*read-eval* :relatedto :concept/evaluation]
   [#'*file* :relatedto :concept/evaluation]
   [#'*source-path* :relatedto :concept/evaluation]
   ;;
   [#'load :relatedto #{:concept/evaluation #'loaded-libs}]
   [#'load-reader :relatedto :concept/evaluation]
   [#'load-file :relatedto :concept/evaluation]
   [#'load-string :relatedto :concept/evaluation]
   ;;
   [#'read]
   [#'read-string]

   [#'count :accepts #{:types/strings :ds/arrays :ds/collections :ds/maps :concept/nil}
    :relatedto clojure.lang.Counted]

   [#'false? :relatedto :types/boolean]
   [#'true? :relatedto :types/boolean]
   [#'nil? :relatedto :concept/nil]
   [#'string? :relatedto :types/strings]
   [#'char? :relatedto :types/char]

   [#'counted? :relatedto Counted]
   [#'seq? :relatedto ISeq]
   [#'special-symbol? :relatedto :concept/special-forms]
   [#'map? :relatedto IPersistentMap]
   [#'set? :relatedto IPersistentSet]
   [#'class? :relatedto Class]
   [#'reversible? :relatedto Reversible]
   [#'sequential? :relatedto Sequential]
   [#'coll? :relatedto IPersistentCollection]
   [#'associative? :relatedto Associative]
   [#'fn? :relatedto Fn]
   [#'ifn? :relatedto IFn]

   [{:type :concept :id :concept/IO :name "I/O"} :relatedto :clojure/root]
   [#'spit :relatedto :concept/IO]
   [#'slurp :implements :concept/IO]
   [#'read-line :implements :concept/IO]
   [#'with-in-str :relatedto :concept/IO]
   [#'with-out-str :relatedto :concept/IO]
   [#'with-open :relatedto :concept/IO]
   [#'*out* :relatedto #{:concept/IO :concept/printing}]
   [#'*err* :relatedto #{:concept/IO :concept/printing}]
   [#'*in* :relatedto :concept/IO]


   [#'format :returns :types/strings]
   ;
   [#'newline :relatedto :concept/text-IO]
   [#'flush :relatedto :concept/text-IO]
   ;
   ;; TODO something here about "printing readably?"  Relate pr et al. to read?
   [{:type :concept :id :concept/printing :name "Printing"}
    :typeof :concept/IO]
   [#'pr :relatedto :concept/printing]
   [#'println :relatedto :concept/printing]
   [#'printf :relatedto :concept/printing]
   [#'pr-str :relatedto :concept/printing]
   [#'print-dup :relatedto #{:concept/printing :concept/reader-syntax}]
   [#'prn-str :relatedto :concept/printing]
   [#'print-str :relatedto :concept/printing]
   [#'print :relatedto :concept/printing]
   [#'prn :relatedto :concept/printing]
   [#'print-simple :relatedto :concept/printing]
   [#'println-str :relatedto :concept/printing]
   [#'*flush-on-newline* :relatedto #{#'prn #'println #'prn-str}]
   [#'*print-meta* :relatedto #{:concept/printing :concept/metadata}]
   [#'*print-readably* :relatedto #{:concept/reader-syntax :concept/printing}]
   [#'*print-level* :relatedto :concept/printing :isa :concept/repl-binding]
   [#'*print-length* :relatedto :concept/printing :isa :concept/repl-binding]
   [#'*print-dup*  :isa :concept/repl-binding :relatedto #'print-dup]

   [{:type :concept :id :ds/seqs :name "Seqs"}
    :relatedto :ds/collections
    :implementedby clojure.lang.ISeq]
   [{:type :concept :id :ds/lazy-seqs :name "Lazy Seqs"}
    :isa :ds/seqs]

   [#'seq :returns :ds/seqs :relatedto clojure.lang.Seqable]
   [#'rseq :returns :ds/seqs :accepts :ds/sorted-collections]

   [#'lazy-seq :creates :ds/lazy-seqs]
   [#'lazy-cat :creates :ds/lazy-seqs :relatedto #'concat]
   [#'concat :relatedto :ds/seqs :creates :ds/lazy-seqs :isa :concept/accumulation]

   [{:type :concept :id :concept/seq-ops :name "Seq Operations"}
    :relatedto :ds/seqs]

   [{:type :concept :id :concept/creating-seqs :name "Creating Seqs"}
    :relatedto #{:ds/seqs :ds/lazy-seqs}]
   [#'rsubseq :relatedto #'subseq :accepts :ds/sorted-collections :isa :concept/creating-seqs]
   [#'subseq :accepts :ds/sorted-collections :isa :concept/creating-seqs]
   [#'iterate :isa :concept/creating-seqs]
   [#'replicate :isa :concept/creating-seqs]
   [#'repeat :isa :concept/creating-seqs]
   [#'range :isa :concept/creating-seqs]
   [#'repeatedly :isa :concept/creating-seqs]
   [#'cycle :creates :ds/lazy-seqs]

   [{:type :concept :id :concept/x-to-seq :name "Specialized Seq Creation"}
    :relatedto :concept/creating-seqs]
   [#'tree-seq :isa :concept/x-to-seq]
   [#'file-seq :isa :concept/x-to-seq :accepts java.io.File]
   [#'xml-seq :isa :concept/x-to-seq :accepts #{java.io.File java.io.InputStream String}]
   [#'resultset-seq :isa :concept/x-to-seq :accepts java.sql.ResultSet]
   [#'iterator-seq :isa :concept/x-to-seq :accepts java.util.Iterator]
   [#'enumeration-seq :isa :concept/x-to-seq :accepts java.util.Enumeration]
   [#'line-seq :isa :concept/x-to-seq :accepts java.io.BufferedReader]

   [#'sequence :relatedto #'seq]

   [{:type :concept :id :concept/splitting-seqs :name "Splitting Seqs"}
    :typeof :concept/seq-ops]
   [#'partition :isa :concept/splitting-seqs]
   [#'partition-by :isa :concept/splitting-seqs]
   [#'partition-all :isa :concept/splitting-seqs]
   [#'split-at :isa :concept/splitting-seqs]
   [#'split-with :isa :concept/splitting-seqs]
   [#'interpose :isa :concept/splitting-seqs]
   [#'interleave :isa :concept/splitting-seqs]

   [{:type :concept :id :concept/seq-prefix :name "Seq Prefix"}
    :typeof :concept/seq-ops]
   [#'take :isa :concept/seq-prefix]
   [#'take-last :isa :concept/seq-prefix]
   [#'take-nth :isa :concept/seq-prefix]
   [#'take-while :isa :concept/seq-prefix]
   [#'butlast :isa :concept/seq-prefix]

   [{:type :concept :id :concept/seq-suffix :name "Seq Suffix"}
    :typeof :concept/seq-ops]
   [#'drop :isa :concept/seq-suffix]
   [#'drop-last :isa :concept/seq-suffix]
   [#'drop-while :isa :concept/seq-suffix]
   [#'rest :isa :concept/seq-suffix]
   [nthrest :isa :concept/seq-suffix]

   [{:type :concept :id :concept/seq-access :name "Item from Seq"}
    :typeof :concept/seq-ops]
   [#'last :isa :concept/seq-access]
   [#'nthnext :isa :concept/seq-access]
   [#'fnext :isa :concept/seq-access]
   [#'nnext :isa :concept/seq-access]
   [#'nth :relatedto :concept/seq-access]
   [#'nfirst :isa :concept/seq-access]
   [#'first :relatedto :concept/seq-access]
   [#'ffirst :isa :concept/seq-access]
   [#'second :relatedto :concept/seq-access]
   [#'next :isa :concept/seq-access]

   [{:type :concept :id :concept/seq-transforms :name "Transforming Seqs"}
    :typeof :concept/seq-ops]
   [#'map :isa :concept/seq-transforms]
   [#'mapcat :relatedto #{#'map #'concat} :isa :concept/seq-transforms]
   [#'map-indexed :relatedto #{#'map #'keep-indexed} :isa :concept/seq-transforms]
   [#'filter :isa :concept/seq-transforms :complements #'remove]
   [#'remove :isa :concept/seq-transforms]
   [#'reduce :isa :concept/seq-transforms :relatedto #'reductions]
   [#'reductions :relatedto #'reduce :isa :concept/seq-transforms]
   [#'keep :relatedto #{#'map #'keep-indexed} :isa :concept/seq-transforms]
   [#'keep-indexed :isa :concept/seq-transforms]
   [#'replace :isa :concept/seq-transforms]
   [#'distinct :isa :concept/seq-transforms]

   [#'zipmap :isa :concept/seq-transforms]
   [#'frequencies :isa :concept/seq-transforms]

   [{:type :concept :id :concept/threading-macros :name "Threading macros"}]
   [#'-> :isa :concept/threading-macros]
   [#'->> :isa #{:concept/threading-macros} :relatedto :concept/seq-ops]

   [{:type :concept :id :concept/seq-search :name "Searching Seqs"}
    :typeof :concept/seq-ops]
   [#'some :isa :concept/seq-search]
   [#'every? :isa :concept/seq-search]
   [#'not-every? :isa :concept/seq-search]
   [#'not-any? :isa :concept/seq-search]
   [#'min-key :isa :concept/seq-search :relatedto #'min]
   [#'max-key :isa :concept/seq-search :relatedto #'max]

   [{:type :concept :id :concept/sorting :name "Sorting"}]
   [#'sort :provides :concept/sorting :returns :ds/seqs]
   [#'sort-by :provides :concept/sorting :returns :ds/seqs]
   [#'compare :relatedto :concept/sorting]
   [#'comparator :relatedto :concept/sorting :returns java.util.Comparator]
   [#'reverse :provides :concept/sorting :returns :ds/seqs]

   [filterv :relatedto #{#'filter :ds/vectors}]
   [mapv :relatedto #{#'map :ds/vectors}]
   [reduce-kv :relatedto #{#'reduce :ds/associative}]

   [{:type :concept :id :concept/accumulation :name "\"Adds to\" a collection"}
    :typeof :ds/seqs]
   [#'conj :isa :concept/accumulation]
   [#'into :isa :concept/accumulation]

   [#'clojure.set/union :isa :concept/accumulation :relatedto :ds/sets]
   [#'clojure.set/difference :relatedto :ds/sets]
   [#'clojure.set/intersection :relatedto :ds/sets]
   [#'clojure.set/rename-keys :relatedto #{:ds/maps}]
   [#'clojure.set/subset? :relatedto :ds/sets]
   [#'clojure.set/superset? :relatedto :ds/sets]

   [{:type :concept :id :concept/relalgebra :name "Relational Algebra"}]
   [#'clojure.set/select :relatedto #{:concept/relalgebra :ds/sets}]
   [#'clojure.set/rename :relatedto #{:concept/relalgebra :ds/sets}]
   [#'clojure.set/index :relatedto #{:concept/relalgebra :ds/sets}]
   [#'clojure.set/map-invert :relatedto #{:concept/relalgebra :ds/sets}]
   [#'clojure.set/project :relatedto #{:concept/relalgebra :ds/sets}]
   [#'clojure.set/join :relatedto #{:concept/relalgebra :ds/sets}]

   [{:type :concept :id :ds/chunked-seqs :name "Chunked Seqs"}
    :typeof :ds/seqs]
   [#'chunk :relatedto :ds/chunked-seqs]
   [#'chunk-rest :relatedto :ds/chunked-seqs]
   [#'chunk-append :relatedto :ds/chunked-seqs]
   [#'chunk-cons :relatedto :ds/chunked-seqs]
   [#'chunk-buffer :relatedto :ds/chunked-seqs]
   [#'chunked-seq? :relatedto :ds/chunked-seqs]
   [#'chunk-next :relatedto :ds/chunked-seqs]
   [#'chunk-first :relatedto :ds/chunked-seqs]

   [#'with-loading-context :isa :concept/impl-detail]
   [#'*allow-unresolved-vars* :isa :concept/impl-detail]
   [#'method-sig :isa :concept/impl-detail]
   [#'print-method :isa :concept/impl-detail]
   [syntax-symbol-anchor :isa :concept/impl-detail]
   [special-form-anchor :isa :concept/impl-detail]
   [#'print-ctor :isa :concept/impl-detail]
   [#'primitives-classnames :isa :concept/impl-detail]
   [#'EMPTY-NODE :isa :concept/impl-detail]
   [#'hash-combine :isa :concept/impl-detail]
   [#'munge :isa :concept/impl-detail]
   [#'destructure :isa :concept/impl-detail :relatedto #'let]
   [clojure.core/->ArrayChunk :isa :concept/impl-detail]
   [clojure.core/->Vec :isa :concept/impl-detail]
   [clojure.core/->VecNode :isa :concept/impl-detail]
   [clojure.core/->VecSeq :isa :concept/impl-detail]

  ;;[#'clojure.core/*clojure-version*]
  ;;[#'clojure.core/*command-line-args*]
  ;;[#'clojure.core/*compiler-options*]

  ;;[#'clojure.core/*fn-loader*]
  ;;[#'clojure.core/*use-context-classloader*]
  ;;[#'clojure.core/*verbose-defrecords*]

  ^{:version "1.4.0"}
  [{:type :concept :id :concept/promoting-arithmetic-ops :name "Promoting arithmetic operators"}
   :relatedto #{:concept/arithmetic :concept/numbers}]
  [clojure.core/*' :isa :concept/promoting-arithmetic-ops]
  [clojure.core/+' :isa :concept/promoting-arithmetic-ops]
  [clojure.core/-' :isa :concept/promoting-arithmetic-ops]
  [clojure.core/dec' :isa :concept/promoting-arithmetic-ops]
  [clojure.core/inc' :isa :concept/promoting-arithmetic-ops]
  ])


(defn- var-name
  [^clojure.lang.Var v]
  (str (-> v .ns ns-name) \/ (.sym v)))

(def orphans (->> publics
               (remove (into #{} (map first ontology)))
               (sort-by var-name)))

;; this "known var" removal is purely an optimization to avoid having to cycle through
;; all vars twice when generating the model
(def ontology (let [known-vars (into #{} (map first ontology))]
                (->> (remove known-vars publics)
                  (concat all-classes)
                  (map vector)
                  (into ontology))))
