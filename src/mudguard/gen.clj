(ns mudguard.gen
  (:require [mudguard.core :as c]
            [clojure.test.check.generators :as g]
            [clojure.string :as str]))

;; Generator logic
;; If generator use generated value
;; If function then apply function to generated value

(defn join-ids [tw separator validatorA validatorB]
  (->> [validatorA validatorB]
       (map #(c/validator-eval % tw))
       (remove nil?)
       (map name)
       (str/join separator)
       keyword))

(defrecord IdWalker []
  c/TreeWalker
  (-mempty [this]
    nil)
  (-leaf [this id vfn constraints]
    id)
  (-at [this id key validator optional?]
    id)
  (-chain [this validatorA validatorB]
    (join-ids this ">" validatorA validatorB))
  (-group [this validatorA validatorB]
    (join-ids this "+" validatorA validatorB))
  (-fmap [this validator _f]
    (c/validator-eval validator this))
  (-each [this validator]
    (c/validator-eval validator this))
  ;; TODO rename to tuple?
  (-entry [this key-validator val-validator]
    (join-ids this ":" key-validator val-validator))
  (-map [this validator-map]
    ;; reuse the logic below for specific key-val stuff
    )
  (-one-of [this validatorA validatorB]
    (join-ids this "|" validatorA validatorB)))

(defn get-generator-id [validator]
  (c/validator-eval validator (IdWalker.)))

(defn validator-to-pred [validator]
  (fn [x]
    (not (c/error? (c/validate validator x)))))

(defn such-that [id pred gen]
  (g/such-that pred
               gen
               {:max-tries 10
                :ex-fn     (fn [m] (ex-info (format "Couldn't gen value for validator %s" id)
                                            (merge m {:id      id
                                                      :example (first (g/sample gen))})))}))

(defn such-that-both [idA idB genA predB]
  (g/such-that predB
               genA
               {:max-tries 10
                :ex-fn     (fn [m] (ex-info (format "Couldn't gen value for validator %s that also satisfies %s" idA idB)
                                            (merge m {:idA     idA
                                                      :idB     idB
                                                      :example (first (g/sample genA))})))}))

(defn look-up-lib [generator-lib validator]
  (let [id (get-generator-id validator)]
    (when-let [v (get generator-lib id)]
      (if (fn? v)
        (v validator)
        v))))

(defn- validator-gen [id vfn constraints generator-lib]
  (let [validator (c/validator id constraints vfn)]
    (if-let [generator (look-up-lib generator-lib validator)]
      generator
      (let [pred (validator-to-pred validator)]
        (such-that id pred g/any)))))

(defn- at-gen [tw key validator]
  (let [gen (c/validator-eval validator tw)]
    (g/fmap (fn [v] {key v}) gen)))

(defn- at-opt-gen [tw key validator]
  (g/one-of [(g/return {}) (at-gen tw key validator)]))

(defn- chain-gen [tw validatorA validatorB generator-lib]
  "Generate a value for conforms to both validatorA and validatorB"
  (let [vAId (get-generator-id validatorA)
        vBId (get-generator-id validatorB)
        genA (or (look-up-lib generator-lib validatorA)
                         (c/validator-eval validatorA tw))
        parseA #(c/validate validatorA %)]
    (let [predB (validator-to-pred validatorB)]
      (such-that-both vAId vBId genA (comp predB parseA)))))
;; TODO need id and way to look up value from generator lib
;; get ID of first validator
;; see if there's a generator in the lib
;; try running through the pred
;; if it can't gen a value, then request a value that works for both validator IDs


(defn- each-gen
  "Generate a list of validators"
  [tw validator]
  (let [gen (c/validator-eval validator tw)]
    (g/vector gen)))

(defn- fmap-gen
  [tw validator f]
  (let [gen (c/validator-eval validator tw)]
    (g/fmap f gen)))

(defn- one-of-gen
  "Generate for one of the two validators"
  [tw validatorA validatorB]
  (let [genA (c/validator-eval validatorA tw)
        genB (c/validator-eval validatorB tw)]
    (g/one-of [genA genB])))

(defrecord GenWalker [generator-lib]
  c/TreeWalker
  (-mempty [_this]
    g/any)
  (-leaf [_this id vfn constraints]
    (validator-gen id vfn constraints generator-lib))
  (-at [this _id key validator optional?]
    (if optional?
      (at-opt-gen this key validator)
      (at-gen this key validator)))
  (-chain [this validatorA validatorB]
    (chain-gen this validatorA validatorB generator-lib))
  (-group [this validatorA validatorB]
    (chain-gen this validatorA validatorB generator-lib))
  (-each [this validator]
    (each-gen this validator))
  (-entry [this key-validator val-validator]
    (g/tuple (c/validator-eval key-validator this)
             (c/validator-eval val-validator this)))
  (-fmap [this validator f]
    (fmap-gen this validator f))
  (-one-of [this validatorA validatorB]
    (one-of-gen this validatorA validatorB)))

(def default-generators
  {:clojure.core/int?     g/small-integer
   :clojure.core/string?  g/string
   :clojure.core/boolean? g/boolean
   :clojure.core/keyword? g/keyword
   :clojure.core/nil?     (g/return nil)
   ::c/valid-key          g/any
   })

(defn generator
  ([validator]
   (generator validator {}))
  ([validator override-generators]
   (let [generators (merge default-generators override-generators)]
     (c/validator-eval validator (GenWalker. generators)))))

;; TODO id walker?
;; TOOD parsing???
;; What to do if lib generator produces invalid values? ;; FIXME deal with this next..

;; FIXME This example fail because seven's aren't being produced
;(g/sample (generator {:a                  c/Int
;                      :b                  (c/predicate :seven #(= % 7))
;                      (c/optional-key :c) c/Int
;                      (c/optional-key :d) c/Nil
;                      :e                  [{:e1 c/Str
;                                            :e2 c/Str}]}))

;; Things to fix
;;   Getting ID