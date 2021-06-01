(ns mudguard.gen
  (:require [mudguard.core :as c]
            [clojure.test.check.generators :as g]
            [clojure.string :as str])
  (:import (java.util Date)))

(def max-tries 5)

(defn join-ids [tw separator validatorA validatorB]
  (->> [validatorA validatorB]
       (map #(c/validator-eval % tw))
       (remove nil?)
       (map name)
       (str/join separator)
       keyword))

(defrecord IdWalker []
  c/TreeWalker
  (-mempty [_this]
    :clojure.core/any?)
  (-leaf [_this id _vfn _constraints]
    id)
  (-at [_this id _key _validator _optional?]
    id)
  (-chain [this _validatorA validatorB]
    (c/validator-eval validatorB this))
  (-group [this _validatorA validatorB]
    (c/validator-eval validatorB this))
  (-fmap [this validator _f]
    (c/validator-eval validator this))
  (-each [this validator]
    (c/validator-eval validator this))
  ;; TODO rename to tuple?
  (-entry [this key-validator val-validator]
    (join-ids this ":" key-validator val-validator))
  (-map [_this _validator-map]
    :-map
    )
  (-one-of [this validatorA validatorB]
    (join-ids this "|" validatorA validatorB)))

(defn get-generator-id [validator]
  (c/validator-eval validator (IdWalker.)))

(defn validator-to-pred [validator]
  (fn [x]
    (not (c/error? (c/coerce validator x)))))

(defn such-that [id pred gen]
  (g/such-that pred
               gen
               {:max-tries max-tries
                :ex-fn     (fn [m] (ex-info (format "Couldn't gen value for validator %s, after %s attempts" id max-tries)
                                            (merge m {:id      id})))}))

(defn such-that-both [idA idB genA predB]
  (g/such-that predB
               genA
               {:max-tries max-tries
                :ex-fn     (fn [m] (ex-info (format "Couldn't generate a value for validator %s that also satisfies %s, after %s attempts" idA idB max-tries)
                                            (merge m {:idA     idA
                                                      :idB     idB})))}))

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
        genB (look-up-lib generator-lib validatorB)
        parseA #(c/coerce validatorA %)]
    (if genB
      genB
      (let [predB (validator-to-pred validatorB)]
        (such-that-both vAId vBId genA (comp predB parseA))))))

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

(defn- gen-opt-key [map-gen key v-gen]
  (g/let [m map-gen
          v v-gen
          b g/boolean]
         (if b
           (assoc m key v)
           m)))

(defn gen-key [map-gen key v-gen]
  (g/let [m map-gen
          v v-gen]
         (assoc m key v)))

(defn gen-entries [map-gen key-generator val-generator]
  (g/let [m1 (g/map key-generator val-generator)
          m2 map-gen]
         (merge m1 m2)))

(defn- map-gen
  [tw validator-map]
  (->> validator-map
       (reduce (fn [g [k v]]
                 (let [val-gen (c/validator-eval v tw)]
                   (cond (c/is-required-key? k)
                         (gen-key g (c/unwrap-key k) val-gen)
                         (c/is-optional-key? k)
                         (gen-opt-key g (c/unwrap-key k) val-gen)
                         (c/is-validator? k)
                         (gen-entries g (c/validator-eval k tw) val-gen)
                         :else (gen-key g k val-gen))))
               (g/return {}))))

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
    (one-of-gen this validatorA validatorB))
  (-map [this validator-map]
    (map-gen this validator-map)))

(def default-generators
  {:clojure.core/int?     g/small-integer
   :clojure.core/string?  g/string
   :clojure.core/boolean? g/boolean
   :clojure.core/keyword? (g/one-of [g/keyword g/keyword-ns])
   :clojure.core/number?  (g/one-of [g/double g/small-integer])
   :clojure.core/float?   g/double
   :clojure.core/inst?    (g/return (Date.))
   :clojure.core/nil?     (g/return nil)
   :clojure.core/nat-int? g/nat
   })

(defn generator
  ([validator]
   (generator validator {}))
  ([validator override-generators]
   (let [generators (merge default-generators override-generators)]
     (g/fmap
       (fn [v]
         (c/coerce-or-throw validator v "Generated value is invalid - check that your custom generators always produce valid values")
         v)
       (c/validator-eval validator (GenWalker. generators))))))