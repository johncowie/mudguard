(ns mudguard.gen
  (:require [mudguard.tree :as t]
            [mudguard.core :as c]
            [clojure.test.check.generators :as g]
            [mudguard.result :as r]))

;; Generator logic
;; If generator use generated value
;; If function then apply function to generated value

(defn join-ids [tw separator validatorA validatorB]
  (keyword
    (str
      (name (t/validator-eval validatorA tw))
      separator
      (name (t/validator-eval validatorB tw)))))

(defrecord IdWalker []
  t/TreeWalker
  (-leaf [this id vfn constraints]
    id)
  (-at [this id key validator optional?]
    id)
  (-chain [this validatorA validatorB]
    (join-ids this ">" validatorA validatorB))
  (-group [this validatorA validatorB]
    (join-ids this "+" validatorA validatorB))
  (-fmap [this validator _f]
    (t/validator-eval validator this))
  (-each [this validator]
    (t/validator-eval validator this))
  (-one-of [this validatorA validatorB]
    (join-ids this "|" validatorA validatorB)))

(defn get-generator-id [validator]
  (t/validator-eval validator (IdWalker.)))

(defn validator-to-pred [validator]
  (fn [x]
    (not (r/error? (t/validate validator x)))))

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
                :ex-fn     (fn [m] (ex-info (format "Couldn't gen value for validator %s that also was also valid for %s" idA idB)
                                            (merge m {:idA     idA
                                                      :idB     idB
                                                      :example (first (g/sample genA))})))}))

(defn look-up-lib [generator-lib validator]
  (let [id (get-generator-id validator)]
    (when-let [v (get generator-lib id)]
      (if (fn? v)
        (let [f (v validator)]
          {::gen g/any
           ::fn  f})
        {::gen v}))))

(defn- validator-gen [id vfn constraints generator-lib]
  (let [validator (t/validator id constraints vfn)]
    (if-let [generator (look-up-lib generator-lib validator)]
      generator
      (let [pred (validator-to-pred validator)]
        {::gen (such-that id pred g/any)}))))

(defn- at-gen [tw key validator]
  (let [{::keys [gen]} (t/validator-eval validator tw)
        conformer (fn [m]
                    (g/fmap (fn [v] (assoc m key v)) gen))]
    {::gen (g/bind (g/map g/any g/any) conformer)
     ::fn  conformer}))

(defn opt-bind-fn [f]
  "Sometimes bind f to generator, sometimes just return generator"
  (fn [v]
    (g/bind g/boolean #(if % (f v) (g/return v)))))

(defn- at-opt-gen [tw key validator]
  (let [{atFn ::fn} (at-gen tw key validator)
        conformer (opt-bind-fn atFn)]
    {::gen (g/bind (g/map g/any g/any) conformer)
     ::fn  conformer}))

(defn- chain-gen [tw validatorA validatorB generator-lib]
  "Generate a value for conforms to both validatorA and validatorB"
  (let [vAId (get-generator-id validatorA)
        vBId (get-generator-id validatorB)
        {genA ::gen} (or (look-up-lib generator-lib validatorA)
                         (t/validator-eval validatorA tw))
        {fnB ::fn} (or (t/validator-eval validatorB tw)
                       (look-up-lib generator-lib validatorB))
        parseA #(c/validate validatorA %)]
    (if fnB
      {::gen (g/bind genA fnB)}
      (let [predB (validator-to-pred validatorB)]
        {::gen (such-that-both vAId vBId genA (comp predB parseA))}))))
;; TODO need id and way to look up value from generator lib
;; get ID of first validator
;; see if there's a generator in the lib
;; try running through the pred
;; if it can't gen a value, then request a value that works for both validator IDs


(defn- each-gen
  "Generate a list of validators"
  [tw validator]
  (let [{::keys [gen]} (t/validator-eval validator tw)]
    {::gen (g/vector gen)}))

(defn- fmap-gen
  [tw validator f]
  (let [{::keys [gen]} (t/validator-eval validator tw)]
    {::gen (g/fmap f gen)}))

(defn- one-of-gen
  "Generate for one of the two validators"
  [tw validatorA validatorB]
  (let [{genA ::gen} (t/validator-eval validatorA tw)
        {genB ::gen} (t/validator-eval validatorB tw)]
    {::gen (g/one-of [genA genB])}))

(defrecord GenWalker [generator-lib]
  t/TreeWalker
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
   :seven                 (fn [_]
                            (fn [_]
                              (g/return 7)))
   ::t/invalid-keys       (fn [validator]
                            (fn [v]
                              (let [{:keys [keys]} (t/get-constraints validator)]
                                (g/return (select-keys v keys)))))
   })

(defn generator
  ([validator]
   (generator validator {}))
  ([validator override-generators]
   (let [generators (merge default-generators override-generators)]
     (::gen (t/validator-eval validator (GenWalker. generators))))))

;; TODO id walker?
;; TOOD parsing???
;; What to do if lib generator produces invalid values? ;; FIXME deal with this next..

;; FIXME This example fail because seven's aren't being produced
(g/sample (generator {:a                  c/Int
                      :b                  (c/predicate :seven #(= % 7))
                      (c/optional-key :c) c/Int
                      (c/optional-key :d) c/Nil
                      :e                  [{:e1 c/Str
                                            :e2 c/Str}]}))

;; Things to fix
;;   Getting ID