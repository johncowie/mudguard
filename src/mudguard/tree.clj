(ns mudguard.tree
  (:require [clojure.set :as set]
            [mudguard.result :as r]
            [clojure.string :as str])
  (:import (clojure.lang APersistentVector APersistentMap IFn)))

(defprotocol TreeWalker
  (-mempty [this])
  (-leaf [this id vfn constraints])
  (-at [this id key validator optional?])
  (-chain [this validatorA validatorB])
  (-group [this validatorA validatorB])
  (-entry [this key-validator val-validator])
  (-fmap [this validator f])
  (-each [this validator])
  (-one-of [this validatorA validatorB]))

(defprotocol TreeEval
  (validator-eval [this tree-walker]))

(defn tree-validate [walker validator v]
  ((validator-eval validator walker) v))

(defrecord ValidatorWalker []
  TreeWalker
  (-mempty [this]
    identity)
  (-leaf [_this id vfn constraints]
    (fn [v]
      (try
        (let [return-value (vfn constraints v)]
          (if (r/success-value? return-value)
            (r/extract-success-val return-value)
            (r/validation-error [id] v constraints)))
        (catch Exception _e
          (r/validation-error [id] v constraints)))))
  (-at [this id key validator optional?]
    (fn [v]
      (if (and (associative? v) (contains? v key))
        (let [x (get v key)
              res (tree-validate this validator x)]
          (if (r/error? res)
            (r/errors-at id res)
            (assoc v key res)))
        (if optional?
          v
          (r/validation-error [id :missing] nil {})))))
  (-chain [this validatorA validatorB]
    (fn [v]
      (let [rA (tree-validate this validatorA v)]
        (if (r/error? rA)
          rA
          (tree-validate this validatorB rA)))))
  (-group [this validatorA validatorB]
    (fn [v]
      (let [rA (tree-validate this validatorA v)]
        (if (r/error? rA)
          (let [rB (tree-validate this validatorB v)]
            (if (r/error? rB)
              (r/join-errors rA rB)
              rA))
          (tree-validate this validatorB rA)))))
  (-each [this validator]
    (fn [v]
      (->> v
           (map (partial tree-validate this validator))
           (map-indexed vector)
           (reduce (fn [acc [i v]]
                     (if (r/error? acc)
                       (if (r/error? v)
                         (r/join-errors acc (r/errors-at i v))
                         acc)
                       (if (r/error? v)
                         (r/errors-at i v)
                         (conj acc v)))) []))))
  (-entry [this key-validator val-validator]
    (fn [[k v]]
      (let [key-res (tree-validate this key-validator k)]
        (if (r/error? key-res)
          (r/errors-at :-key key-res)
          (let [val-res (tree-validate this val-validator v)]
            (if (r/error? val-res)
              (r/errors-at :-value val-res)
              [key-res val-res]))))))
  (-fmap [this validator f]
    (fn [v]
      (let [res (tree-validate this validator v)]
        (if (r/error? res)
          res
          (f res)))))
  (-one-of [this validatorA validatorB]
    (fn [v]
      (let [rA (tree-validate this validatorA v)]
        (if (r/error? rA)
          (tree-validate this validatorB v)
          rA)))))

(defrecord PossibleErrorsWalker []
  TreeWalker
  (-mempty [this]
    (r/validation-errors))
  (-leaf [_this id _vfn constraints]
    (r/sample-error [id] constraints))
  (-at [this id _key validator optional?]
    (if optional?
      (r/errors-at id (validator-eval validator this))
      (r/join-errors
        (r/sample-error [id :missing])
        (r/errors-at id (validator-eval validator this)))))
  (-chain [this validatorA validatorB]
    (r/join-errors
      (validator-eval validatorA this)
      (validator-eval validatorB this)))
  (-group [this validatorA validatorB]
    (r/join-errors
      (validator-eval validatorA this)
      (validator-eval validatorB this)))
  (-fmap [this validator _f]
    (validator-eval validator this))
  (-entry [this key-validator val-validator]
    (r/join-errors
      (r/errors-at :-key (validator-eval key-validator this))
      (r/errors-at :-value (validator-eval val-validator this))))
  (-each [this validator]
    (validator-eval validator this))
  (-one-of [this _validatorA validatorB]
    (validator-eval validatorB this)))

(defrecord MemptyEval []
  TreeEval
  (validator-eval [_this tree-walker]
    (-mempty tree-walker)))

(defrecord ValidatorEval [id constraints vfn]
  TreeEval
  (validator-eval [_this tree-walker]
    (-leaf tree-walker id vfn constraints)))

(defn get-constraints [validator]
  (if (instance? ValidatorEval validator)
    (:constraints validator)
    (throw (Exception. "Can't retrieve constraints from composite validator"))))

(defrecord AtEval [id key validator optional?]
  TreeEval
  (validator-eval [_this tree-walker]
    (-at tree-walker id key validator optional?)))

(defrecord ChainEval [validatorA validatorB]
  TreeEval
  (validator-eval [_this tree-walker]
    (-chain tree-walker validatorA validatorB)))

(defrecord GroupEval [validatorA validatorB]
  TreeEval
  (validator-eval [_this tree-walker]
    (-group tree-walker validatorA validatorB)))

(defrecord FmapEval [validator f]
  TreeEval
  (validator-eval [_this tree-walker]
    (-fmap tree-walker validator f)))

(defrecord EachEval [validator]
  TreeEval
  (validator-eval [_this tree-walker]
    (-each tree-walker validator)))

(defrecord OneOfEval [validatorA validatorB]
  TreeEval
  (validator-eval [_this tree-walker]
    (-one-of tree-walker validatorA validatorB)))

(defrecord EntryEval [key-validator val-validator]
  TreeEval
  (validator-eval [_this tree-walker]
    (-entry tree-walker key-validator val-validator)))

;; Constructor functions
(defn validate [validator data]
  (let [validator-fn (validator-eval validator (ValidatorWalker.))]
    (validator-fn data)))

(defn validator [id constraints fn]
  (assert (keyword? id) (str "Validator ID must be a keyword - was " id))
  (ValidatorEval. id constraints fn))

(defn predicate
  [id predicate-fn]
  (validator id {} (fn [_ v]
                     (when (predicate-fn v)
                       (r/success-value v)))))

(defn parser [id parser-fn]
  (validator id {} (fn [_ v]
                     (let [parsed (parser-fn v)]
                       (when-not (nil? parsed)
                         (r/success-value parsed))))))

(def Associative (predicate :clojure.core/associative? associative?))
(def Coll (predicate :clojure.core/coll? coll?))

(defn chain [validator1 validator2 & validators]
  (->> validators
       (concat [validator1 validator2])
       (reduce (fn [v1 v2] (ChainEval. v1 v2)))))

(defn group [validator1 validator2 & validators]
  (->> validators
       (concat [validator1 validator2])
       (reduce (fn [v1 v2] (GroupEval. v1 v2)))))

(defn at
  ([key validator]
   (at key key validator))
  ([id key validator]
   (assert (keyword? id) (str "Validator ID must be a keyword - was " id))
   (AtEval. id key validator false)))

(defn opt-at
  ([key validator]
   (opt-at key key validator))
  ([id key validator]
   (assert (keyword? id) (str "Validator ID must be a keyword - was " id))
   (AtEval. id key validator true)))


(defn fmap [validator f]
  (FmapEval. validator f))

(defn each [validator]
  ;; TODO chain with check that value is a collection
  (chain
    Coll
    (EachEval. validator)))

(defn one-of [validator1 validator2 & validators]
  (->> validators
       (concat [validator1 validator2])
       (reduce (fn [v1 v2] (OneOfEval. v1 v2)))))

(defn- entry [key-validator val-validator]
  ;; TODO chain with check value is a tuple and value has 2 values
  (EntryEval. key-validator val-validator))

(defn validate [validator data]
  (let [validator-fn (validator-eval validator (ValidatorWalker.))]
    (validator-fn data)))

(defn possible-errors [validator]
  (validator-eval validator (PossibleErrorsWalker.)))


;; Collection shenanigans

(extend-type APersistentVector
  TreeEval
  (validator-eval [this tree-walker]
    (validator-eval (each (first this)) tree-walker)))

(defrecord RequiredKey [id key])
(defrecord OptionalKey [id key])

(defn required-key
  ([k]
   (required-key k k))
  ([id k]
   (RequiredKey. id k)))

(defn is-required-key? [k]
  (instance? RequiredKey k))

(defn required-key-validator [{:keys [id key]} validator]
  (at id key validator))

(defn optional-key
  ([k]
   (optional-key k k))
  ([id k]
   (OptionalKey. id k)))

(defn is-optional-key? [k]
  (instance? OptionalKey k))

(defn unwrap-key [k]
  (if (or (is-required-key? k) (is-optional-key? k))
    (:key k)
    k))

(defn optional-key-validator [{:keys [id key]} validator]
  (opt-at id key validator))

(defn valid-key-validator [valid-keys]
  (validator ::invalid-key {:keys valid-keys}
             (fn [_constraints k]
               (when (contains? (set valid-keys) k)
                 (r/success-value k)))))

(def no-op-validator (MemptyEval.))

(defn entries-validator [m]
  ;; TODO (+ throw error if more than one generic key/val validator pair
  (let [valid-keys (->> m
                        keys
                        (map unwrap-key)
                        (remove #(satisfies? TreeEval %)))
        [keyv valv] (->> m
                         (filter (comp #(satisfies? TreeEval %) first))
                         first)
        valid-key-entry (entry (valid-key-validator valid-keys) no-op-validator)
        entry-validator (if keyv
                          (one-of valid-key-entry (entry keyv valv))
                          valid-key-entry)]
    (fmap (EachEval. entry-validator) #(into {} %))))

(defn- specific-key-validator [m]
  (->> m
       (map (fn [[k v]]
              (cond
                (is-optional-key? k) (optional-key-validator k v)
                (is-required-key? k) (required-key-validator k v)
                (satisfies? TreeEval k) nil
                :else (at k v))))
       (remove nil?)
       (reduce group no-op-validator)))

(defn map-validator [m]
  (chain (predicate :clojure.core/associative? associative?)
         (group (specific-key-validator m)
                (entries-validator m))))

(extend-type APersistentMap
  TreeEval
  (validator-eval [this tree-walker]
    (let [validator (map-validator this)]
      (validator-eval validator tree-walker))))