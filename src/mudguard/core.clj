(ns mudguard.core
  (:require [clojure.string :as str])
  (:import (clojure.lang APersistentVector APersistentMap)))

;; Result values
(defn validation-error
  ([id value]
   (validation-error id value {}))
  ([id value constraints]
   {::errors [{::id          id
               ::input       value
               ::constraints constraints}]}))

(defn sample-error
  ([id]
   (sample-error id {}))
  ([id constraints]
   (validation-error id ::sample-input constraints)))

(defn join-errors [errorA errorB]
  {::errors (concat (::errors errorA) (::errors errorB))})

(defn validation-errors [& errors]
  (reduce join-errors {::errors []} errors))

(defn error? [res]
  (some? (::errors res)))

(defn errors-at [key err]
  (update err ::errors (partial map (fn [e] (update e ::id (partial cons key))))))

(defn success-value [v]
  {::success v})

(defn success-value? [v]
  (and (associative? v) (contains? v ::success)))

(defn extract-success-val [v]
  (::success v))


;; Protocols Tree

(defprotocol TreeWalker
  (-mempty [this])
  (-leaf [this id vfn constraints])
  (-at [this id key validator optional?])
  (-chain [this validatorA validatorB])
  (-group [this validatorA validatorB])
  (-entry [this key-validator val-validator])
  (-fmap [this validator f])
  (-each [this validator])
  (-map [this validator-map])
  (-one-of [this validatorA validatorB]))

(defprotocol TreeEval
  (validator-eval [this tree-walker]))

;; Constructors

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

(defrecord MapEval [validator-map]
  TreeEval
  (validator-eval [_this tree-walker]
    (-map tree-walker validator-map)))

(def no-op-validator (MemptyEval.))

(defn validator [id constraints fn]
  (assert (keyword? id) (str "Validator ID must be a keyword - was " id))
  (ValidatorEval. id constraints fn))

(defn predicate
  [id predicate-fn]
  (validator id {} (fn [_ v]
                     (when (predicate-fn v)
                       (success-value v)))))

(defn parser [id parser-fn]
  (validator id {} (fn [_ v]
                     (let [parsed (parser-fn v)]
                       (when-not (nil? parsed)
                         (success-value parsed))))))

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
  (EachEval. validator))

(defn one-of [validator1 validator2 & validators]
  (->> validators
       (concat [validator1 validator2])
       (reduce (fn [v1 v2] (OneOfEval. v1 v2)))))

(defn entry [key-validator val-validator]
  (EntryEval. key-validator val-validator))

;;; Handy validators
(def Coll (predicate :clojure.core/coll? coll?))
(def Associative (predicate :clojure.core/associative? associative?))
(def Int (predicate :clojure.core/int? int?))
(def Bool (predicate :clojure.core/boolean? boolean?))
(def Str (predicate :clojure.core/string? string?))
(def Nil (predicate :clojure.core/nil? nil?))
(def Keyword (predicate :clojure.core/keyword? keyword?))
(def Any no-op-validator)


;; Map stuff

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
  (validator ::valid-key {:keys valid-keys}
             (fn [_constraints k]
               (when (contains? (set valid-keys) k)
                 (success-value k)))))

(defn is-validator? [v]
  (satisfies? TreeEval v))

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
    (fmap (each entry-validator) #(into {} %))))

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
  (chain Associative
         (group (specific-key-validator m)
                (entries-validator m))))

;; Implementation

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
          (if (success-value? return-value)
            (extract-success-val return-value)
            (validation-error [id] v constraints)))
        (catch Exception _e
          (validation-error [id] v constraints)))))
  (-at [this id key validator optional?]
    (fn [v]
      (if (and (associative? v) (contains? v key))
        (let [x (get v key)
              res (tree-validate this validator x)]
          (if (error? res)
            (errors-at id res)
            (assoc v key res)))
        (if optional?
          v
          (validation-error [id :missing] nil {})))))
  (-chain [this validatorA validatorB]
    (fn [v]
      (let [rA (tree-validate this validatorA v)]
        (if (error? rA)
          rA
          (tree-validate this validatorB rA)))))
  (-group [this validatorA validatorB]
    (fn [v]
      (let [rA (tree-validate this validatorA v)]
        (if (error? rA)
          (let [rB (tree-validate this validatorB v)]
            (if (error? rB)
              (join-errors rA rB)
              rA))
          (tree-validate this validatorB rA)))))
  (-each [this validator]
    (fn [v]
      (if-not (coll? v)
        (validation-error [:clojure.core/coll?] v)
        (->> v
             (map (partial tree-validate this validator))
             (map-indexed vector)
             (reduce (fn [acc [i v]]
                       (if (error? acc)
                         (if (error? v)
                           (join-errors acc (errors-at i v))
                           acc)
                         (if (error? v)
                           (errors-at i v)
                           (conj acc v)))) [])))))
  (-entry [this key-validator val-validator]
    ;; TODO check type
    (fn [[k v]]
      (let [key-res (tree-validate this key-validator k)]
        (if (error? key-res)
          (errors-at :-key key-res)
          (let [val-res (tree-validate this val-validator v)]
            (if (error? val-res)
              (errors-at :-value val-res)
              [key-res val-res]))))))
  (-fmap [this validator f]
    (fn [v]
      (let [res (tree-validate this validator v)]
        (if (error? res)
          res
          (f res)))))
  (-one-of [this validatorA validatorB]
    (fn [v]
      (let [rA (tree-validate this validatorA v)]
        (if (error? rA)
          (tree-validate this validatorB v)
          rA))))
  (-map [this validator-map]
    (fn [v]
      (let [validator (map-validator validator-map)]
        (tree-validate this validator v)))))

(defrecord PossibleErrorsWalker []
  TreeWalker
  (-mempty [this]
    (validation-errors))
  (-leaf [_this id _vfn constraints]
    (sample-error [id] constraints))
  (-at [this id _key validator optional?]
    (if optional?
      (errors-at id (validator-eval validator this))
      (join-errors
        (sample-error [id :missing])
        (errors-at id (validator-eval validator this)))))
  (-chain [this validatorA validatorB]
    (join-errors
      (validator-eval validatorA this)
      (validator-eval validatorB this)))
  (-group [this validatorA validatorB]
    (join-errors
      (validator-eval validatorA this)
      (validator-eval validatorB this)))
  (-fmap [this validator _f]
    (validator-eval validator this))
  (-entry [this key-validator val-validator]
    (join-errors
      (errors-at :-key (validator-eval key-validator this))
      (errors-at :-value (validator-eval val-validator this))))
  (-each [this validator]
    (join-errors
      (sample-error [:clojure.core/coll?])
      (validator-eval validator this)))
  (-one-of [this _validatorA validatorB]
    (validator-eval validatorB this))
  (-map [this validator-map]
    (let [validator (map-validator validator-map)]
      (validator-eval validator this))))

(defn validate [validator data]
  (let [validator-fn (validator-eval validator (ValidatorWalker.))]
    (validator-fn data)))

(defn throw-if-invalid
  ([validator data]
   (throw-if-invalid validator data "Validation failed."))
  ([validator data msg]
   (let [res (validate validator data)]
     (if (error? res)
       (throw (ex-info msg res))
       res))))

(defn possible-errors [validator]
  (validator-eval validator (PossibleErrorsWalker.)))


;; Collection shenanigans

(extend-type APersistentVector
  TreeEval
  (validator-eval [this tree-walker]
    (validator-eval (each (first this)) tree-walker)))

(extend-type APersistentMap
  TreeEval
  (validator-eval [this tree-walker]
    (validator-eval (MapEval. this) tree-walker)))

;;; Handy validators
(def Int (predicate :clojure.core/int? int?))
(def Bool (predicate :clojure.core/boolean? boolean?))
(def Str (predicate :clojure.core/string? string?))
(def Nil (predicate :clojure.core/nil? nil?))
(def Keyword (predicate :clojure.core/keyword? keyword?))
(def Num (predicate :clojure.core/number? number?))
(def FloatNum (predicate :clojure.core/float? float?))
(def Inst (predicate :clojure.core/inst? inst?))
(def Any no-op-validator)
;; TODO specify default generators here?

(def NotBlank (predicate :not-blank (complement str/blank?)))
(defn maybe [validator] (one-of Nil validator))

;; TODO
;(defn matches-regex [regex] Any)

;; TODO
;; - bunch of default validators
;; - equals value

;; TODO roadmap
;; Generators
;; Pre-compile
;; Simplify map shenanigans
;; Swagger
