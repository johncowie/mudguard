(ns mudguard.tree
  (:require [clojure.set :as set]
            [mudguard.result :as r]
            [clojure.string :as str])
  (:import (clojure.lang APersistentVector APersistentMap IFn)))

(defprotocol TreeWalker
  (-leaf [this id vfn constraints])
  (-at [this id key validator optional?])
  (-chain [this validatorA validatorB])
  (-group [this validatorA validatorB])
  (-each [this validator])
  (-one-of [this validatorA validatorB]))

(defprotocol TreeEval
  (validator-eval [this tree-walker]))

(defn tree-validate [walker validator v]
  ((validator-eval validator walker) v))

(defrecord ValidatorWalker []
  TreeWalker
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
      (if (coll? v)
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
                           (conj acc v)))) []))
        (r/validation-error [:not-collection] v))))
  (-one-of [this validatorA validatorB]
    (fn [v]
      (let [rA (tree-validate this validatorA v)]
        (if (r/error? rA)
          (tree-validate this validatorB v)
          rA)))))

(defrecord PossibleErrorsWalker []
  TreeWalker
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
  (-each [this validator]
    (r/join-errors
      (r/sample-error [:not-collection])
      (validator-eval validator this)))
  (-one-of [this _validatorA validatorB]
    (validator-eval validatorB this)))

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

(defrecord EachEval [validator]
  TreeEval
  (validator-eval [_this tree-walker]
    (-each tree-walker validator)))

(defrecord OneOfEval [validatorA validatorB]
  TreeEval
  (validator-eval [_this tree-walker]
    (-one-of tree-walker validatorA validatorB)))

;; Constructor functions
(defn validate [validator data]
  (let [validator-fn (validator-eval validator (ValidatorWalker.))]
    (validator-fn data)))

(defn validator [id constraints fn]
  (assert (keyword? id) (str "Validator ID must be a keyword - was " id))
  (ValidatorEval. id constraints fn))


;; TODO get rid of this crap?
(def ^:private allowed-preds
  #{"clojure.core/int?"
    "clojure.core/string?"
    "clojure.core/boolean?"
    "clojure.core/number?"
    "clojure.core/float?"
    "clojure.core/double?"
    "clojure.core/nil?"
    "clojure.core/keyword?"
    "clojure.core/any?"})

(defn predicate-id [sym]
  ;; TODO throw exception if fn--
  (let [f-ref (-> (Compiler/demunge (str sym))
                  (str/split #"@")
                  first
                  (str/split #"--")
                  first)]
    (if (allowed-preds f-ref)
      (keyword f-ref)
      f-ref)))


(defn predicate
  ([predicate-fn]
   (predicate (predicate-id predicate-fn) predicate-fn))
  ([id predicate-fn]
   (validator id {} (fn [_ v]
                      (when (predicate-fn v)
                        (r/success-value v))))))

(defn parser [id parser-fn]
  (validator id {} (fn [_ v]
                     (let [parsed (parser-fn v)]
                       (when-not (nil? parsed)
                         (r/success-value parsed))))))

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


(defn chain [validator1 validator2 & validators]
  (->> validators
       (concat [validator1 validator2])
       (reduce (fn [v1 v2] (ChainEval. v1 v2)))))

(defn group [validator1 validator2 & validators]
  (->> validators
       (concat [validator1 validator2])
       (reduce (fn [v1 v2] (GroupEval. v1 v2)))))

(defn each [validator]
  (EachEval. validator))

(defn one-of [validator1 validator2 & validators]
  (->> validators
       (concat [validator1 validator2])
       (reduce (fn [v1 v2] (OneOfEval. v1 v2)))))

(defn validate [validator data]
  (let [validator-fn (validator-eval validator (ValidatorWalker.))]
    (validator-fn data)))

(defn possible-errors [validator]
  (validator-eval validator (PossibleErrorsWalker.)))

;(defn generator [validator]
;  (validator-eval validator (GeneratorWalker. )))


;; Collection shenanigans

(extend-type IFn
  TreeEval
  (validator-eval [this tree-walker]
    (let [id (predicate-id this)
          vfn (fn [_ v]
                (when (this v)
                  (r/success-value v)))]
      (-leaf tree-walker id vfn {}))))

(extend-type APersistentVector
  TreeEval
  (validator-eval [this tree-walker]
    (-each tree-walker (first this))))

(defrecord RequiredKey [id key])
(defrecord OptionalKey [id key])

(defn required-key
  ([k]
   (required-key k k))
  ([id k]
   (RequiredKey. id k)))

(defn required-key-validator [{:keys [id key]} validator]
  (at id key validator))

(defn optional-key
  ([k]
   (optional-key k k))
  ([id k]
   (OptionalKey. id k)))

(defn optional-key-validator [{:keys [id key]} validator]
  (opt-at id key validator))

(def any-key ::any-key)

(defn key-set-validator [valid-keys]
  (validator ::invalid-keys {:keys valid-keys}
             (fn [constraints m]
               (let [input-keys (keys m)
                     invalid-keys (set/difference (set input-keys) (set (:keys constraints)))]
                 (when (empty? invalid-keys)
                   (r/success-value m))))))

(defn set-strictness [strict-mode? valid-keys key-validator]
  (if strict-mode?
    (group key-validator
           (key-set-validator valid-keys))
    key-validator))

(defn is-required-key? [k]
  (instance? RequiredKey k))

(defn is-optional-key? [k]
  (instance? OptionalKey k))

(defn- unwrap-key [k]
  (if (or (is-required-key? k) (is-optional-key? k))
    (:key k)
    k))

(defn- unwrap-keys [m]
  (->> m
       keys
       (map unwrap-key)
       (remove #{any-key})))

(defn- construct-map-validator [m]
  (let [strict-mode? (not (contains? m any-key))
        unwrapped-keys (unwrap-keys m)]
    (->> m
         (map (fn [[k v]]
                (cond
                  (is-optional-key? k) (optional-key-validator k v)
                  (is-required-key? k) (required-key-validator k v)
                  (= k any-key) nil
                  :else (at k v))))
         (remove nil?)
         (reduce group)
         (set-strictness strict-mode? unwrapped-keys))))

(extend-type APersistentMap
  TreeEval
  (validator-eval [this tree-walker]
    (let [validator (construct-map-validator this)]
      (validator-eval validator tree-walker))))
