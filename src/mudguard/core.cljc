(ns mudguard.core
  (:require [clojure.string :as str]
            [clojure.set :as set])
  (:import (clojure.lang IFn)))

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
  (some? (::success v)))

(defn extract-success-val [v]
  (::success v))

(defprotocol IValidator
  (validate [validator v])
  (possible-errors [validator]))

(defn throw-if-invalid [validator v]
  (let [r (validate validator v)]
    (if (error? r)
      (throw (ex-info "Validation failed." r))
      v)))

(defn check [validator v]
  (let [res (validate validator v)]
    (when (error? res)
      res)))

(defrecord NoOpValidator []
  IValidator
  (validate [_ v]
    v)
  (possible-errors [_]
    (validation-errors)))

(def no-op-validator (NoOpValidator.))

(defrecord Validator [id fn constraints]
  IValidator
  (validate [_ v]
    (try
      (let [return-value (fn constraints v)]
        (if (success-value? return-value)
          (extract-success-val return-value)
          (validation-error [id] v constraints)))
      (catch Exception e
        (validation-error [id] v constraints))))
  (possible-errors [_]
    (sample-error [id] constraints)))

(defn validator [id constraints fn]
  (assert (keyword? id) (str "Validator ID must be a keyword - was " id))
  (Validator. id fn constraints))

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

(defn- predicate-id [sym]
  ;; TODO throw exception if fn--
  (let [f-ref (-> (clojure.lang.Compiler/demunge (str sym))
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
                        (success-value v))))))

(defn parser [id parser-fn]
  (validator id {} (fn [_ v]
                     (when-let [parsed (parser-fn v)]
                       (success-value parsed)))))

(extend-type IFn
  IValidator
  (validate [self data]
    (validate (predicate self) data))
  (possible-errors [self]
    (possible-errors (predicate self))))

(defrecord At [optional? id key validator]
  IValidator
  (validate [this v]
    (if (and (associative? v) (contains? v key))
      (let [x (get v key)
            res (validate validator x)]
        (if (error? res)
          (errors-at id res)
          (assoc v key res)))
      (if optional?
        v
        (validation-error [id :missing] nil {}))))
  (possible-errors [_]
    (if optional?
      (errors-at id (possible-errors validator))
      (join-errors
        (sample-error [id :missing])
        (errors-at id (possible-errors validator))))))

(defn at
  ([key validator]
   (at key key validator))
  ([id key validator]
   (assert (keyword? id) (str "Validator ID must be a keyword - was " id))
   (At. false id key validator)))

(defn opt-at
  ([key validator]
   (opt-at key key validator))
  ([id key validator]
   (assert (keyword? id) (str "Validator ID must be a keyword - was " id))
   (At. true id key validator)))

(defrecord Chain [validatorA validatorB]
  IValidator
  (validate [_ v]
    (let [rA (validate validatorA v)]
      (if (error? rA)
        rA
        (validate validatorB rA))))
  (possible-errors [_]
    (join-errors
      (possible-errors validatorA)
      (possible-errors validatorB))))

(defn chain [validator1 validator2 & validators]
  (->> validators
       (concat [validator1 validator2])
       (reduce (fn [v1 v2] (Chain. v1 v2)))))

(defrecord Group [validatorA validatorB]
  IValidator
  (validate [_ v]
    (let [rA (validate validatorA v)]
      (if (error? rA)
        (let [rB (validate validatorB v)]
          (if (error? rB)
            (join-errors rA rB)
            rA))
        (validate validatorB rA))))
  (possible-errors [_]
    (join-errors
      (possible-errors validatorA)
      (possible-errors validatorB))))

(defn group [validator1 validator2 & validators]
  (->> validators
       (concat [validator1 validator2])
       (reduce (fn [v1 v2] (Group. v1 v2)))))

(defrecord Each [validator]
  IValidator
  (validate [_ v]
    (if (coll? v)
      (->> v
           (map (partial validate validator))
           (map-indexed vector)
           (reduce (fn [acc [i v]]
                     (if (error? acc)
                       (if (error? v)
                         (join-errors acc (errors-at i v))
                         acc)
                       (if (error? v)
                         (errors-at i v)
                         (conj acc v)))) []))
      (validation-error [:not-collection] v)))
  (possible-errors [_]
    (join-errors
      (sample-error [:not-collection])
      (possible-errors validator))))

(defn each [validator]
  (Each. validator))

(defrecord OneOf [validatorA validatorB]
  IValidator
  (validate [_ v]
    (let [rA (validate validatorA v)]
      (if (error? rA)
        (validate validatorB v)
        rA)))
  (possible-errors [_]
    (possible-errors validatorB)))

(defn one-of [validator1 validator2 & validators]
  (->> validators
       (concat [validator1 validator2])
       (reduce (fn [v1 v2] (OneOf. v1 v2)))))



;; MAP shenanigans

(defn required-key
  ([k]
   (required-key k k))
  ([id k]
   {::required-key {:id id :key k}}))

(defn required-key-validator [{::keys [required-key]} validator]
  (let [{:keys [id key]} required-key]
    (at id key validator)))

(defn optional-key
  ([k]
   (optional-key k k))
  ([id k]
   {::optional-key {:id id :key k}}))


(defn optional-key-validator [{::keys [optional-key]} validator]
  (let [{:keys [id key]} optional-key]
    (opt-at id key validator)))

(def any-key ::any-key)

(defn key-set-validator [valid-keys]
  (validator :invalid-keys {:keys valid-keys}
             (fn [constraints m]
               (let [input-keys (keys m)
                     invalid-keys (set/difference (set input-keys) (set (:keys constraints)))]
                 (when (empty? invalid-keys)
                   (success-value m))))))

(defn set-strictness [strict-mode? valid-keys key-validator]
  (if strict-mode?
    (group (key-set-validator valid-keys)
           key-validator)
    key-validator))

(defn is-wrapped-key? [k wrapper]
  (and (map? k) (contains? k wrapper)))

(defn- unwrap-key [k]
  (cond (is-wrapped-key? k ::optional-key) (-> k ::optional-key :key)
        (is-wrapped-key? k ::required-key) (-> k ::required-key :key)
        :else k))

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
                  (::optional-key k) (optional-key-validator k v)
                  (::required-key k) (required-key-validator k v)
                  (= k any-key) no-op-validator
                  :else (at k v))))
         (reduce group no-op-validator)
         (set-strictness strict-mode? unwrapped-keys))))

(extend-type clojure.lang.APersistentMap
  IValidator
  (validate [self data]
    (-> self construct-map-validator (validate data)))
  (possible-errors [self]
    (-> self construct-map-validator possible-errors)))

(extend-type clojure.lang.APersistentVector
  IValidator
  (validate [self data]
    (-> self first each (validate data)))
  (possible-errors [self]
    (-> self first each possible-errors)))


(def Int int?)
(def Bool boolean?)
(def Str string?)
(def Nil nil?)
(def Any any?)

(def NotBlank (predicate :not-blank (complement str/blank?)))
;; TODO
(defn matches-regex [regex] Any)

;; TODO
;; - optional value
;; - can specify if extra keys are tolerated
;; - maybe at-any? required?
;; - bunch of default validators


(defn maybe [validator]
  (one-of Nil validator))

