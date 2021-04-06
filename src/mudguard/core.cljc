(ns mudguard.core
  (:require [clojure.string :as str])
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
   (validation-error id ::no-input constraints)))

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
    (validation-error [id] ::no-input constraints)))

(defn validator [id constraints fn]
  (Validator. id fn constraints))

(defn predicate-id [sym]
  ;; TODO throw exception if fn--
  (-> (clojure.lang.Compiler/demunge (str sym))
      (str/split #"/")
      last
      (str/split #"@")
      first
      keyword))

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

(defrecord At [optional? key validator]
  IValidator
  (validate [this v]
    (if (and (associative? v) (contains? v key))
      (let [x (get v key)
            res (validate validator x)]
        (if (error? res)
          (errors-at key res)
          (assoc v key res)))
      (if optional?
        v
        (validation-error [key :missing] nil {}))))
  (possible-errors [_]
    (if optional?
      (errors-at key (possible-errors validator))
      (join-errors
        (validation-error [key :missing] ::no-input {})
        (errors-at key (possible-errors validator))))))

(defn at [key validator]
  (At. false key validator))

(defn opt-at [key validator]
  (At. true key validator))

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
                       (conj acc v)))) [])))
  (possible-errors [_]
    (possible-errors validator)))

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

(defn optional-key [k]
  {::optional-key k})

(extend-type clojure.lang.PersistentArrayMap
  IValidator
  (validate [self data]
    (let [validator (->> self
                         (map (fn [[k v]]
                                (if-let [opt-k (::optional-key k)]
                                  (opt-at opt-k v)
                                  (at k v))))
                         (reduce group no-op-validator))]
      (validate validator data)))
  (possible-errors [self]
    (let [validator (->> self
                         (map (fn [[k v]]
                                (if-let [opt-k (::optional-key k)]
                                  (opt-at opt-k v)
                                  (at k v))))
                         (reduce group no-op-validator))]
      (possible-errors validator))))

(extend-type clojure.lang.PersistentVector
  IValidator
  (validate [self data]
    (-> self first each (validate data)))
  (possible-errors [self]
    (-> self first each possible-errors)))

;; TODO
;; - optional value
;; - schema
;; - translations
;; - bunch of default validators


