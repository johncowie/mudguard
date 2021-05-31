(ns mudguard.core
  (:require [clojure.string :as str]
            [mudguard.tree :as t]
            [mudguard.result :as r]))

(def validation-errors r/validation-errors)
(def validation-error r/validation-error)
(def sample-error r/sample-error)
(def success-value r/success-value)
(def error? r/error?)

(def at t/at)
(def opt-at t/opt-at)
(def group t/group)
(def chain t/chain)
(def each t/each)
(def one-of t/one-of)
(def predicate t/predicate)

(def optional-key t/optional-key)
(def required-key t/required-key)

(def validate t/validate)
(def validator t/validator)
(def parser t/parser)
(def possible-errors t/possible-errors)

(def Int (predicate :clojure.core/int? int?))
(def Bool (predicate :clojure.core/boolean? boolean?))
(def Str (predicate :clojure.core/string? string?))
(def Nil (predicate :clojure.core/nil? nil?))
(def Keyword (predicate :clojure.core/keyword? keyword?))
(def Any (predicate :clojure.core/any? any?)) ;; TODO can't fail so should be no-op
;; TODO specify default generators here?

(def NotBlank (predicate :not-blank (complement str/blank?)))
;; TODO
;(defn matches-regex [regex] Any)

;; TODO
;; - optional value
;; - can specify if extra keys are tolerated
;; - maybe at-any? required?
;; - bunch of default validators
;; - map of type..
;; - equals value

;; TODO roadmap
;; Generators
;; Pre-compile
;; Simplify map shenanigans
;; Swagger

(defn maybe [validator]
  (one-of Nil validator))
