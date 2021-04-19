(ns mudguard.validators
  (:require [mudguard.core :as m]
            [clojure.string :as str]))

(def Int int?)
(def Bool boolean?)
(def Str string?)
(def Nil nil?)
(def Any any?)

(def NotBlank (m/predicate :not-blank (complement str/blank?)))
;; TODO
(defn matches-regex [regex] Any)
