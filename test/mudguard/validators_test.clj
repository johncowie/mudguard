(ns mudguard.validators-test
  (:require [clojure.test :refer :all]
            [mudguard.core :as m]
            [mudguard.validators :as sut]))

(defn valid? [validator val]
  (not (m/error? (m/validate validator val))))

(defn invalid? [validator val]
  (m/error? (m/validate validator val)))

(deftest NotBlank-test
  (is (valid? sut/NotBlank "bob"))
  (is (valid? sut/NotBlank "  sd "))
  (is (invalid? sut/NotBlank ""))
  (is (invalid? sut/NotBlank "   ")))

(deftest matches-regex-test
  (comment "todo")
  )
