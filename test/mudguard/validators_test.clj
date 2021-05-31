(ns mudguard.validators-test
  (:require [clojure.test :refer :all]
            [mudguard.core :as sut]))

(defn valid? [validator val]
  (not (sut/error? (sut/coerce validator val))))

(defn invalid? [validator val]
  (sut/error? (sut/coerce validator val)))

(deftest NotBlank-test
  (is (valid? sut/NotBlank "bob"))
  (is (valid? sut/NotBlank "  sd "))
  (is (invalid? sut/NotBlank ""))
  (is (invalid? sut/NotBlank "   ")))

(deftest matches-regex-test
  (comment "todo")
  )
