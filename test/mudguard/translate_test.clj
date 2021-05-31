(ns mudguard.translate-test
  (:require [clojure.test :refer :all]
            [mudguard.translate :as sut]
            [mudguard.core :as core]))

(deftest matches-translation?
  (testing "pattern matching on translation paths (indices are ignored)"
    (are [path translation-key should-match]
      (is (= should-match
             (sut/matches-translation? path translation-key)))
      [:int?] [:int?] true
      [:int?] [:boolean?] false
      [:a :int?] [:int?] true
      [:a :int?] [:a :int?] true
      [:a :int?] [:b :int?] false
      [:int?] [:a :int?] false
      [0 :int?] [:int?] true
      [:a 1 :int?] [:a :int?] true
      [:a 1 :int?] [:b :int?] false
      )
    )
  )

(deftest translate-errors-test
  (testing "error translations"
    (testing "can provide general translation"
      (let [errors (core/validation-errors
                     (core/sample-error [:a :int?])
                     (core/sample-error [:b :int?]))
            translations {:int? "Value should be an integer"}]
        (is (= {[:a] ["Value should be an integer"]
                [:b] ["Value should be an integer"]}
               (sut/translate-errors translations errors)))))
    (testing "multiple errors for same path"
      (is (= {[:a] ["Value should be an integer"
                    "Value should not be nil"]
              [:b] ["Value should not be nil"]}
             (sut/translate-errors
               {:int?     "Value should be an integer"
                :not-nil? "Value should not be nil"}
               (core/validation-errors
                 (core/sample-error [:a :int?])
                 (core/sample-error [:a :not-nil?])
                 (core/sample-error [:b :not-nil?]))))))
    (testing "More specific translation match favoured"
      (is (= {[:a] ["Value should be an integer"]
              [:b] ["B should be an integer"]}
             (sut/translate-errors
               {:int?      "Value should be an integer"
                [:b :int?] "B should be an integer"}
               (core/validation-errors
                 (core/sample-error [:a :int?])
                 (core/sample-error [:b :int?]))))))

    (testing "non-nested validation keyed with empty path"
      (is (= {[] ["Value should be an integer"
                  "Value should not be nil"]}
             (sut/translate-errors
               {:int?      "Value should be an integer"
                :not-nil?  "Value should not be nil"
                [:b :int?] "B should be an integer"}
               (core/validation-errors
                 (core/sample-error [:int?])
                 (core/sample-error [:not-nil?]))))))

    (testing "Indicies in paths are ignored")               ;; TODO

    (testing "Context keys can be templated into message"
      (is (= {[:a] ["Failed with value bob"]
              [:b] ["B failed with value bill"]}
             (sut/translate-errors
               {:int?      (fn [{::core/keys [input]}]
                             (format "Failed with value %s" input))
                [:b :int?] (fn [{::core/keys [input]}]
                             (format "B failed with value %s" input))}
               (core/validation-errors
                 (core/validation-error [:a :int?] "bob")
                 (core/validation-error [:b :int?] "bill"))))))

    (testing "Index can be templated into message")         ;; TODO
    ))

(deftest restructure-messages-test
  (testing "error message paths used to rehydrate into object structure"
    (is (= {:a ["Value should be an integer"
                "Value should not be nil"]
            :b {:c ["Value should not be nil"]}}
           (sut/restructure-messages {[:a]    ["Value should be an integer"
                                               "Value should not be nil"]
                                      [:b :c] ["Value should not be nil"]}))))
  (testing "overlapping errors at different levels of structure.."
    (is (= {:a {::sut/errors ["Need structure :("]
                :a1          ["Need integer"]}}
           (sut/restructure-messages {[:a]     ["Need structure :("]
                                      [:a :a1] ["Need integer"]}))))
  (testing "empty path key"
    (is (= ["Some error"]
           (sut/restructure-messages {[] ["Some error"]})))
    (is (= {::sut/errors ["Some error"]
            :a           ["A errors"]}
           (sut/restructure-messages {[]   ["Some error"]
                                      [:a] ["A errors"]})))))

(deftest untranslatable-errors-test
  (testing "TODO"
    (let [errors (core/validation-errors
                   (core/sample-error [:a :int?])
                   (core/sample-error [:b :int?])
                   (core/sample-error [0 :missing]))]
      (testing "all errors returned if no translations provided"
        (is (= errors
               (sut/untranslatable-errors {} errors))))
      (testing "just missing errors returned if some translations provided"
        (is (= (core/validation-errors
                 (core/sample-error [:a :int?])
                 (core/sample-error [:b :int?]))
               (sut/untranslatable-errors {:missing "Value is missing"} errors)))
        (is (= (core/sample-error [:a :int?])
               (sut/untranslatable-errors {:missing   "Value is missing"
                                           [:b :int?] "B should be int"} errors))))
      (testing "nil returned if all translations provided"
        (is (= nil
               (sut/untranslatable-errors {:missing "Value is missing"
                                           :int?    "B should be int"} errors)))))))
