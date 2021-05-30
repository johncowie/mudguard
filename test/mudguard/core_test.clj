(ns mudguard.core-test
  (:require [clojure.test :refer :all]
            [mudguard.core :as sut]))

(deftest predicate-test
  (let [validator (sut/predicate :int? int?)]
    (is (= 1
           (sut/validate validator 1)))
    (is (= (sut/validation-error [:int?] "bob" {})
           (sut/validate validator "bob")))
    (testing "possible errors"
      (is (= (sut/sample-error [:int?] {})
             (sut/possible-errors validator))))))

(deftest parser-validator-test
  (let [parser (sut/validator :parse-int {}
                              (fn [_ v]
                                (sut/success-value (Integer/parseInt v))))]
    (is (= 1
           (sut/validate parser "1")))
    (is (= (sut/validation-error [:parse-int] "bob" {})
           (sut/validate parser "bob")))
    (testing "possible-errors"
      (is (= (sut/sample-error [:parse-int] {})
             (sut/possible-errors parser))))))

(deftest parser-test
  (testing "can use the parser helper function for creating simple parsers"
    (let [parser (sut/parser :parse-boolean (fn [s]
                                              (case s
                                                "true" true
                                                "false" false
                                                nil)))]
      (is (= true
             (sut/validate parser "true")))
      (is (= false
             (sut/validate parser "false")))
      (is (= (sut/validation-error [:parse-boolean] "blah")
             (sut/validate parser "blah"))))))

(deftest at-test
  (testing "mandatory"
    (let [int-parser (sut/validator :parse-int {}
                                    (fn [_ v]
                                      (sut/success-value (Integer/parseInt v))))
          validator (sut/at :a int-parser)]
      (is (= (sut/validation-error [:a :parse-int] "a" {})
             (sut/validate validator {:a "a"})))
      (is (= {:a 1}
             (sut/validate validator {:a "1"})))
      (is (= (sut/validation-error [:a :missing] nil {})
             (sut/validate validator {})))
      (testing "possible-errors"
        (is (= (sut/validation-errors
                 (sut/sample-error [:a :missing] {})
                 (sut/sample-error [:a :parse-int] {}))
               (sut/possible-errors validator))))))
  (testing "optional"
    (let [validator (sut/opt-at :a int?)]
      (is (= (sut/validation-error [:a :clojure.core/int?] "a" {})
             (sut/validate validator {:a "a"})))
      (is (= {:a 1}
             (sut/validate validator {:a 1})))
      (is (= {}
             (sut/validate validator {})))
      (testing "possible-errors"
        (is (= (sut/sample-error [:a :clojure.core/int?] {})
               (sut/possible-errors validator)))))))

(deftest group-test
  (let [int-parser (sut/validator :parse-int {}
                                  (fn [_ v]
                                    (sut/success-value (Integer/parseInt v))))
        validator (sut/group (sut/at :a int-parser)
                             (sut/at :b int-parser)
                             (sut/at :c int-parser))]
    (is (= {:a 1 :b 3 :c 5}
           (sut/validate validator {:a "1" :b "3" :c "5"})))
    (is (= (sut/validation-errors
             (sut/validation-error [:a :parse-int] "blah" {})
             (sut/validation-error [:b :parse-int] "bleh" {}))
           (sut/validate validator {:a "blah" :b "bleh" :c "8"})))
    (is (= (sut/validation-error [:c :parse-int] "x" {})
           (sut/validate validator {:a "1" :b "2" :c "x"})))
    (testing "possible-errors"
      (is (= (sut/validation-errors
               (sut/sample-error [:a :missing] {})
               (sut/sample-error [:a :parse-int] {})
               (sut/sample-error [:b :missing] {})
               (sut/sample-error [:b :parse-int] {})
               (sut/sample-error [:c :missing] {})
               (sut/sample-error [:c :parse-int] {}))
             (sut/possible-errors validator))))))

(deftest chain-test
  (let [is-str (sut/predicate :string? string?)
        int-parser (sut/validator :parse-int {}
                                  (fn [_ v]
                                    (sut/success-value (Integer/parseInt v))))
        >10 (sut/predicate :>10 #(> % 10))
        validator (sut/chain is-str int-parser >10)]
    (is (= 20
           (sut/validate validator "20")))
    (is (= (sut/validation-error [:string?] 23 {})
           (sut/validate validator 23)))
    (is (= (sut/validation-error [:parse-int] "bob" {})
           (sut/validate validator "bob")))
    (is (= (sut/validation-error [:>10] 5 {})
           (sut/validate validator "5")))
    (testing "possible-failures"
      (is (= (sut/validation-errors
               (sut/sample-error [:string?] {})
               (sut/sample-error [:parse-int] {})
               (sut/sample-error [:>10] {}))
             (sut/possible-errors validator))))))

(deftest each-test
  (let [int-parser (sut/validator :parse-int {}
                                  (fn [_ v]
                                    (sut/success-value (Integer/parseInt v))))
        validator (sut/each int-parser)]
    (is (= [1 2 3]
           (sut/validate validator ["1" "2" "3"])))
    (is (= (sut/validation-errors
             (sut/validation-error [0 :parse-int] "A" {})
             (sut/validation-error [2 :parse-int] "B" {}))
           (sut/validate validator ["A" "2" "B"])))
    (testing "error if value is not a sequence"
      (is (= (sut/validation-error [:not-collection] :bob)
             (sut/validate validator :bob)))))
  (testing "possible-errors"
    (is (= (sut/validation-errors
             (sut/sample-error [:not-collection])
             (sut/sample-error [:clojure.core/int?]))
           (sut/possible-errors (sut/each int?))))))

(deftest one-of-test
  (let [validator (sut/one-of int? string? boolean?)]
    (is (= 1
           (sut/validate validator 1)))
    (is (= "blah"
           (sut/validate validator "blah")))
    (is (= false
           (sut/validate validator false)))
    (is (= (sut/validation-error [:clojure.core/boolean?] :keyword {})
           (sut/validate validator :keyword))))
  (testing "possible-errors, can only fail on last validator"
    (is (= (sut/sample-error [:clojure.core/boolean?])
           (sut/possible-errors (sut/one-of int? string? boolean?))))
    (is (= (sut/sample-error [:clojure.core/int?])
           (sut/possible-errors (sut/one-of boolean? string? int?))))))

(deftest function-test
  (testing "can use predicate function as validator"
    (is (= 1
           (sut/validate int? 1)))
    (is (= (sut/validation-error [:clojure.core/int?] "")
           (sut/validate int? "")))))

(deftest map-test
  (testing "can use clojure map as validator"
    (let [validator {:a sut/Int :b sut/Keyword}]
      (is (= {:a 1 :b :keyword}
             (sut/validate validator {:a 1 :b :keyword})))
      (is (= (sut/validation-errors
               (sut/validation-error [:a :clojure.core/int?] "bill")
               (sut/validation-error [:b :clojure.core/keyword?] "ted"))
             (sut/validate validator {:a "bill" :b "ted"})))
      (is (= (sut/validation-error [:mudguard.tree/invalid-keys] {:a 1 :b :keyword :c 3} {:keys [:a :b]}) ;; TODO capture the invalid keys found
             (sut/validate validator {:a 1 :b :keyword :c 3})))))
  (testing "can specify optional keys"
    (let [validator {(sut/optional-key :a) sut/Int
                     :b                    sut/Int}]
      (is (= {:a 1 :b 2}
             (sut/validate validator {:a 1 :b 2})))
      (is (= (sut/validation-error [:b :missing] nil)
             (sut/validate validator {})))
      (is (= {:b 3}
             (sut/validate validator {:b 3})))
      (is (= (sut/validation-error [:a :clojure.core/int?] "blah")
             (sut/validate validator {:a "blah" :b 3})))))
  (testing "can specify any-key"
    (let [validator {:a sut/Int sut/any-key sut/Any}]
      (is (= {:a 1}
             (sut/validate validator {:a 1})))
      (is (= {:a 2 :b 3}
             (sut/validate validator {:a 2 :b 3})))
      (is (= (sut/validation-error [:a :missing] nil)
             (sut/validate validator {})))))
  (testing "nested"
    (is (= {:a {:aa 2 :ab 3}
            :b 5}
           (sut/validate
             {:a {:aa sut/Int :ab sut/Int}
              :b sut/Any}
             {:a {:aa 2 :ab 3}
              :b 5})))
    (is (= (sut/validation-error [:a :aa :missing] nil)
           (sut/validate
             {:a {:aa sut/Int (sut/optional-key :ab) sut/Int}
              :b sut/Any}
             {:a {}
              :b 5}))))
  (testing "possible errors"
    (is (= (sut/validation-errors
             (sut/sample-error [:a :missing])
             (sut/sample-error [:a :clojure.core/int?])
             (sut/sample-error [:mudguard.tree/invalid-keys] {:keys [:a]}))
           (sut/possible-errors {:a sut/Int})))))

(deftest col-test
  (testing "can use clojure vector as validator"
    (let [validator [{:a sut/Int}]]
      (is (= [{:a 1} {:a 2}]
             (sut/validate validator [{:a 1} {:a 2}])))
      (is (= (sut/validation-errors
               (sut/validation-error [0 :a :clojure.core/int?] "X")
               (sut/validation-error [1 :a :missing] nil))
             (sut/validate validator [{:a "X"} {} {:a 3}]))))))
