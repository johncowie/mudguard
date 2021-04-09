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

(deftest parser-test
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
      (is (= (sut/validation-error [:a :int?] "a" {})
             (sut/validate validator {:a "a"})))
      (is (= {:a 1}
             (sut/validate validator {:a 1})))
      (is (= {}
             (sut/validate validator {})))
      (testing "possible-errors"
        (is (= (sut/sample-error [:a :int?] {})
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
             (sut/sample-error [:int?]))
           (sut/possible-errors (sut/each int?))))))

(deftest one-of-test
  (let [validator (sut/one-of int? string? boolean?)]
    (is (= 1
           (sut/validate validator 1)))
    (is (= "blah"
           (sut/validate validator "blah")))
    (is (= false
           (sut/validate validator false)))
    (is (= (sut/validation-error [:boolean?] :keyword {})
           (sut/validate validator :keyword))))
  (testing "possible-errors, can only fail on last validator"
    (is (= (sut/sample-error [:boolean?])
           (sut/possible-errors (sut/one-of int? string? boolean?))))
    (is (= (sut/sample-error [:int?])
           (sut/possible-errors (sut/one-of boolean? string? int?))))))

(deftest function-test
  (testing "can use predicate function as validator"
    (is (= 1
           (sut/validate int? 1)))
    (is (= (sut/validation-error [:int?] "")
           (sut/validate int? "")))))

(deftest map-test
  (testing "can use clojure map as validator"
    (is (= {:a 1 :b :keyword}
           (sut/validate {:a int? :b keyword?} {:a 1 :b :keyword})))
    (is (= (sut/validation-errors
             (sut/validation-error [:a :int?] "bill")
             (sut/validation-error [:b :keyword?] "ted"))
           (sut/validate {:a int? :b keyword?} {:a "bill" :b "ted"}))))
  (testing "can specify optional keys"
    (let [validator {(sut/optional-key :a) int?
                     :b                    int?}]
      (is (= {:a 1 :b 2}
             (sut/validate validator {:a 1 :b 2})))
      (is (= (sut/validation-error [:b :missing] nil)
             (sut/validate validator {})))
      (is (= {:b 3}
             (sut/validate validator {:b 3})))
      (is (= (sut/validation-error [:a :int?] "blah")
             (sut/validate validator {:a "blah" :b 3})))))
  (testing "nested"
    (is (= {:a {:aa 2 :ab 3}
            :b 5}
           (sut/validate
             {:a {:aa int? :ab int?}
              :b any?}
             {:a {:aa 2 :ab 3}
              :b 5})))
    (is (= (sut/validation-error [:a :aa :missing] nil)
           (sut/validate
             {:a {:aa int? (sut/optional-key :ab) int?}
              :b any?}
             {:a {}
              :b 5})))))

(deftest col-test
  (testing "can use clojure vector as validator"
    (let [validator [{:a int?}]]
      (is (= [{:a 1} {:a 2}]
             (sut/validate validator [{:a 1} {:a 2}])))
      (is (= (sut/validation-errors
               (sut/validation-error [0 :a :int?] "X")
               (sut/validation-error [1 :a :missing] nil))
             (sut/validate validator [{:a "X"} {} {:a 3}]))))))
