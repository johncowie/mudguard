(ns mudguard.gen-test
  (:require [clojure.test :refer :all]
            [clojure.test.check.generators :as g]
            [mudguard.core :as m]
            [mudguard.gen :as mg]))

(deftest generator-test
  (testing "generating primitives"
    (testing "Int example"
      (is (first (g/sample (mg/generator m/Int))))
      (is (thrown? Exception (first (g/sample (mg/generator m/Int {:clojure.core/int? g/string}))))))
    (testing "others"
      (are [validator]
        (try
          (-> (g/sample (mg/generator validator {:parse-int    (g/fmap str g/small-integer)
                                                 :more-than-10 (g/return 12)}))
              (nth 5))
          true
          (catch Exception e
            (println (.getMessage e))
            false))
        m/Str
        m/Any
        m/Keyword
        m/Bool
        m/Nil
        m/Num
        m/FloatNum
        m/Inst
        m/NatInt
        {:a m/Str}
        (m/at :a m/Int)
        (m/opt-at :a m/Str)
        (m/chain m/Str
                 (m/parser :parse-int #(Integer/parseInt %)))
        [{:a m/Int}]
        (m/chain m/Int
                 (m/group (m/predicate :even even?)
                          (m/predicate :more-than-10 #(> % 10))))
        (m/one-of m/Str m/Int m/Bool)
        {(m/required-key :a "A") m/Str
         (m/required-key :b "B") m/Str
         m/Any                   m/Any}
        )
      )
    )
  )



