(ns turtle-geometry.number.complex-test
  "tests for Complex number"
  (:require [turtle-geometry.protocols :as p]
            [turtle-geometry.number.complex :as c]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            #?@(:clj
                [[clojure.test :refer :all]
                 [clojure.test.check.clojure-test :refer [defspec]]
                 [clojure.test.check.properties :as prop]]
                :cljs
                [[cljs.test :as text :refer-macros [is deftest are testing run-tests]]
                 [clojure.test.check.clojure-test :refer-macros [defspec]]
                 [clojure.test.check.properties :as prop :include-macros true]])))

(deftest complex
  (testing "negative and reciprocal"
    (let [w (c/complex 1 2)]
      (is (p/one? (p/multiply w (p/reciprocal w))))
      (is (p/zero? (p/add w (p/negative w)))))))

(deftest infinity
  (testing "basic properties of infinity"
    (is (c/infinity? c/Infinity))
    (is (c/undefined? (p/negative c/Infinity)))
    (is (p/zero? (p/reciprocal c/Infinity)))
    (is (c/infinity? (p/reciprocal c/zero)))
    (is (c/infinity? (p/add c/Infinity c/one)))
    (is (c/infinity? (p/add c/one c/Infinity)))
    (is (c/infinity? (p/multiply c/i c/Infinity)))
    (is (c/infinity? (p/multiply c/Infinity c/i)))
    (is (c/undefined? (p/multiply c/zero c/Infinity)))
    (is (c/undefined? (p/multiply c/Infinity c/zero)))))

(comment
  (require '[turtle-geometry.number.complex-test] :reload)
  (in-ns 'turtle-geometry.number.complex-test)
  (clojure.test/run-tests)
  )
