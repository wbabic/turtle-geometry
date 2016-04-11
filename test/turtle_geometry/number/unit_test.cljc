(ns turtle-geometry.number.unit-test
  "tests for units"
  (:require [turtle-geometry.protocols :as p]
            [turtle-geometry.number.unit :as u]
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

(deftest units
  (testing "basic unit multiplication"
    (is (p/equals? (u/unit 90) (p/multiply (u/unit 30) (u/unit 60))))
    (is (p/equals? (u/unit 90) (p/multiply (u/unit 45) (u/unit 45))))
    (is (p/equals? (u/unit 90) (p/multiply (u/unit 15) (u/unit 75))))
    (is (p/equals? (u/unit 15) (u/unit 375)))
    (is (p/equals? (u/unit 30) (p/multiply (u/unit 15) (u/unit 15))))
    (is (p/equals? (u/unit 45) (p/multiply (u/unit 15) (u/unit 30))))))

(deftest units-inverse
  (doseq [a (map #(* % 15) (range 25))]
    (let [u (u/unit a)]
      (is (p/one?  (p/multiply u (p/reciprocal u)))))))

(comment
  (require '[turtle-geometry.number.unit-test] :reload)
  (in-ns 'turtle-geometry.number.unit-test)
  (clojure.test/run-tests)
  )
