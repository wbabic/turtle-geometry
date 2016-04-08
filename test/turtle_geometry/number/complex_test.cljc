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

(deftest units
  (testing "basic unit multiplication"
    (is (p/equals? c/i (p/multiply (p/unit 30) (p/unit 60))))
    (is (p/equals? c/i (p/multiply (p/unit 45) (p/unit 45))))
    (is (p/equals? c/i (p/multiply (p/unit 15) (p/unit 75))))
    (is (p/equals? (p/unit 15) (p/unit 375)))
    (is (p/equals? (p/unit 30) (p/multiply (p/unit 15) (p/unit 15))))
    (is (p/equals? (p/unit 45) (p/multiply (p/unit 15) (p/unit 30))))))

(deftest units-inverse
  (doseq [a (map #(* % 15) (range 25))]
    (let [u (p/unit a)]
      (is (p/zero? (p/add u (p/negative u))))
      (is (p/one?  (p/multiply u (p/unit (- 360 a))))))))

(comment
  (require '[turtle-geometry.number.complex-test] :reload)
  (in-ns 'turtle-geometry.number.complex-test)
  (clojure.test/run-tests)
  )
