(ns turtle-geometry.geometry-test
  "geometry test using inexact complex numbers"
  (:require [turtle-geometry.protocols :as p]
            [turtle-geometry.geometry :as g]
            [turtle-geometry.turtle :as turtle]
            [turtle-geometry.number.units.twenty-four :as units :refer [unit]]
            [turtle-geometry.number.complex :as complex]
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

(deftest basic-composition
  (testing "basic properties of compose"
    (is (= g/Identity (g/compose)) "no arguments is identity")
    (is (= (g/->Reflection) (g/compose (g/reflection))) "one argument returns itself")
    (is (= (g/->Composition (list (g/->Reflection)
                                  (g/->Rotation (unit 45))
                                  (g/->Translation complex/one)))
           (g/compose (g/reflection) (g/rotation (unit 45)) (g/translation complex/one)))
        "compose works for 3 args")))

(deftest basic-identity
  (testing "Identity behaves as expected"
    (is (= (p/inverse g/Identity) g/Identity) "inverse of Identity is Identity")
    (is (p/equals? g/Identity (g/rotation (unit 0))) "rotation by 0 is identity")
    (is (p/equals? g/Identity (g/rotation (unit 360))) "rotation by 360 is identity")
    (is (p/equals? g/Identity (g/rotation (unit -360))) "rotation by -360 is identity")
    (is (p/equals? g/Identity (g/translation complex/zero)) "translation by zero is identity")
    (is (p/equals? g/Identity (g/dilation 1)) "dilation by one is identity")
    (is (p/equals? (complex/complex 2 3)
                   ((p/transform-fn g/Identity) (complex/complex 2 3)))
        "transform-fn of Identity is identity")))

(deftest basic-transforms
  (testing "basic properties of transforms"
    (is (p/equals? (turtle/position (complex/complex -1 0))
                   (p/transform (turtle/position complex/one)
                                (g/reflection complex/zero (unit 90))))
        "reflection about y-axis transforms one to -one")))

(comment
  (require '[turtle-geometry.geometry-test] :reload)
  (in-ns 'turtle-geometry.geometry-test)
  (clojure.test/run-tests)
  )
