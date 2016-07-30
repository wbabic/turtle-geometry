(ns turtle-geometry.geometry-test
  "geometry test using inexact complex numbers"
  (:require [turtle-geometry.protocols :as p]
            [turtle-geometry.geometry :as g]
            [turtle-geometry.turtle :as turtle]
            [turtle-geometry.number :as complex :refer [one zero i infinity complex unit]]
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
                                  (g/->Translation one)))
           (g/compose (g/reflection) (g/rotation (unit 45)) (g/translation one)))
        "compose works for 3 args")))

(deftest basic-identity
  (testing "Identity behaves as expected"
    (is (= (p/inverse g/Identity) g/Identity) "inverse of Identity is Identity")
    (is (p/equals? g/Identity (g/rotation (unit 0))) "rotation by 0 is identity")
    (is (p/equals? g/Identity (g/rotation (unit 360))) "rotation by 360 is identity")
    (is (p/equals? g/Identity (g/rotation (unit -360))) "rotation by -360 is identity")
    (is (p/equals? g/Identity (g/translation zero)) "translation by zero is identity")
    (is (p/equals? g/Identity (g/dilation 1)) "dilation by one is identity")
    (is (p/equals? (complex 2 3)
                   ((p/transform-fn g/Identity) (complex 2 3)))
        "transform-fn of Identity is identity")))

(deftest basic-transforms
  (testing "basic properties of inversion"
    (is (p/almost-equals? (p/transform (g/position one)
                                       (g/reflection zero (unit 90)))
                          (g/position (complex -1 0))
                          1E-7)
        "reflection about y-axis transforms one to -one")
    (is (p/almost-equals? (p/transform (g/position (complex 2 0))
                                       (g/reflection one (unit 90)))
                          (g/position zero)
                          1E-7)
        "reflection in line through one parallel to y-axis")
    (is (p/equals? (p/transform (g/position (complex 2 0))
                                (g/inversion))
                   (g/position (complex (/ 2) 0)))
        "inversion in unit circle works")
    (is (p/equals? (p/transform (g/position (complex 4 0))
                                (g/inversion (complex 2 0) 2))
                   (g/position (complex 4 0)))
        "inversion in circle at center [2 0] with radius 2 works")
    (is (p/equals? (p/transform (g/position (complex 5 0))
                                (g/inversion (complex 2 0) 2))
                   (g/position (complex (/ 10 3) 0)))
        "inversion in circle at center [2 0] with radius 2 works")
    (is (p/equals? (p/transform (g/position zero)
                                (g/inversion))
                   (g/position infinity))
        "inversion in unit circle takes zero to infinity")
    (is (p/equals? (p/transform (g/position infinity)
                                (g/inversion))
                   (g/position zero))
        "inversion in unit circle takes infinity to zero")
    (is (p/equals? (p/transform (g/position one)
                                (g/inversion one 1))
                   (g/position infinity))
        "inversion in circle takes center to infinity")
    (is (p/equals? (p/transform (g/position infinity)
                                (g/inversion one 1))
                   (g/position one))
        "inversion in circle takes infinity to center")))

(comment
  (require '[turtle-geometry.geometry-test] :reload)
  (in-ns 'turtle-geometry.geometry-test)
  (clojure.test/run-tests)
  )
