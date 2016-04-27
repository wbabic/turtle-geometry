(ns turtle-geometry.number.twenty-test
  "tests for the complex numbers behind the twenty-fold turtle"
  (:require [turtle-geometry.protocols :as p]
            [turtle-geometry.number :as n]
            [turtle-geometry.number.units.twenty :as units]
            [turtle-geometry.number.root :as root]
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

(defn unit-vector [angle]
  [(Math/cos (n/deg->rad angle))
   (Math/sin (n/deg->rad angle))])

(defn evaluates? [epsilon angle complex]
  (let [[c s] (unit-vector angle)
        [c-u s-u] (p/evaluate complex)]
    (and (n/almost-equals epsilon c c-u)
         (n/almost-equals epsilon s s-u))))

(deftest unit-18
  (testing "cos-18 and sin-18 evaluate correctly"
    (is (evaluates? 1E-16
                    18
                    units/unit-18))
    (is (p/equals? (Math/cos (n/deg->rad 18)) (p/evaluate units/cos-18)))
    (is (p/equals? (Math/cos (n/deg->rad 36)) (p/evaluate units/cos-36)))
    (is (p/equals? (Math/sin (n/deg->rad 36)) (p/evaluate units/sin-36)))
    (is (p/equals? (Math/cos (n/deg->rad 54)) (p/evaluate (:x units/unit-54))))
    (is (p/equals? (Math/sin (n/deg->rad 54)) (p/evaluate (:y units/unit-54))))))

(deftest evaluation
  (testing "that units evaluate to cos sin"
    (let [epsilon 1E-14]
      (doseq [angle (map #(* % 18) (range 21))]
        (let [u (units/unit angle)]
          (is (evaluates? epsilon angle u) (str "testing angle " angle)))))))

(deftest multiplication
  (testing "multiplication of unit-18"
    (is (p/equals? (p/multiply units/sin-18 units/sin-18)
                   (p/multiply (p/add (p/negative root/rt5) 3) (/ 8))))))

(comment
  (require '[turtle-geometry.number.twenty-test] :reload)
  (in-ns 'turtle-geometry.number.twenty-test)
  (clojure.test/run-tests)

  (p/multiply units/cos-18 units/sin-18)
)
