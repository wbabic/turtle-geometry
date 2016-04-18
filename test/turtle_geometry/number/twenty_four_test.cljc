(ns turtle-geometry.number.twenty-four-test
  "tests for the complex numbers behind the twenty-four-fold turtle"
  (:require [turtle-geometry.protocols :as p]
            [turtle-geometry.number :as n]
            [turtle-geometry.number.unit :as u]
            [turtle-geometry.number.units.twenty-four :as units]
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

(deftest unity
  (testing "unity works as expected"
    (is (p/one? (units/unit 0)))
    (is (p/one? (units/unit 360)))
    (is (p/one? (units/unit -360)))
    (is (p/one? (units/unit 720)))))

(defn unit-vector [angle]
  [(Math/cos (n/deg->rad angle))
   (Math/sin (n/deg->rad angle))])

(defn evaluates? [epsilon angle complex]
  (let [[c s] (unit-vector angle)
        [c-u s-u] (p/evaluate complex)]
    (and (n/almost-equals epsilon c c-u)
         (n/almost-equals epsilon s s-u))))

(deftest evaluation
  (testing "that units evaluate to cos sin"
    (let [epsilon 1E-14]
      (doseq [angle (map #(* % 15) (range 25))]
        (let [u (units/unit angle)]
          (is (evaluates? epsilon angle u) (str "testing angle " angle)))))))

(comment
  (require '[turtle-geometry.number.twenty-four-test] :reload)
  (in-ns 'turtle-geometry.number.twenty-four-test)
  (clojure.test/run-tests)
  )
