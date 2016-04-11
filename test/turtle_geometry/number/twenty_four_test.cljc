(ns turtle-geometry.number.twenty-four-test
  "tests for the complex numbers behind the twenty-four-fold turtle"
  (:require [turtle-geometry.protocols :as p]
            [turtle-geometry.number.unit :as u]
            [turtle-geometry.number.units.twenty-four :as n]
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
    (let [w (n/unit 15)]
      (is (p/one? (n/unit 0)))
      (is (p/one? (n/unit 360)))
      (is (p/one? (n/unit -360)))
      (is (p/one? (n/unit 720))))))

(defn deg->rad [degrees]
  (* (/ degrees 180) Math/PI))

(defn almost-equals [epsilon x y]
  (< (Math/abs (- x y)) epsilon))

(defn unit-vector [angle]
  [(Math/cos (deg->rad angle))
   (Math/sin (deg->rad angle))])

(defn evaluates? [epsilon angle complex]
  (let [[c s] (unit-vector angle)
        [c-u s-u] (p/evaluate complex)]
    (and (almost-equals epsilon c c-u)
         (almost-equals epsilon s s-u))))

(deftest evaluation
  (testing "that units evaluate to cos sin"
    (let [angle 15
          epsilon 1E-14]
      (doseq [a (map #(* % 15) (range 25))]
        (let [u (n/unit a)]
          (is (evaluates? epsilon a u) (str "testing angle " a)))))))

(comment
  (require '[turtle-geometry.number.twenty-four-test] :reload)
  (in-ns 'turtle-geometry.number.twenty-four-test)
  (clojure.test/run-tests)
  )
