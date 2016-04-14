(ns turtle-geometry.number.twenty-test
  "tests for the complex numbers behind the twenty-fold turtle"
  (:require [turtle-geometry.protocols :as p]
            [turtle-geometry.number.unit :as u]
            [turtle-geometry.number.units.twenty :as n]
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

(deftest unit-18
  (testing "cos-18 and sin-18 evaluate correctly"
    (is (evaluates? 1E-16
                    18
                    n/unit-18))
    (is (p/equals? (Math/cos (deg->rad 18)) (p/evaluate n/cos-18)))
    (is (p/equals? (Math/sin (deg->rad 18)) (p/evaluate n/sin-18)))
    (is (p/equals? (Math/cos (deg->rad 36)) (p/evaluate n/cos-36)))
    (is (p/equals? (Math/sin (deg->rad 36)) (p/evaluate n/sin-36)))
    (is (p/equals? (Math/cos (deg->rad 54)) (p/evaluate (:x n/unit-54))))
    (is (p/equals? (Math/sin (deg->rad 54)) (p/evaluate (:y n/unit-54))))))

(deftest evaluation
  (testing "that units evaluate to cos sin"
    (let [epsilon 1E-14]
      (doseq [angle (map #(* % 18) (range 21))]
        (let [u (n/unit angle)]
          (is (evaluates? epsilon angle u) (str "testing angle " angle)))))))

(comment
  (require '[turtle-geometry.number.twenty-test] :reload)
  (in-ns 'turtle-geometry.number.twenty-test)
  (clojure.test/run-tests)
)
