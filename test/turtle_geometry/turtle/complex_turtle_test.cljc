(ns turtle-geometry.turtle.complex-turtle-test
  (:require [turtle-geometry.protocols :as p]
            [turtle-geometry.geometry :as g]
            [turtle-geometry.turtle :as turtle]
            [turtle-geometry.turtle.complex :as impl]
            [turtle-geometry.number.complex :as n]
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

;; twenty four fold turtle
(deftest twenty-four-fold
  (testing "24 turns brings a turtle home"
    (let [initial-turtle impl/initial-turtle
          transformed-turtle (last (take 25
                                         (iterate #(p/turn % 15) initial-turtle)))]
      (is (p/equals? initial-turtle transformed-turtle)))))

(deftest twenty-four-fold-from
  (testing "24 turns brings a turtle home, but not always exactly"
    (let [t0 impl/initial-turtle
          t1 (-> t0 (p/turn 3) (p/move 1))
          t2 (last (take 25 (iterate #(p/turn % 15) t1)))
          t3 (-> t2 (p/move -1) (p/turn -3))]
      (is (p/equals? t1 t2))
      (is (not (p/equals? t0 t3)))
      (is (p/almost-equals? t0 t3 1E-15))
      (is (not (p/almost-equals? t0 t3 1E-16))))))

(comment
  (require '[turtle-geometry.turtle.complex-turtle-test] :reload)
  (in-ns 'turtle-geometry.turtle.complex-turtle-test)
  (clojure.test/run-tests)
  )
