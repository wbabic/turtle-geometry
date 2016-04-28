(ns turtle-geometry.turtle.twenty-four-fold-test
  (:require [turtle-geometry.protocols :as p]
            [turtle-geometry.geometry :as g]
            [turtle-geometry.turtle :as turtle]
            [turtle-geometry.turtle.twenty-four-fold :as impl]
            [turtle-geometry.number.units.twenty-four :as units :refer [unit]]
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

(deftest twenty-four-fold-turtle
  (testing "various turtle tests"
    (let [initial-turtle impl/initial-turtle]
      (is (p/equals? initial-turtle initial-turtle) "initial turtle equals itself")
      (is (p/equals? initial-turtle
                     (-> initial-turtle
                         (p/turn 15)
                         (p/turn (- 360 15))))
          "initial-turtle equals itself after a full turn")
      (is (p/equals? (-> initial-turtle (p/move 10))
                     (-> initial-turtle
                         (p/resize 10)
                         (p/move 1)
                         (p/resize (/ 10))))
          "move and resize initial turtle")
      (is (p/equals? (-> initial-turtle
                         (p/move 10)
                         (p/turn 15))
                     (-> initial-turtle
                         (p/resize 10)
                         (p/move 1)
                         (p/turn -345)
                         (p/resize (/ 10))))
          "move resize and turn")
      (is (not (p/equals? initial-turtle (p/turn initial-turtle 15)))
          "not all turtles are equal"))))

;; turtle transforms
(deftest basic-trans
  (testing "the basic transformations behave as expected"
    (let [initial-turtle (impl/turtle)]
      (is (p/equals? (p/transform (turtle/heading (unit 0)) (g/->Rotation (unit 15)))
                     (turtle/heading (unit 15)))
          "heading transforms")
      (is (p/equals? (p/transform initial-turtle
                                  (g/->Translation (n/complex 2 3)))
                     (impl/turtle (turtle/position (n/complex 2 3))))
          "translate turtle")
      (is (p/equals? (p/transform
                      initial-turtle
                      (g/compose
                       (g/->Rotation (unit 15))
                       (g/->Translation (n/complex 2 3))))
                     (impl/turtle (turtle/position (n/complex 2 3))
                                  (turtle/heading (unit 15))))
          "rotate and translate turtle")
      (is (p/equals? (p/transform
                      initial-turtle
                      (g/compose
                       (g/->Reflection)
                       (g/->Rotation (unit 15))
                       (g/->Translation (n/complex 2 3))))
                     (impl/turtle (turtle/position (n/complex 2 3))
                                  (turtle/heading (unit 15))
                                  (turtle/orientation -1)))
          "rotate, translate and reflect turtle"))))

(deftest turtle->home
  (testing "the transformation that brings a turtle home"
    (let [t0 impl/initial-turtle
          t1 (-> t0
                 (p/turn 15)
                 (p/resize 10)
                 (p/move 1)
                 (p/reflect))]
      (is (p/equals? t0 (p/transform t0 (turtle/turtle->home t0))))
      (is (p/equals? t0 (p/transform t1 (turtle/turtle->home t1))))
      (is (p/equals? t1 (p/transform t0 (turtle/home->turtle t1)))))))

(deftest turtle-centric-trans
  (testing "rotation and reflection wrt a turtle"
    (let [t0 (impl/turtle)
          t1 (-> t0 (p/move 3) (p/turn 90))
          g (turtle/turtle-centric-transformation t1 (g/->Reflection))
          h (turtle/turtle-centric-transformation t1 (g/->Rotation (unit -90)))
          t2 (p/transform t0 g)
          t3 (p/transform t0 h)]
      (is (p/equals? t2 (impl/turtle (turtle/position (n/complex 6 0))
                                     (turtle/heading (unit 180))
                                     (turtle/orientation -1))))
      (is (p/equals? t3 (impl/turtle (turtle/position (n/complex 3 3))
                                     (turtle/heading (unit -90))))))))

(deftest reflective-turtle
  (testing "basic properties of a reflected turtle"
    (let [t0 (impl/turtle)
          t1 (-> t0 (p/reflect) (p/turn 15))
          t (g/compose (g/->Rotation (unit 15)) (g/->Reflection))
          s (turtle/home->turtle t1)]
      (is (= -1 (-> t1 :orientation :value))
          "a reflected turtle has reversed orientation")
      (is (= -15 (-> t1 :heading :unit :angle))
          "reflection changes rotation direction")
      (is (p/equals? t1 (p/transform t0 t))
          "t is equivalent transformation")
      (is (p/equals? t1 (p/transform t0 s))
          "home->turtle works as expected"))))

(comment
  (require '[turtle-geometry.turtle.twenty-four-fold-test] :reload)
  (in-ns 'turtle-geometry.turtle.twenty-four-fold-test)
  (use 'clojure.repl)
  (clojure.test/run-tests)
 )
