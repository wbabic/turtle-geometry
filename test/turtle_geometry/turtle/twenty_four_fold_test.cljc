(ns turtle-geometry.turtle.twenty-four-fold-test
  (:require [turtle-geometry.protocols :as p]
            [turtle-geometry.geometry :as g]
            [turtle-geometry.turtle :as turtle]
            [turtle-geometry.turtle.twenty-four-fold :as impl]
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
(deftest home-trans
  (testing "the transformation that brings a turtle home"
    (let [initial-turtle impl/initial-turtle
          transformed-turtle (-> initial-turtle
                                 (p/turn 15)
                                 (p/resize 10)
                                 (p/move 1)
                                 (p/reflect))]
      (is (p/equals? initial-turtle
                     (p/transform initial-turtle
                                  (turtle/home-transformation initial-turtle))))
      (is (p/equals? initial-turtle
                     (p/transform transformed-turtle
                                  (turtle/home-transformation transformed-turtle)))))))

(comment
  (require '[turtle-geometry.turtle.twenty-four-fold-test] :reload)
  (in-ns 'turtle-geometry.turtle.twenty-four-fold-test)
  (use 'clojure.repl)
  (clojure.test/run-tests)
 )
