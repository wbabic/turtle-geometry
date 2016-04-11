(ns turtle-geometry.turtle.twenty-four-fold-test
  (:require [turtle-geometry.protocols :as p]
            [turtle-geometry.geometry :as g]
            [turtle-geometry.turtle :as t]
            [turtle-geometry.turtle.twenty-four-fold :as turtle]
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
    (let [initial-turtle turtle/initial-turtle
          transformed-turtle (last (take 25
                                         (iterate #(p/turn % 15) initial-turtle)))]
      (is (p/equals? initial-turtle transformed-turtle)))))

;; turtle transforms

(comment
  (require '[turtle-geometry.turtle.twenty-four-fold-test] :reload)
  (in-ns 'turtle-geometry.turtle.twenty-four-fold-test)
  (use 'clojure.repl)
  (clojure.test/run-tests)
 )
