(ns turtle-geometry.geometry.inversion-test
  "geometry test using inexact complex numbers"
  (:require [turtle-geometry.protocols :as p]
            [turtle-geometry.geometry :as g]
            [turtle-geometry.turtle :as turtle]
            [turtle-geometry.number :as n]
            [turtle-geometry.mappings :as m]
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

;; generate circles
;; position point center and a radius

;; inversion in that circle
;; should then have the property that
;; the image of any point z is
;; on the line from center to z and it's distance from the center
;; times the distance of z from the center is the radius squared


;; generate a position point
;; generate a complex number

(def complex-gen
  (gen/fmap (partial apply n/complex)
            (gen/vector gen/ratio 2)))

(def point-gen
  (gen/fmap g/position complex-gen))

(def circle-gen
  (gen/fmap (partial apply g/circle)
            (gen/tuple point-gen gen/s-pos-int)))


(comment
  (require '[turtle-geometry.geometry.inversion-test] :reload)
  (in-ns 'turtle-geometry.geometry.inversion-test)
  (use 'clojure.repl)
  (clojure.test/run-tests)
  (clojure.pprint/pprint (gen/sample circle-gen))

  (n/length (n/complex 1 1))
  (n/difference n/one n/i)
  (n/distance n/one n/i)

  (let [l (g/line-segment (g/position n/i)
                          (g/position n/one))
        t (g/translation n/one)]
    (p/transform l t))

  (let [l (g/line-segment (g/position (n/complex 0 2))
                          (g/position (n/complex 2 0)))
        t (g/inversion)]
    (p/transform l t))

  (let [p1 (g/position n/one)
        p2 (g/position n/i)
        i (g/inversion)
        l (g/line-segment p1 p2)]
    (p/transform l i))
  )
