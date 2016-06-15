(ns turtle-geometry.geometry.inversion-test
  "geometry test using inexact complex numbers"
  (:require [turtle-geometry.protocols :as p]
            [turtle-geometry.geometry :as g]
            [turtle-geometry.turtle :as turtle]
            [turtle-geometry.number :as complex]
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
  (gen/fmap (partial apply complex/complex)
            (gen/vector gen/ratio 2)))

(def point-gen
  (gen/fmap g/position complex-gen))

(def circle-gen
  (gen/fmap (partial apply g/circle)
            (gen/tuple point-gen gen/s-pos-int)))


(comment
  (require '[turtle-geometry.geometry.inversion-test] :reload)
  (in-ns 'turtle-geometry.geometry.inversion-test)
  (clojure.test/run-tests)
  (clojure.pprint/pprint (gen/sample circle-gen))
  (complex/length (complex/complex 1 1))
  (complex/difference complex/one complex/i)
  (complex/distance complex/one complex/i)
  )
