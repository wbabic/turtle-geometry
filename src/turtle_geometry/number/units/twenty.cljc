(ns turtle-geometry.number.units.twenty
  (:require [turtle-geometry.protocols :as p]
            [turtle-geometry.number.complex :as complex :refer [one i]]
            [turtle-geometry.number.unit :as unit]
            [turtle-geometry.number.real :as real]
            [turtle-geometry.number.root :as root]))

(def sin-18 (p/multiply (p/add root/rt5 -1) (/ 4)))

(def r (root/rat-roots 10 (root/root 5 2)))

(def cos-18
  (let [r (root/rat-roots 10 (root/root 5 2))]
    (root/root r (/ 4))))

(def unit-18 (complex/->Complex sin-18 cos-18))

(def unit-36 (p/multiply unit-18 unit-18))

(comment
  (require '[turtle-geometry.number.units.twenty] :reload)
  (in-ns 'turtle-geometry.number.units.twenty)
  )
