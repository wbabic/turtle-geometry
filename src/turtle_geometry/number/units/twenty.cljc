(ns turtle-geometry.number.units.twenty
  (:require [turtle-geometry.protocols :as p]
            [turtle-geometry.number.complex :as complex :refer [one i]]
            [turtle-geometry.number.real :as real]
            [turtle-geometry.number.root :as root :refer [phi Phi]]))

(def cos-18
  (let [r (p/add Phi 2)]
    (root/root r (/ 2))))

(def sin-18
  (p/multiply phi (/ 2)))

(def cos-36
  (p/multiply Phi (/ 2)))

(def sin-36
  (let [r (p/add (p/negative phi) 2)]
    (root/root r (/ 2))))

(def unit-18 (complex/->Complex cos-18 sin-18))
(def unit-36 (complex/->Complex cos-36 sin-36))
(def unit-54 (complex/swap-x-y unit-36))
(def unit-72 (complex/swap-x-y unit-18))

(def unit->complex
  {0 one
   18 unit-18
   36 unit-36
   54 unit-54
   72 unit-72
   90 i})

;; exact representations of units with angle being multiples of 18
(defn unit [angle]
  (assert (zero? (mod angle 18)))
  (let [a (mod angle 360)]
    (if (<= a 180)
      (if (<= a 90)
        (unit->complex a)
        (p/multiply i (unit (- a 90))))
      (p/multiply (p/negative one) (unit (- a 180))))))

(comment
  (require '[turtle-geometry.number.units.twenty] :reload)
  (in-ns 'turtle-geometry.number.units.twenty)

  )
