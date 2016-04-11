(ns turtle-geometry.number.units.twenty-four
  "24 units of unity"
  (:require [turtle-geometry.protocols :as p]
            [turtle-geometry.number.complex :as complex :refer [one i]]
            [turtle-geometry.number.unit :as unit]
            [turtle-geometry.number.real :as real]
            [turtle-geometry.number.root :as root]))

;; from right isosceles triangle
;; and the Pythagorean theorem
(def unit-45
  (let [x (p/multiply root/rt2 (/ 2))]
    (complex/->Complex x x)))

;; from an equilateral triangle
(def unit-60
  (let [y (p/multiply root/rt3 (/ 2))]
    (complex/->Complex (/ 2) y)))

(defn swap-x-y
  "reflect in y=x axis"
  [{:keys [x y]}]
  (complex/->Complex y x))

(def unit-30 (swap-x-y unit-60))

;; using de Moivre's and Euler's formulas
(def unit-15
  (p/multiply unit-60 (p/conjugate unit-45)))

(def unit-75 (swap-x-y unit-15))

(def unit->complex
  {0 one
   15 unit-15
   30 unit-30
   45 unit-45
   60 unit-60
   75 unit-75
   90 i
   105 (p/multiply i unit-15)
   120 (p/multiply i unit-30)
   135 (p/multiply i unit-45)
   150 (p/multiply i unit-60)
   165 (p/multiply i unit-75)
   180 (p/negative one)})

;; exact representations of units with angle being multiples of 15
(defn unit [angle]
  (assert (zero? (mod angle 15)))
  (let [a (mod angle 360)]
    (if (<= a 180)
      (unit->complex a)
      (p/multiply (p/negative one) (unit (- a 180))))))

(extend-protocol p/Unit
  Number
  (p/unit [angle-in-degrees]
    (unit angle-in-degrees)))

(comment
  (require '[turtle-geometry.number.units.twenty-four] :reload)
  (in-ns 'turtle-geometry.number.units.twenty-four)
  (use 'clojure.repl)

  unit-15
  #turtle_geometry.number.complex.Complex
  {:x #turtle_geometry.number.root.RationalRoot
   {:ratio 0,
    :roots (#turtle_geometry.number.root.Root{:base 2, :multiplier 1/4}
            #turtle_geometry.number.root.Root{:base 6, :multiplier 1/4})},
   :y #turtle_geometry.number.root.RationalRoot
   {:ratio 0,
    :roots (#turtle_geometry.number.root.Root{:base 2, :multiplier -1/4}
            #turtle_geometry.number.root.Root{:base 6, :multiplier 1/4})}}

  ;; units of the first quadrant
  (map clojure.pprint/pprint
       (take 7
             (iterate
              #(p/multiply (p/unit 15) %)
              one)))
  )
