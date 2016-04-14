(ns turtle-geometry.number.units.twenty
  (:require [turtle-geometry.protocols :as p]
            [turtle-geometry.number.complex :as complex :refer [one i]]
            [turtle-geometry.number.unit :as unit]
            [turtle-geometry.number.real :as real]
            [turtle-geometry.number.root :as root]))

(def cos-18
  (let [r (p/add root/Phi 2)]
    (root/root r (/ 2))))

(def r (root/root (p/add (p/negative root/Phi) 2)))
(def R (root/root (p/add root/phi 2)))

(def sin-18
  (let [r (p/add (p/negative root/Phi) 2)]
    (root/root r (/ 2))))

(def unit-18 (complex/->Complex cos-18 sin-18))

(def cos-36
  (let [r (p/add root/phi 2)]
    (root/root r (/ 2))))

(def sin-36
  (let [r (p/add (p/negative root/phi) 2)]
    (root/root r (/ 2))))

(def unit-36 (complex/->Complex cos-36 sin-36))

(defn swap-x-y
  "reflect in y=x axis"
  [{:keys [x y]}]
  (complex/->Complex y x))

(def unit-54 (swap-x-y unit-36))

(def unit-72 (swap-x-y unit-18))

(def unit->complex
  {0 one
   18 unit-18
   36 unit-36
   54 unit-54
   72 unit-72
   90 i})

;; exact representations of units with angle being multiples of 15
(defn unit [angle]
  (assert (zero? (mod angle 18)))
  (let [a (mod angle 360)]
    (if (<= a 180)
      (if (<= a 90)
        (unit->complex a)
        (p/multiply i (unit (- a 90))))
      (p/multiply (p/negative one) (unit (- a 180))))))

(defn almost-equals [epsilon x y]
  (< (Math/abs (- x y)) epsilon))

(comment
  (require '[turtle-geometry.number.units.twenty] :reload)
  (in-ns 'turtle-geometry.number.units.twenty)
  (clojure.test/run-tests)

  (extend-protocol p/Unit
    Number
    (p/unit [angle-in-degrees]
      (unit angle-in-degrees)))

  (p/equals? (p/evaluate r) (p/evaluate root/phi))
  (almost-equals 1E-16 (p/evaluate r) (p/evaluate root/phi))
  ;;=> false
  (almost-equals 1E-15 (p/evaluate r) (p/evaluate root/phi))
  (p/equals? (p/evaluate R) (p/evaluate root/Phi))
  ;;=> true
)
