(ns turtle-geometry.number.complex
  "complex number implementation"
  (:require [turtle-geometry.protocols :as p]
            [turtle-geometry.number.real :as real]
            [turtle-geometry.number.root :as root]))

(defrecord Complex [x y]
  p/Addition
  (p/add [_ w]
    (->Complex (p/add x (:x w))
               (p/add y (:y w))))
  (p/negative [_]
    (->Complex (p/negative x) (p/negative y)))
  (p/zero? [_]
    (and (zero? x) (zero? y)))

  p/Multiplication
  (p/multiply [_ w]
    (if (number? w)
      (->Complex (p/multiply x w) (p/multiply y w))
      (->Complex (p/add (p/multiply x (:x w))
                        (p/negative (p/multiply y (:y w))))
                 (p/add (p/multiply x (:y w))
                        (p/multiply y (:x w))))))
  (p/reciprocal [_]
    (let [r (+ (* x x) (* y y))]
      (->Complex (/ x r)
                 (/ (- y) r))))
  (p/one? [_]
    (and (== x 1) (== y 0)))

  p/Conjugate
  (p/conjugate [_]
    (->Complex x (p/negative y)))

  p/Equality
  (p/equals? [_ w]
    (and (p/equals? x (:x w))
         (p/equals? y (:y w))))

  p/Evaluate
  (p/evaluate [_]
    [(p/evaluate x) (p/evaluate y)]))

(def zero (->Complex 0 0))
(def one (->Complex 1 0))
(def i (->Complex 0 1))

(def unit-45
  (let [x (p/multiply root/rt2 (/ 2))]
    (->Complex x x)))

(def unit-60
  (let [y (p/multiply root/rt3 (/ 2))]
    (->Complex (/ 2) y)))

(defn swap-x-y [{:keys [x y]}]
  (->Complex y x))

(def unit-30 (swap-x-y unit-60))

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

;; representations of multiples of 15
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
  (require '[turtle-geometry.number.complex] :reload)
  (in-ns 'turtle-geometry.number.complex)
  (use 'clojure.repl)

  (p/conjugate one)
  (p/conjugate i)
  (p/add one i)
  (p/negative one)
  (p/equals? (p/add one i) (->Complex 1 1))
  (p/equals? (p/multiply i i) (->Complex -1 0))
  (p/equals? (p/multiply i (p/reciprocal i)) one)
  (p/one? (p/multiply i (p/reciprocal i)))
  (let [w (->Complex 1 2)]
    (p/equals? (p/multiply w (p/reciprocal w)) one))
  (let [w (->Complex 1 2)]
    (p/one? (p/multiply w (p/reciprocal w))))
  (let [w (->Complex 1 2)]
    (p/zero? (p/add w (p/negative w))))
  (p/evaluate one)
  (p/evaluate (->Complex (/ 2) root/omega))
  (p/evaluate unit-30)
  (p/evaluate unit-45)
  (p/conjugate unit-30)
  (p/equals? (p/multiply unit-45 unit-45) i)

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

  (p/equals? (p/multiply unit-30 unit-60) i)
  (p/equals? (unit 15) (unit 375))

  (p/multiply (unit 15) (unit 30))
  #turtle_geometry.number.complex.Complex
  {:x #turtle_geometry.number.root.Root
   {:base 2, :multiplier 1/2},
   :y #turtle_geometry.number.root.Root
   {:base 2, :multiplier 1/2}}

  (p/equals? unit-30 (p/multiply (unit 15) (unit 15)))
  (p/equals? unit-45 (p/multiply (unit 15) (unit 30)))
  (p/equals? i (p/multiply (unit 15) (unit 75)))
  (p/one? (p/multiply (unit 15) (unit (- 360 15))))
  ;;=> true

  )
