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
    (->Complex (p/add (p/multiply x (:x w))
                      (p/negative (p/multiply y (:y w))))
               (p/add (p/multiply x (:y w))
                      (p/multiply y (:x w)))))
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
    (and (== x (:x w))
         (== y (:y w))))

  p/Evaluate
  (p/evaluate [_]
    [(p/evaluate x) (p/evaluate y)])
  )

(def zero (->Complex 0 0))
(def one (->Complex 1 0))
(def i (->Complex 0 1))

(def unit-30
  (let [x (p/multiply root/rt3 (/ 2))]
    (->Complex x (/ 2))))

(def unit-60
  (let [y (p/multiply root/rt3 (/ 2))]
    (->Complex (/ 2) y)))

(def unit-45
  (let [x (p/multiply root/rt2 (/ 2))]
    (->Complex x x)))

(def unit-15
  (p/multiply unit-60 (p/conjugate unit-45)))

(comment
  (require '[turtle-geometry.number.complex] :reload)
  (in-ns 'turtle-geometry.number.complex)

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
  )
