(ns turtle-geometry.number.real
  "protocol implementation for numbers"
  (:require [turtle-geometry.protocols :as p]))

(extend-protocol p/Addition
  Number
  (p/add [x y]
    (if (number? y)
      (+ x y)
      (p/add y x)))
  (p/negative [x] (- x))
  (p/zero? [x] (zero? x)))

(extend-protocol p/Multiplication
  Number
  (p/multiply [x y]
    (if (number? y)
      (* x y)
      (p/multiply y x)))
  (p/reciprocal [x] (/ x))
  (p/one? [x] (== 1 x)))

(extend-protocol p/Conjugate
  Number
  (p/conjugate [x] x))

(extend-protocol p/Equality
  Number
  (equals? [x y]
    (== x y))
  (almost-equals? [x y epsilon]
    (< (Math/abs (- x y)) epsilon)))

(extend-protocol p/Evaluate
  Number
  (p/evaluate [x] x))

(comment
  (require '[turtle-geometry.number.real] :reload)
  (in-ns 'turtle-geometry.number.real)

  (p/evaluate 1.2)
  (p/negative 1.2)
  (p/zero? 1.2)
  (p/zero? 0.0)
  (p/add 1 1)
  (p/one? 1.0)
  (p/reciprocal 2)
  (p/zero? (p/add 2 (p/negative 2)))
  (p/one? (p/multiply 2 (p/reciprocal 2)))
  )
