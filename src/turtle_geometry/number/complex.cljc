(ns turtle-geometry.number.complex
  "complex number implementation using rational roots
  providing units in multiples of 15 degrees"
  (:require [turtle-geometry.protocols :as p]))

(defrecord Complex [x y]
  p/Addition
  (add [_ w]
    (->Complex (p/add x (:x w))
               (p/add y (:y w))))
  (negative [_]
    (->Complex (p/negative x) (p/negative y)))
  (zero? [_]
    (and (zero? x) (zero? y)))

  p/Multiplication
  (multiply [_ w]
    (if (number? w)
      (->Complex (p/multiply x w) (p/multiply y w))
      (->Complex (p/add (p/multiply x (:x w))
                        (p/negative (p/multiply y (:y w))))
                 (p/add (p/multiply x (:y w))
                        (p/multiply y (:x w))))))
  (reciprocal [_]
    (let [r (+ (p/multiply x x) (p/multiply y y))
          r-recip (p/reciprocal r)]
      (->Complex (p/multiply x r-recip)
                 (p/multiply (p/negative y) r-recip))))
  (one? [_]
    (and (p/equals? x 1) (p/equals? y 0)))

  p/Conjugate
  (conjugate [_]
    (->Complex x (p/negative y)))

  p/Equality
  (equals? [_ w]
    (and (p/equals? x (:x w))
         (p/equals? y (:y w))))
  (almost-equals? [_ w epsilon]
    (and (p/almost-equals? x (:x w) epsilon)
         (p/almost-equals? y (:y w) epsilon)))

  p/Evaluate
  (evaluate [_]
    [(p/evaluate x) (p/evaluate y)]))

(defn complex
  "complex constructor"
  [x y]
  (->Complex x y))

(def zero (complex 0 0))
(def one (complex 1 0))
(def i (complex 0 1))

(defn swap-x-y
  "reflect in y=x axis"
  [{:keys [x y]}]
  (->Complex y x))
