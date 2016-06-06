(ns turtle-geometry.number.complex
  "complex number implementation with infinity"
  (:require [turtle-geometry.protocols :as p]))

(def undefined
  (reify
    p/Addition
    (add [z _] z)
    (negative [z] z)
    (zero? [_] false)
    p/Multiplication
    (multiply [z _] z)
    (reciprocal [z] z)
    (one? [_] false)
    p/Equality
    (equals? [_ z]
      (= z undefined))))

(declare zero complex)

(def infinity
  (reify
    p/Addition
    (add [z _] z)
    (negative [z] undefined)
    (zero? [_] false)

    p/Multiplication
    (multiply [z w]
      (if (p/zero? w) undefined infinity))
    (reciprocal [_] zero)
    (one? [_] false)

    p/Conjugate
    (conjugate [_] infinity)

    p/Equality
    (equals? [_ z]
      (= z infinity))))

(defn isundefined? [z] (= undefined z))
(defn infinity? [z] (= infinity z))

(defrecord Complex [x y]
  p/Addition
  (add [_ w]
    (if (infinity? w) infinity
        (complex (p/add x (:x w))
                   (p/add y (:y w)))))
  (negative [_]
    (complex (p/negative x) (p/negative y)))
  (zero? [_]
    (and (zero? x) (zero? y)))

  p/Multiplication
  (multiply [z w]
    (cond (number? w)
          (complex (p/multiply x w) (p/multiply y w))
          (infinity? w) (if (p/zero? z) undefined infinity)
          :else
          (complex (p/add (p/multiply x (:x w))
                            (p/negative (p/multiply y (:y w))))
                     (p/add (p/multiply x (:y w))
                            (p/multiply y (:x w))))))
  (reciprocal [z]
    (if (p/zero? z) infinity
        (let [r (+ (p/multiply x x) (p/multiply y y))
              r-recip (p/reciprocal r)]
          (complex (p/multiply x r-recip)
                     (p/multiply (p/negative y) r-recip)))))
  (one? [_]
    (and (p/equals? x 1) (p/equals? y 0)))

  p/Conjugate
  (conjugate [_]
    (complex x (p/negative y)))

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

(defn length-sq [{:keys [x y]}]
  (+ (* x x) (* y y)))

(defn length [z]
  (Math/sqrt (length-sq z)))

(defn difference
  "returns the vector from z to w"
  [z w]
  (p/add (p/negative z) w))

(defn distance-sq
  "return the distance between z and w"
  [z w]
  (length-sq (difference z w)))

(defn distance
  "return the distance between z and w"
  [z w]
  (length (difference z w)))

(comment
  (require '[turtle-geometry.number.complex] :reload)
  (in-ns 'turtle-geometry.number.complex)
  )
