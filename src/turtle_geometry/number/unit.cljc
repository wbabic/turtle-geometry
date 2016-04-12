(ns turtle-geometry.number.unit
  "units of the plane"
  (:require [turtle-geometry.protocols :as p]
            [turtle-geometry.number.complex :as complex :refer [one i]]
            [turtle-geometry.number.real :as real]
            [turtle-geometry.number.root :as root]))

(defrecord Unit [angle]
  p/Addition
  (negative [_] (->Unit (+ angle 180)))
  (zero? [_] false)

  p/Multiplication
  (multiply [_ u]
    (->Unit (+ angle (:angle u))))
  (reciprocal [_] (->Unit (- 360 angle)))
  (one? [_] (= 0 (mod angle 360)))

  p/Conjugate
  (conjugate [_] (->Unit (- angle)))

  p/Equality
  (equals? [_ u]
    (== 0 (mod (- angle (:angle u)) 360))))

(defn unit
  "unit constructor"
  ([] (unit 0))
  ([angle]
   (->Unit angle)))

(comment
  (require '[turtle-geometry.number.unit] :reload)
  (in-ns 'turtle-geometry.number.unit)
  (use 'clojure.repl)

  (unit 15)
  (p/multiply (unit 15) (unit 30))
  )
