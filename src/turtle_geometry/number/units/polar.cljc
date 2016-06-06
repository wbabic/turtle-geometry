(ns turtle-geometry.number.units.polar
  "inexact representations of polar numbers for all angles"
  (:require [turtle-geometry.protocols :as p]
            [turtle-geometry.number :as n]
            [turtle-geometry.number.complex :as complex :refer [one i]]))

(defn angle [complex]
  (n/mod-tau (Math/atan2 (:y complex) (:x complex))))

(defn length-sq [{:keys [x y]}]
  (+ (* x x) (* y y)))

(defn length [complex]
  (Math/sqrt (length-sq complex)))

(defn angle->complex
  ([angle] (angle->complex angle 1))
  ([angle length]
   (complex/complex
    (* length (Math/cos (n/deg->rad angle)))
    (* length (Math/sin (n/deg->rad angle))))))

(declare unit)

(defrecord Unit [angle]
  p/Unit
  (angle->complex [_] (angle->complex angle))

  p/Multiplication
  (multiply [_ u]
    (unit (+ angle (:angle u))))
  (reciprocal [_] (unit (- 360 angle)))
  (one? [_] (= 0 (mod angle 360)))

  p/Conjugate
  (conjugate [_] (unit (- angle)))

  p/Equality
  (equals? [_ u]
    (== 0 (mod (- angle (:angle u)) 360)))
  (almost-equals? [_ u epsilon]
    (p/almost-equals? 0 (mod (- angle (:angle u)) 360) epsilon)))

(defn unit [angle]
  (->Unit angle))

(comment
  (require '[turtle-geometry.number.units.polar] :reload)
  (in-ns 'turtle-geometry.number.units.polar)

  (p/equals? one (angle->complex 0))
  ;;=> true
  (n/rad->deg (angle i))
  ;;=> 90.0
  (p/almost-equals? i (angle->complex 90) 1E-16)
  ;;=> true
  (p/equals? i (angle->complex 90))
  ;;=> false
  )
