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

(defn unit
  ([angle] (unit angle 1))
  ([angle length]
   (complex/complex
    (* length (Math/cos (n/deg->rad angle)))
    (* length (Math/sin (n/deg->rad angle))))))

(comment
  (require '[turtle-geometry.number.units.polar] :reload)
  (in-ns 'turtle-geometry.number.units.polar)
  )
