(ns turtle-geometry.number.units.polar
  "inexact representations of polar numbers for all angles"
  (:require [turtle-geometry.protocols :as p]

            [turtle-geometry.number.complex :as n :refer [one i]]
            [turtle-geometry.number.unit :as unit]
            [turtle-geometry.number.real :as real]
            [turtle-geometry.number.root :as root]))

(def ^:const PI Math/PI)
(def TAU (* 2 PI))
(defn mod-tau [x] (mod x TAU))

(defn deg->rad [angle]
  (* (/ angle 180) Math/PI))

(defn angle [complex]
  (mod-tau (Math/atan2 (:y complex) (:x complex))))

(defn length-sq [{:keys [x y]}]
  (+ (* x x) (* y y)))

(defn length [complex]
  (Math/sqrt (length-sq complex)))

(defn unit
  ([angle] (unit angle 1))
  ([angle length]
   (n/complex
    (* length (Math/cos (deg->rad angle)))
    (* length (Math/sin (deg->rad angle))))))

(comment
  (require '[turtle-geometry.number.units.polar] :reload)
  (in-ns 'turtle-geometry.number.units.polar)
  )
