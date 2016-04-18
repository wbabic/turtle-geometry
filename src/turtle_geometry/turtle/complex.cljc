(ns turtle-geometry.turtle.complex
  "a turtle implementation using complex numbers
  providing inexact representation for rotations in any angle"
  (:require [turtle-geometry.protocols :as p]
            [turtle-geometry.geometry :as g]
            [turtle-geometry.turtle :as t]
            [turtle-geometry.number.unit :as u]
            [turtle-geometry.number.complex :as n]
            [turtle-geometry.number.units.polar :as units])
  (:import  [turtle_geometry.turtle Heading]))

(extend-protocol p/Complex
  Heading
  (complex [h] (p/multiply (units/unit (:angle (:unit h))) (:length h))))

(comment
  (require '[turtle-geometry.turtle.complex] :reload)
  (in-ns 'turtle-geometry.turtle.complex)

  (t/heading)
  (p/complex (t/heading))
  )
