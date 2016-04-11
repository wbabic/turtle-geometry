(ns turtle-geometry.protocols
  "protocols for algebra of number and transform"
  (:refer-clojure :exclude [zero?])
  (:require [clojure.core :as core]))

;; number
(defprotocol Addition
  (add [x y])
  (negative [x])
  (zero? [x]))

(defprotocol Multiplication
  (multiply [x y])
  (reciprocal [z])
  (one? [x]))

(defprotocol Conjugate
  (conjugate [x]))

(defprotocol Equality
  (equals? [x y]))

(defprotocol Evaluate
  (evaluate [x]))

(defprotocol Unit
  (unit [theta]))

;; turtle
(defprotocol Turtle
  (move [turtle d])
  (turn [turtle a])
  (resize [turtle r])
  (reflect [turtle])
  (invert [turtle]))

(defprotocol Heading
  (angle [_] "angle of heading")
  (length [_] "length of heading")
  (complex [_] "heading represented as a complex number"))

;; geometry
(defprotocol Transform
  (inverse [transformation])
  (transform-fn [transform]))

(defprotocol Transformable
  (transform [object transformation]))
