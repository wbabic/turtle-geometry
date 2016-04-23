(ns turtle-geometry.protocols
  "protocols for algebra of number and transform"
  (:refer-clojure :exclude [zero? keyword])
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
  (equals? [x y])
  (almost-equals? [x y epsilon]))

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
  (length [_] "length of heading"))
(defprotocol Complex
  (complex [_]))
(defprotocol Orientation
  (value [_])
  (keyword [_]))

;; geometry
(defprotocol Transform
  (inverse [transformation])
  (transform-fn [transform]))

(defprotocol Transformable
  (transform [object transformation]))
