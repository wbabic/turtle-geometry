(ns turtle-geometry.protocols
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

;; geometry
(defprotocol Transform
  (inverse [transformation])
  (transform-fn [transform]))

(defprotocol Transformable
  (transform [object transformation]))

(comment
  (require '[turtle-geometry.protocols] :reload)
  (in-ns 'turtle-geometry.protocols)
  )
