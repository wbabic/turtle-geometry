 (ns turtle-geometry.protocols
  "protocols for algebra of number and transform"
  (:refer-clojure :exclude [zero? keyword vector]))

;; number
(defprotocol Addition
  (add [x y])
  (negative [x])
  (zero? [x]))

(defprotocol Multiplication
  (multiply [x y])
  (reciprocal [z])
  (one? [x]))

(defprotocol Unit
  (angle->complex [angle]))

(defprotocol Conjugate
  (conjugate [x]))

(defprotocol Equality
  (equals? [x y])
  (almost-equals? [x y epsilon]))

(defprotocol Evaluate
  (evaluate [x]))

;; turtle
(defprotocol Turtle
  (move [turtle d])
  (turn [turtle a])
  (resize [turtle r])
  (reflect [turtle])
  (invert [turtle]))

(defprotocol Heading
  (angle [heading] "angle of heading")
  (length [heading] "length of heading")
  (vector [heading] "return heading as a complex number"))

(defprotocol Position
  (point [point] "return position as a complex number"))

(defprotocol Orientation
  (value [_] "return the value as +1 or -1")
  (keyword [_] "return the keyword as :clockwise or :counter-clockwise"))

;; geometry
(defprotocol Transform
  (inverse [transformation])
  (transform-fn [transformation]))

(defprotocol Transformable
  (transform [object transformation]))

(defprotocol Parameterized
  (value-for [_ parameter]))

(defprotocol Renderable
  (render [_ style]))
