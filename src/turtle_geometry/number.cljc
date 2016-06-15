(ns turtle-geometry.number
  "complex number implementation with infinity"
  (:require [turtle-geometry.protocols :as p]))

(def ^:const PI Math/PI)
(def ^:const TAU (* 2 PI))
(defn mod-tau [x] (mod x TAU))

(defn deg->rad [degrees]
  (* (/ degrees 180) Math/PI))

(defn rad->deg [radians]
  (* (/ radians Math/PI) 180))

(defn almost-equals? [x y epsilon]
  (< (Math/abs (- x y)) epsilon))

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
      (if (number? w)
        (if (zero? w) undefined infinity)
        (if (p/zero? w) undefined infinity)))
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
        (complex (+ x (:x w))
                 (+ y (:y w)))))
  (negative [_]
    (complex (- x) (- y)))
  (zero? [_]
    (and (zero? x) (zero? y)))

  p/Multiplication
  (multiply [z w]
    (cond (number? w)
          (complex (* x w) (* y w))
          (infinity? w) (if (p/zero? z) undefined infinity)
          :else
          (complex (+ (* x (:x w))
                      (- (* y (:y w))))
                   (+ (* x (:y w))
                      (* y (:x w))))))
  (reciprocal [z]
    (if (p/zero? z) infinity
        (let [r (+ (* x x) (* y y))
              r-recip (/ r)]
          (complex (* x r-recip)
                   (* (- y) r-recip)))))
  (one? [_]
    (and (== x 1) (== y 0)))

  p/Conjugate
  (conjugate [_]
    (complex x (- y)))

  p/Equality
  (equals? [_ w]
    (and (== x (:x w))
         (== y (:y w))))
  (almost-equals? [_ w epsilon]
    (and (almost-equals? x (:x w) epsilon)
         (almost-equals? y (:y w) epsilon)))

  p/Evaluate
  (evaluate [_]
    [x y]))

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

(defn angle [complex]
  (mod-tau (Math/atan2 (:y complex) (:x complex))))

(defn angle->complex
  ([angle] (angle->complex angle 1))
  ([angle length]
   (complex
    (* length (Math/cos (deg->rad angle)))
    (* length (Math/sin (deg->rad angle))))))

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
    (almost-equals? 0 (mod (- angle (:angle u)) 360) epsilon)))

(defn unit [angle]
  (->Unit angle))

(comment
  (require '[turtle-geometry.number] :reload)
  (in-ns 'turtle-geometry.number)
  )
