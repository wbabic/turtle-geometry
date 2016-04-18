(ns turtle-geometry.number
  "turtle number"
  (:require [turtle-geometry.protocols :as p]
            [turtle-geometry.number.complex :as n]
            [turtle-geometry.number.unit :as u]))

(defn deg->rad [degrees]
  (* (/ degrees 180) Math/PI))

(defn almost-equals [epsilon x y]
  (< (Math/abs (- x y)) epsilon))

(defn multiply
  ([] n/one)
  ([z] z)
  ([z w] (p/multiply z w))
  ([z w & zs]
   (reduce multiply (multiply z w) zs)))

(defn add
  ([] n/zero)
  ([z] z)
  ([z w] (p/add z w))
  ([z w & zs]
   (reduce add (add z w) zs)))

(comment
  (require '[turtle-geometry.number] :reload)
  (in-ns 'turtle-geometry.number)

  (p/equals? n/one
             (reduce multiply nil))
  (p/equals? (u/unit 90)
             (reduce
              multiply
              (repeat 6 (u/unit 15))))
  (p/equals? n/one (apply multiply (repeat 4 n/i)))
  (p/equals? (u/unit 0) (apply multiply (repeat 24 (u/unit 15))))
  ;;=> true

  (p/evaluate (apply add (repeat 5 n/one)))
  ;;=> [5 0]
  (p/evaluate (apply add (repeat 5 n/i)))
  ;;=> [0 5]
  )
