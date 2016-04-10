(ns turtle-geometry.turtle.exact
  "a turtle implementation using complex numbers and rational roots
  providing exact representation for rotations in multiples of 15 degrees
  a twenty four fold turtle"
  (:refer-clojure :exclude [vector])
  (:require [turtle-geometry.protocols :as p]
            [turtle-geometry.geometry :as g]
            [turtle-geometry.turtle :as t]
            [turtle-geometry.number.complex :as n]))

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

(defn point [z]
  (g/->Point z))

(defn vector [z]
  (g/->Vector z))

(def initial-turtle
  (t/->Turtle
   (point n/zero)
   (vector n/one)
   (g/->Orientation :counter-clockwise)))

(defn display-turtle
  [{:keys [position heading orientation]}]
  {:position (p/evaluate (:point position))
   :heading (p/evaluate (:vector heading))
   :orientation (:keyword orientation)})

(comment
  (require '[turtle-geometry.turtle.exact] :reload)
  (in-ns 'turtle-geometry.turtle.exact)
  (n/unit 15)

  (p/equals? n/one
             (reduce multiply nil))
  (p/equals? n/i
             (reduce
              multiply
              (repeat 6 (n/unit 15))))
  (p/equals? n/one (apply multiply (repeat 4 n/i)))
  (p/equals? n/one (apply multiply (repeat 24 (p/unit 15))))
  ;;=> true

  (p/evaluate (apply add (repeat 5 n/one)))
  ;;=> [5 0]
  (p/evaluate (apply add (repeat 5 n/i)))
  ;;=> [0 5]

  (display-turtle initial-turtle)
  (p/equals? initial-turtle initial-turtle)
  (p/equals? initial-turtle
             (-> initial-turtle
                 (p/turn 15)
                 (p/turn (- 360 15))))
  (p/equals? initial-turtle
             (last (take 25 (iterate #(p/turn % 15) initial-turtle))))
  ;;=>  true
  (p/equals? initial-turtle (p/turn initial-turtle 15))
  ;;=> false

  (display-turtle (p/move initial-turtle 10))
  (-> initial-turtle
      (p/turn 15)
      (p/move 10)
      :position
      :point
      :x)
  #turtle_geometry.number.root.RationalRoot
  {:ratio 0,
   :roots (#turtle_geometry.number.root.Root
           {:base 2, :multiplier 5/2}
           #turtle_geometry.number.root.Root
           {:base 6, :multiplier 5/2})}

  (-> initial-turtle
      (p/resize 2)
      (p/turn 15)
      (p/move 10)
      (p/reflect)
      display-turtle)
  )
