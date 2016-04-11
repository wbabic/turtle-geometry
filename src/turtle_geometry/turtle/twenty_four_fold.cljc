(ns turtle-geometry.turtle.twenty-four-fold
  "a turtle implementation using complex numbers and rational roots
  providing exact representation for rotations in multiples of 15 degrees
  a twenty four fold turtle"
  (:refer-clojure :exclude [vector])
  (:require [turtle-geometry.protocols :as p]
            [turtle-geometry.geometry :as g]
            [turtle-geometry.turtle :as t]
            [turtle-geometry.number.complex :as n]
            [turtle-geometry.number.units.twenty-four :as units])
  (:import  [turtle_geometry.geometry Rotation Dilation Translation]))

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

(defrecord Heading [angle length]
  p/Heading
  (angle [_] angle)
  (length [_] length)
  (complex [_] (p/multiply (units/unit angle) length))

  p/Transformable
  (transform [heading transformation]
    (condp instance? transformation
      Dilation
      (update-in heading [:length] #(* % (:ratio transformation)))
      Rotation
      (update-in heading [:angle] #(+ % (:angle transformation)))
      Translation
      heading))

  p/Equality
  (equals? [_ h]
    (and (== length (p/length h))
         (= 0 (mod (- angle (p/angle h)) 360)))))

(defn heading [angle length]
  (->Heading angle length))

(def initial-turtle
  (t/->Turtle
   (point n/zero)
   (heading 0 1)
   (g/->Orientation :counter-clockwise)))

(defn display-turtle
  [{:keys [position heading orientation]}]
  {:position (p/evaluate (:point position))
   :heading {:length (p/length heading) :angle (p/angle heading)}
   :orientation (:keyword orientation)})

(comment
  (require '[turtle-geometry.turtle.twenty-four-fold] :reload)
  (in-ns 'turtle-geometry.turtle.twenty-four-fold)

  (p/equals? n/one
             (reduce multiply nil))
  (p/equals? n/i
             (reduce
              multiply
              (repeat 6 (units/unit 15))))
  (p/equals? n/one (apply multiply (repeat 4 n/i)))
  (p/equals? n/one (apply multiply (repeat 24 (p/unit 15))))
  ;;=> true

  (p/evaluate (apply add (repeat 5 n/one)))
  ;;=> [5 0]
  (p/evaluate (apply add (repeat 5 n/i)))
  ;;=> [0 5]

  (display-turtle initial-turtle)
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
