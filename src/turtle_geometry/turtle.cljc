(ns turtle-geometry.turtle
  "A transformable turtle making use of geometric transformations"
  (:require
   [turtle-geometry.protocols :as p]
   [turtle-geometry.number.unit :as u]
   [turtle-geometry.geometry :as g])
  (:import  [turtle_geometry.geometry Translation Rotation Dilation Reflection Composition]))

;; abstract heading - needs complex to be implemented
(defrecord Heading [unit length]
  p/Heading
  (angle [_] (:angle unit))
  (length [_] length)

  p/Transformable
  (transform [heading transformation]
    (condp instance? transformation
      Dilation
      (update-in heading [:length] #(* % (:ratio transformation)))
      Rotation
      (update-in heading [:unit] #(p/multiply % (u/unit (:angle transformation))))
      Translation
      heading
      Reflection
      (update-in heading [:unit] #(p/conjugate %))
      Composition
      (let [transformations (:sequence transformation)]
        (reduce
         (fn [turtle trans] (p/transform turtle trans))
         heading
         transformations))))

  p/Equality
  (equals? [_ h]
    (and (== length (p/length h))
         (p/equals? unit (:unit h))))
  (almost-equals? [_ h epsilon]
    (and (p/almost-equals? unit (:unit h) epsilon))))

(defn heading
  ([] (heading (u/unit)))
  ([unit] (heading unit 1))
  ([unit length]
   (if (number? unit)
     (->Heading (u/unit unit) length)
     (->Heading unit length))))

(defrecord Turtle [position heading orientation]
  p/Turtle
  (p/move [{position :position heading :heading :as turtle} distance]
    (update-in turtle [:position]
               #(p/transform %
                             (g/->Translation
                              (p/multiply (p/complex heading) distance)))))
  (p/turn [turtle angle]
    (update-in turtle [:heading]
               #(p/transform % (g/->Rotation angle))))
  (p/resize [turtle ratio]
    (update-in turtle [:heading]
               #(p/transform % (g/->Dilation ratio))))
  (p/reflect [turtle]
    (update-in turtle [:orientation]
               #(p/transform % (g/->Reflection))))

  p/Transformable
  (p/transform [turtle transformation]
    (-> turtle
        (update-in [:position]    #(p/transform % transformation))
        (update-in [:heading]     #(p/transform % transformation))
        (update-in [:orientation] #(p/transform % transformation))))

  p/Equality
  (equals? [_ turtle]
    (and (p/equals? position (:position turtle))
         (p/equals? heading (:heading turtle))
         (p/equals? orientation (:orientation turtle))))
  (almost-equals? [_ turtle epsilon]
    (and (p/almost-equals? heading (:heading turtle) epsilon)
         (p/almost-equals? position (:position turtle) epsilon)
         (p/equals? orientation (:orientation turtle)))))

(defn display-turtle
  [{:keys [position heading orientation]}]
  {:position (p/evaluate (p/complex position))
   :heading {:length (p/length heading) :angle (p/angle heading)}
   :orientation (p/keyword orientation)})

(defn home->turtle
  "the transformation that brings the home turtle to the given turtle"
  [{:keys [position heading orientation]}]
  (if (= :counter-clockwise (p/keyword orientation))
    (g/->Composition
     (list
      (g/->Rotation (p/angle heading))
      (g/->Dilation (p/length heading))
      (g/->Translation (p/complex position))))
    (g/->Composition
     (list
      (g/->Reflection)
      (g/->Rotation (p/angle heading))
      (g/->Dilation (p/length heading))
      (g/->Translation (p/complex position))))))

(defn turtle->home
  "the transformation that brings the given turtle home"
  [turtle]
  (p/inverse (home->turtle turtle)))

(defn turtle-centric-transformation
  "perform given transformation
  wrt given turtle"
  [turtle trans]
  (g/conjugate (home->turtle turtle) trans))
