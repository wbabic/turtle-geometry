(ns turtle-geometry.turtle
  "A transformable turtle making use of geometric transformations"
  (:require
   [turtle-geometry.protocols :as p]
   [turtle-geometry.geometry :as g]))

;; heading must implement the Heading protocol
;; position, heading, and orientation must implement the
;; Transformable and Equality protocols

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
  (p/equals? [_ turtle]
    (and (p/equals? position (:position turtle))
         (p/equals? heading (:heading turtle))
         (p/equals? orientation (:orientation turtle)))))

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
