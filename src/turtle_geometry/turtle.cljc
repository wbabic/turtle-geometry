(ns turtle-geometry.turtle
  "A transformable turtle making use of geometric transformations"
  (:require
   [turtle-geometry.protocols :as p]
   [turtle-geometry.geometry :as g]))

(defrecord Turtle [position heading orientation])

(extend-protocol p/Turtle
  Turtle
  (move [{position :position heading :heading :as turtle} d]
    (update-in turtle [:position]
               #(p/transform %
                             (g/->Translation
                              (p/add
                               (:point position)
                               (p/multiply (:vector heading) d))))))
  (turn [turtle a]
    (update-in turtle [:heading]
               #(p/transform % (g/->Rotation a))))
  (resize [turtle r]
    (update-in turtle [:heading]
               #(p/transform % (g/->Dilation r))))
  (reflect [turtle]
    (update-in turtle [:orientation]
               #(p/transform % (g/->Reflection)))))

(extend-protocol p/Transformable
  Turtle
  (p/transform [turtle transformation]
    (-> turtle
        (update-in [:position]    #(p/transform % transformation))
        (update-in [:heading]     #(p/transform % transformation))
        (update-in [:orientation] #(p/transform % transformation)))))