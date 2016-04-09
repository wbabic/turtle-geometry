(ns turtle-geometry.turtle
  "A transformable turtle making use of geometric transformations"
  (:require
   [turtle-geometry.protocols :as p]
   [turtle-geometry.geometry :as g]))

(defrecord Turtle [position heading orientation]
  p/Turtle
  (p/move [{position :position heading :heading :as turtle} d]
    (update-in turtle [:position]
               #(p/transform %
                             (g/->Translation
                              (p/add
                               (:point position)
                               (p/multiply (:vector heading) d))))))
  (p/turn [turtle a]
    (update-in turtle [:heading]
               #(p/transform % (g/->Rotation a))))
  (p/resize [turtle r]
    (update-in turtle [:heading]
               #(p/transform % (g/->Dilation r))))
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
