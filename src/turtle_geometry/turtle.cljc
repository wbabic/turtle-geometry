(ns turtle-geometry.turtle
  "A transformable turtle making use of geometric transformations"
  (:require
   [turtle-geometry.protocols :as p]
   [turtle-geometry.geometry :as g])
  (:import  [turtle_geometry.geometry Translation Rotation Dilation Reflection Composition]))

(defrecord Turtle [position heading orientation]
  p/Turtle
  (p/move [turtle distance]
    (let [v (p/multiply (p/angle->complex (:unit heading))
                        (* (:length heading) distance))]
      (update-in turtle [:position :complex] #(p/add % v))))
  (p/turn [turtle angle]
    (update-in turtle [:heading :unit :angle] #(+ % (* angle (:value orientation)))))
  (p/resize [turtle ratio]
    (update-in turtle [:heading :length] #(* % ratio)))
  (p/reflect [turtle]
    (-> turtle
        (update-in  [:heading :unit :angle] #(- %))
        (update-in  [:orientation :value] #(- %))))

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
  {:position (p/evaluate (p/point position))
   :heading {:length (p/length heading) :angle (p/angle heading)}
   :orientation (p/keyword orientation)})

(defn home->turtle
  "the transformation that brings the home turtle to the given turtle"
  [{:keys [position heading orientation]}]
  (let [ts (list
            (g/->Rotation    (:unit heading))
            (g/->Dilation    (:length heading))
            (g/->Translation (:complex position)))]
    (if (= :counter-clockwise (p/keyword orientation))
      (apply g/compose ts)
      (apply g/compose (g/->Reflection) ts))))

(defn turtle->home
  "the transformation that brings the given turtle home"
  [turtle]
  (p/inverse (home->turtle turtle)))

(defn turtle-centric-transformation
  "perform given transformation
  wrt given turtle"
  [turtle trans]
  (g/conjugate (home->turtle turtle) trans))
