(ns turtle-geometry.turtle
  "A transformable turtle making use of geometric transformations"
  (:require
   [turtle-geometry.protocols :as p]
   [turtle-geometry.geometry :as g])
  (:import  [turtle_geometry.geometry Translation Rotation Dilation Reflection Composition]))

(defrecord Position [complex]
  p/Position
  (point [_] complex)

  p/Transformable
  (transform [_ transformation]
    (->Position ((p/transform-fn transformation) complex)))

  p/Equality
  (equals? [_ p]
    (p/equals? complex (:complex p)))
  (almost-equals? [_ p epsilon]
    (p/almost-equals? complex (:complex p) epsilon)))

(defn position
  "represent a position by a complex number z"
  [z]
  (->Position z))

;; abstract heading - needs complex to be implemented
(defrecord Heading [unit length]
  p/Heading
  (angle [_] (:angle unit))
  (length [_] length)
  (vector [_] (p/angle->complex unit))

  p/Transformable
  (transform [heading transformation]
    (condp instance? transformation
      Dilation (update-in heading [:length] #(* % (:ratio transformation)))
      Rotation (update-in heading [:unit :angle] #(+ % (get-in transformation
                                                               [:unit :angle])))
      Translation heading
      Reflection (update-in heading [:unit :angle] #(- %))
      Composition
      (let [transformations (:sequence transformation)]
        (reduce
         (fn [turtle trans] (p/transform turtle trans))
         heading
         transformations))))

  p/Equality
  (equals? [_ h]
    (and (== length (:length h))
         (p/equals? unit (:unit h))))
  (almost-equals? [_ h epsilon]
    (and (p/almost-equals? length (:length h) epsilon)
         (p/almost-equals? unit (:unit h) epsilon))))

(defn heading
  ([unit] (heading unit 1))
  ([unit length]
   (->Heading unit length)))

(defrecord Orientation [value]
  p/Orientation
  (value [_] value)
  (keyword [_] (if (= value 1)
                 :counter-clockwise
                 :clockwise))

  p/Transformable
  (transform [orientation transformation]
    (condp instance? transformation
      Reflection (->Orientation (- value))
      Composition
      (reduce
       (fn [orien trans]
         (p/transform orien trans))
       orientation
       (:sequence transformation))
      orientation))

  p/Equality
  (p/equals? [_ o]
    (= value (p/value o))))

(defn orientation
  "constructor function for orientation"
  ([] (orientation 1))
  ([value] (->Orientation value)))

(defrecord Turtle [position heading orientation]
  p/Turtle
  (p/move [turtle distance]
    (let [v (p/multiply (p/angle->complex (:unit heading))
                        (* (:length heading) distance))]
      (update-in turtle [:position :complex] #(p/add % v))))
  (p/turn [turtle angle]
    (update-in turtle [:heading :unit :angle] #(+ % angle)))
  (p/resize [turtle ratio]
    (update-in turtle [:heading :length]
               #(* % ratio)))
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
