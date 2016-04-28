(ns turtle-geometry.turtle.twenty-four-fold
  "a turtle implementation using complex numbers and rational roots
  providing exact representation for rotations in multiples of 15 degrees
  a twenty four fold turtle"
  (:require [turtle-geometry.protocols :as p]
            [turtle-geometry.geometry :as g]
            [turtle-geometry.turtle :as t]
            [turtle-geometry.number.complex :as n]
            [turtle-geometry.number.units.twenty-four :as units :refer [unit]])
  (:import  [turtle_geometry.turtle Heading]))

(defn turtle
  "twenty-four-fold turtle constructor"
  ([] (turtle (t/position n/zero) (t/heading (unit 0)) (t/orientation)))
  ([point] (turtle point (t/heading (unit 0)) (t/orientation)))
  ([point heading] (turtle point heading (t/orientation)))
  ([point heading orientation]
   (t/->Turtle point heading orientation)))

(def initial-turtle (turtle))

(comment
  (require '[turtle-geometry.turtle.twenty-four-fold] :reload)
  (in-ns 'turtle-geometry.turtle.twenty-four-fold)

  (clojure.pprint/pprint (t/heading (unit 15)))
  (p/vector (t/heading (unit 15)))

  (clojure.pprint/pprint (p/vector (t/heading (unit 15))))

  (t/display-turtle initial-turtle)
  (t/display-turtle (p/move initial-turtle 10))
  (-> initial-turtle
      (p/turn 15)
      (p/move 10)
      :position
      :complex :x)
  #turtle_geometry.number.root.RationalRoot
  {:ratio 0,
   :roots (#turtle_geometry.number.root.Root
           {:base 2, :multiplier 5/2}
           #turtle_geometry.number.root.Root
           {:base 6, :multiplier 5/2})}

  ;; move a turtle about
  (-> initial-turtle
      (p/resize 2)
      (p/turn 15)
      (p/move 10)
      (p/reflect)
      t/display-turtle)
  ;;=>
  {:position [9.659258262890683 2.5881904510252074],
   :heading {:length 2, :angle -15},
   :orientation :clockwise}

  (let [transformed-turtle (-> initial-turtle
                               (p/turn 15)
                               (p/resize 10)
                               (p/move 1)
                               (p/reflect))]
    (clojure.pprint/pprint
     (t/turtle->home transformed-turtle)))
  )

(comment

  (p/transform (:position initial-turtle) (g/->Dilation 2))
  (p/transform (:heading initial-turtle) (g/->Dilation 2))
  (p/transform (:orientation initial-turtle) (g/->Dilation 2))

  (p/transform (:position initial-turtle) (g/->Rotation (unit 45)))
  (p/transform (:heading initial-turtle) (g/->Rotation (unit 45)))
  (p/transform (:orientation initial-turtle) (g/->Rotation (unit 45)))

  ;; simple transforms of a turtle in the plane
  (clojure.pprint/pprint
   (let [t0 (-> (turtle) (p/turn 90) (p/move 3))
         t1 (p/transform t0 (g/->Reflection))
         t2 (p/transform t0 (g/->Dilation 2))
         t3 (p/transform t0 (g/->Translation (n/complex 2 0)))
         t4 (p/transform t0 (g/->Rotation (unit 90)))]
     (mapv t/display-turtle [t0 t1 t2 t3 t4])))
  ;; =>
  [{:position [0 3],
    :heading {:length 1, :angle 90},
    :orientation :counter-clockwise}
   {:position [0 -3],
    :heading {:length 1, :angle -90},
    :orientation :clockwise}
   {:position [0 6],
    :heading {:length 2, :angle 90},
    :orientation :counter-clockwise}
   {:position [2 3],
    :heading {:length 1, :angle 90},
    :orientation :counter-clockwise}
   {:position [-3 0],
    :heading {:length 1, :angle 180},
    :orientation :counter-clockwise}]

  ;; some simple turtle-centric transformations
  (let [t0 (turtle)
        t1 (-> t0 (p/move 3) (p/turn 90))
        f (t/turtle->home t1)
        g (t/turtle-centric-transformation t1 (g/->Reflection))
        h (t/turtle-centric-transformation t1 (g/->Rotation (unit -90)))
        t2 (p/transform t1 f)
        t3 (p/transform t0 g)
        t4 (p/transform t0 h)]
    (mapv t/display-turtle [t0 t1 t2 t3 t4]))
  ;;=>
  [{:position [0 0], :heading {:length 1, :angle 0}, :orientation :counter-clockwise}
   {:position [3 0], :heading {:length 1, :angle 90}, :orientation :counter-clockwise}
   {:position [0 0], :heading {:length 1, :angle 0}, :orientation :counter-clockwise}
   {:position [6 0], :heading {:length 1, :angle 180}, :orientation :clockwise}
   {:position [3 3], :heading {:length 1, :angle -90}, :orientation :counter-clockwise}]
  )
