(ns turtle-geometry.geometry
  "transforms and transformables using complex numbers"
  (:require [turtle-geometry.protocols :as p]))

(defrecord Point [z])
(defrecord Vector [z])
(defrecord Orientation [keyword])

;; geometric transformations as data
;; using complex numbers
(defrecord Reflection []
  p/Transform
  (p/inverse [reflection] reflection)
  (p/transform-fn [reflection]
    #(p/conjugate %)))

(defrecord Dilation [ratio]
  p/Transform
  (p/inverse [{:keys [ratio]}] (->Dilation (/ ratio)))
  (p/transform-fn [{:keys [ratio]}]
    #(p/multiply % ratio)))

(defrecord Rotation [angle]
  p/Transform
  (p/inverse [rotation] (->Rotation (- (:angle rotation))))
  (p/transform-fn [rotation]
    #(p/multiply % (p/unit (:angle rotation)))))

(defrecord Translation [vecctor]
  p/Transform
  (p/inverse [translation] (->Translation (p/negative (:v translation))))
  (p/transform-fn [translation]
    #(p/add % (:vector translation))))

(defrecord Affine [a b]
  p/Transform
  (p/inverse [{:keys [a b]}]
    (let [c (p/reciprocal a)
          d (p/multiply (p/negative b) (p/reciprocal a))]
      (->Affine c d)))
  (p/transform-fn [{:keys [a b]}]
    #(p/add (p/multiply a %) b)))

(defrecord Composition [sequence]
  p/Transform
  (p/inverse [{sequence :sequence}]
    (->Composition (reverse (map p/inverse sequence))))
  (p/transform-fn [{sequence :sequence}]
    (apply comp (reverse (map p/transform-fn sequence)))))

(defrecord Mobius [a b c d])
(defrecord Inversion [])
(defrecord Reciprocal [])

(defn mobius
  [a b c d] (->Mobius a b c d))

(defn toggle-orientation [orientation]
  (if (= orientation :counter-clockwise)
    :clockwise
    :counter-clockwise))

(extend-protocol p/Transformable
  Vector
  (p/transform [vector transformation]
    (let [f (p/transform-fn transformation)]
      (condp instance? transformation
        Translation
        ;; translation does not effect vectors
        vector
        (update-in vector [:z] (p/transform-fn transformation)))))
  Point
  (p/transform [point transformation]
    (update-in point [:z] (p/transform-fn transformation)))

  Orientation
  (p/transform [orientation transformation]
    (condp instance? transformation
      Reflection
      ;; only reflect can change orientation (and inversion)
      (update-in orientation [:keyword] toggle-orientation)
      Composition
      (reduce
       (fn [orien trans]
         (p/transform orien trans))
       orientation
       (:sequence transformation))
      orientation)))

(comment
  (require '[turtle-geometry.geometry] :reload)
  (in-ns 'turtle-geometry.geometry)
  )
