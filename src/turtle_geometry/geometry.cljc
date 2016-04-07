(ns turtle-geometry.geometry
  "basic geometric objects and transforms provide functions of complex number"
  (:require [turtle-geometry.protocols :as p]))

;; primitive geometric objects
(defrecord Point [point])
(defrecord Vector [vector])
(defrecord Orientation [keyword])

;; primitive geometric transforms
(defrecord Translation [vector]
  p/Transform
  (p/inverse [translation] (->Translation (p/negative (:v translation))))
  (p/transform-fn [translation]
    #(p/add % (:vector translation))))

(defrecord Rotation [angle]
  p/Transform
  (p/inverse [rotation] (->Rotation (- (:angle rotation))))
  (p/transform-fn [rotation]
    #(p/multiply % (p/unit (:angle rotation)))))

(defrecord Dilation [ratio]
  p/Transform
  (p/inverse [{:keys [ratio]}] (->Dilation (/ ratio)))
  (p/transform-fn [{:keys [ratio]}]
    #(p/multiply % ratio)))

(defrecord Affine [a b]
  p/Transform
  (p/inverse [{:keys [a b]}]
    (let [c (p/reciprocal a)
          d (p/multiply (p/negative b) (p/reciprocal a))]
      (->Affine c d)))
  (p/transform-fn [{:keys [a b]}]
    #(p/add (p/multiply a %) b)))

(defrecord Reflection []
  p/Transform
  (p/inverse [reflection] reflection)
  (p/transform-fn [reflection]
    #(p/conjugate %)))

(defrecord Composition [sequence]
  p/Transform
  (p/inverse [{sequence :sequence}]
    (->Composition (reverse (map p/inverse sequence))))
  (p/transform-fn [{sequence :sequence}]
    (apply comp (reverse (map p/transform-fn sequence)))))

;; todo
(defrecord Mobius [a b c d])
(defrecord Inversion [])
(defrecord Reciprocal [])

(defn mobius
  [a b c d] (->Mobius a b c d))

(defn toggle-orientation [orientation]
  (if (= orientation :counter-clockwise)
    :clockwise
    :counter-clockwise))

;; implementation of Transformable protocol for
;; primitive geometric objects
(extend-protocol p/Transformable
  Vector
  (p/transform [vector transformation]
    (condp instance? transformation
      Translation
      vector
      (update-in vector [:vector] (p/transform-fn transformation))))
  Point
  (p/transform [point transformation]
    (update-in point [:point] (p/transform-fn transformation)))

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
