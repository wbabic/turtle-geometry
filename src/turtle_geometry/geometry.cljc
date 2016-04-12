(ns turtle-geometry.geometry
  "basic geometric objects and transforms provide functions of complex number"
  (:require [turtle-geometry.protocols :as p]))

;; primitive geometric objects
(defrecord Point [point]
  p/Complex
  (complex [_] point)

  p/Equality
  (p/equals? [_ p]
    (p/equals? point (:point p))))

(defrecord Vector [vector]
  p/Complex
  (complex [_] vector)
  p/Equality
  (p/equals? [_ v]
    (p/equals? vector (:vector v))))

(defrecord Orientation [value]
  p/Orientation
  (value [_] value)
  (keyword [_] (if (= value 1)
                 :counter-clockwise
                 :clockwise))
  p/Equality
  (p/equals? [_ o]
    (= value (p/value o))))

;; primitive geometric transforms
(defrecord Translation [vector]
  p/Transform
  (p/inverse [_] (->Translation (p/negative vector)))
  (p/transform-fn [_]
    #(p/add % vector)))

(defrecord Rotation [angle]
  p/Transform
  (p/inverse [_] (->Rotation (- angle)))
  (p/transform-fn [_]
    #(p/multiply % (p/unit angle))))

(defrecord Dilation [ratio]
  p/Transform
  (p/inverse [_] (->Dilation (/ ratio)))
  (p/transform-fn [_]
    #(p/multiply % ratio)))

(defrecord Affine [a b]
  p/Transform
  (p/inverse [_]
    (let [c (p/reciprocal a)
          d (p/multiply (p/negative b) (p/reciprocal a))]
      (->Affine c d)))
  (p/transform-fn [_]
    #(p/add (p/multiply % a) b)))

(defrecord Reflection []
  p/Transform
  (p/inverse [reflection] reflection)
  (p/transform-fn [_]
    #(p/conjugate %)))

(defrecord Composition [sequence]
  p/Transform
  (p/inverse [_]
    (->Composition (reverse (map p/inverse sequence))))
  (p/transform-fn [_]
    (apply comp (reverse (map p/transform-fn sequence)))))

;; todo
(defrecord Mobius [a b c d])
(defrecord Inversion [])
(defrecord Reciprocal [])

(defn mobius
  [a b c d] (->Mobius a b c d))

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
      ;; only a reflection changes orientation (and inversion)
      (update-in orientation [:value] #(* -1 %))
      Composition
      (reduce
       (fn [orien trans]
         (p/transform orien trans))
       orientation
       (:sequence transformation))
      orientation)))

(defn conjugate
  "conjugate of transformation g by trasnformation f"
  [f g]
  (->Composition
   (list (p/inverse f) g f)))

(defn rotation
  ([angle] (->Rotation angle))
  ([p angle] (conjugate (->Translation p) (->Rotation angle))))

(defn dilation
  ([ratio] (->Dilation ratio))
  ([p ratio] (conjugate (->Translation p) (->Dilation ratio))))
