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

(defn orientation
  "constructor function for orientation"
  ([] (orientation 1))
  ([value] (->Orientation value)))

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
  "conjugate of transformation g by transformation f"
  [f g]
  (->Composition
   (list (p/inverse f) g f)))

(defn rotation
  ([angle] (->Rotation angle))
  ([p angle] (conjugate (->Translation p) (->Rotation angle))))

(defn dilation
  ([ratio] (->Dilation ratio))
  ([p ratio] (conjugate (->Translation p) (->Dilation ratio))))

(defn toggle [conj]
  (if (true? conj) false true))

(defn reduce-triple
  "apply a transform to triple"
  [transform [a b conj]]
  (condp instance? transform
    Reflection
    [(p/conjugate a) (p/conjugate b) (toggle conj)]
    Dilation
    (let [r (:ratio transform)]
      [(p/multiply a r) (p/multiply b r) conj])
    Rotation
    (let [angle (:angle transform)
          w (p/unit angle)]
      [(p/multiply a w) (p/multiply b w) conj])
    Translation
    (let [v (:v transform)]
      [a (p/add b v) conj])
    Affine
    (let [{:keys [a1 b1]} transform
          c (p/multiply a a1)
          d (p/add (p/multiply b a1) b1)]
      [c d conj])
    Composition
    (let [sequence (:sequence transform)]
      (reduce
       (fn [triple transform]
         (reduce-triple transform triple))
       [a b conj]
       sequence))))

(defn reduce-composition
  "reduce a composition into a single transformation"
  [composition identity-triple]
  (let [sequence (:sequence composition)
        [a b conj] (reduce-triple composition identity-triple)
        affine (->Affine a b)]
    (if (false? conj)
      affine
      (->Composition (list affine (->Reflection))))))
