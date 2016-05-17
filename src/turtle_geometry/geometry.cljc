(ns turtle-geometry.geometry
  "basic geometric objects and transforms provide functions of complex number"
  (:require [turtle-geometry.protocols :as p]))

;; primitive geometric objects

;; primitive geometric transforms
(defrecord Translation [vector]
  p/Transform
  (p/inverse [_] (->Translation (p/negative vector)))
  (p/transform-fn [_]
    #(p/add % vector)))

(defrecord Rotation [unit]
  p/Transform
  (p/inverse [_] (->Rotation (p/conjugate unit)))
  (p/transform-fn [_]
    #(p/multiply % (p/angle->complex unit))))

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

(def Identity
  (reify
    p/Transform
    (inverse [i] i)
    (transform-fn [_]
      identity)
    p/Equality
    (equals? [_ t]
      (condp instance? t
        Rotation
        (== 0 (mod (-> t :unit :angle) 360))
        Translation
        (p/zero? (:vector t))
        Dilation
        (p/one? (:ratio t))
        false))))

(defn compose
  "compose transformations"
  ([] Identity)
  ([t] t)
  ([t & ts]
   (->Composition (conj ts t))))

(defrecord Reciprocal []
  p/Transform
  (p/inverse [reciprocal] reciprocal)
  (p/transform-fn [_]
    #(p/reciprocal %)))

(def Inversion (compose (->Reciprocal) (->Reflection)))

;; todo
(defrecord Mobius [a b c d])

(defn mobius
  [a b c d] (->Mobius a b c d))

(defn conjugate
  "conjugate of transformation g by transformation f"
  [f g]
  (->Composition
   (list (p/inverse f) g f)))

(defn rotation
  ([unit] (->Rotation unit))
  ([p unit] (conjugate (->Translation p) (->Rotation unit))))

(defn dilation
  ([ratio] (->Dilation ratio))
  ([p ratio] (conjugate (->Translation p) (->Dilation ratio))))

(defn translation
  [z] (->Translation z))

(defn reflection
  ([] (->Reflection))
  ([point heading]
   (let [f (compose (rotation heading) (translation point))]
     (conjugate f (->Reflection)))))

(defn inversion
  ([] Inversion)
  ([center radius]
   (let [f (compose (dilation radius) (translation center))]
     (conjugate f Inversion))))

(comment
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
            w (p//angle->complex angle)]
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
        (->Composition (list affine (->Reflection)))))))

;; geometry

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
