(ns turtle-geometry.geometry
  "basic geometric objects and transforms provide functions of complex number"
  (:require [turtle-geometry.protocols :as p]
            [turtle-geometry.number :as n])
  (:refer-clojure :exclude [zero? keyword vector]))

;; primitive geometric transforms
(declare translation rotation dilation affine compose position orientation reflection)

(defrecord Translation [vector]
  p/Transform
  (p/inverse [_] (translation (p/negative vector)))
  (p/transform-fn [_]
    #(p/add % vector)))

(defrecord Rotation [unit]
  p/Transform
  (p/inverse [_] (rotation (p/conjugate unit)))
  (p/transform-fn [_]
    #(p/multiply % (p/angle->complex unit))))

(defrecord Dilation [ratio]
  p/Transform
  (p/inverse [_] (dilation (/ ratio)))
  (p/transform-fn [_]
    #(p/multiply % ratio)))

(defrecord Affine [a b]
  p/Transform
  (p/inverse [_]
    (let [c (p/reciprocal a)
          d (p/multiply (p/negative b) (p/reciprocal a))]
      (affine c d)))
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
    (apply compose (reverse (map p/inverse sequence))))
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
        (== 1 (:ratio t))
        false))))

(defn compose
  "compose transformations"
  ([] Identity)
  ([t] t)
  ([t & ts]
   (->Composition (conj ts t))))

(defn prepend [composition & transforms]
  (apply compose (concat transforms (:sequence composition))))

(defrecord Reciprocal []
  p/Transform
  (p/inverse [reciprocal] reciprocal)
  (p/transform-fn [_]
    #(p/reciprocal %)))

(def Inversion (compose (->Reciprocal) (->Reflection)))

;; todo
(defrecord Mobius [a b c d])

(defn conjugate
  "conjugate of transformation g by transformation f"
  [f g]
  (->Composition
   (list (p/inverse f) g f)))

(defn translation
  [z] (->Translation z))

(defn rotation
  ([unit] (->Rotation unit))
  ([p unit] (conjugate (->Translation p) (->Rotation unit))))

(defn dilation
  ([ratio] (->Dilation ratio))
  ([p ratio] (conjugate (->Translation p) (->Dilation ratio))))

(defn reflection
  ([] (->Reflection))
  ([point heading]
   (let [f (compose (rotation heading) (translation point))]
     (conjugate f (->Reflection)))))

(defn affine
  [c d]
  (->Affine c d))

(defn inversion
  ([] Inversion)
  ([center radius]
   (let [f (compose (dilation radius) (translation center))]
     (conjugate f Inversion))))

(defn mobius
  [a b c d] (->Mobius a b c d))

;; primitive geometric objects
(defrecord Position [complex]
  p/Position
  (point [_] complex)

  p/Transformable
  (transform [_ transformation]
    (position ((p/transform-fn transformation) complex)))

  p/Equality
  (equals? [_ p]
    (p/equals? complex (:complex p)))
  (almost-equals? [_ p epsilon]
    (p/almost-equals? complex (:complex p) epsilon)))

(defn position
  "represent a position by a complex number z"
  [z]
  (->Position z))

(declare vector)

(defrecord Vector [complex]
  p/Heading
  (angle [_] (n/rad->deg (n/angle complex)))
  (length [_] (n/length complex))
  (vector [_] complex)

  p/Transformable
  (transform [v transformation]
    (condp instance? transformation
      Dilation (vector (p/multiply complex (:ratio transformation)))
      Rotation (let [u (n/unit (get-in transformation
                                       [:unit :angle]))]
                 (vector (p/multiply complex (p/angle->complex u))))
      Translation v
      Reflection (vector (p/conjugate complex))
      Composition
      (let [transformations (:sequence transformation)]
        (reduce
         (fn [v trans] (p/transform v trans))
         v
         transformations))))

  p/Equality
  (equals? [_ h]
    (p/equals? complex (p/vector h)))
  (almost-equals? [_ h epsilon]
    (p/almost-equals? complex (p/vector h) epsilon)))

(defn vector
  "represent a vector by a complex number z"
  [z]
  (->Vector z))

;; abstract heading
(defrecord Heading [unit length]
  p/Heading
  (angle [_] (:angle unit))
  (length [_] length)
  (vector [_] (n/angle->complex (:angle unit) length))

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
         transformations))
      (do
        (println "no transform defined for heading for " transformation)
        heading)))

  p/Equality
  (equals? [_ h]
    (and (== length (:length h))
         (p/equals? unit (:unit h))))
  (almost-equals? [_ h epsilon]
    (and (n/almost-equals? length (:length h) epsilon)
         (p/almost-equals? unit (:unit h) epsilon))))

(defn heading
  ([] (heading (n/unit 0)))
  ([unit] (heading unit 1))
  ([unit length]
   (->Heading unit length)))

;; operations with positions and vectors
(defn add
  "add a position and a heading returning a position
  or add two headings returning another heading"
  [p v]
  (assert (and (or (satisfies? p/Position p)
                   (satisfies? p/Heading p))
               (satisfies? p/Heading v)))
  (cond
    (satisfies? p/Position p)
    (let [z (p/point p)
          v1 (p/vector v)]
      (position (p/add z v1)))

    (satisfies? p/Heading p)
    (let [v1 (p/vector p)
          v2 (p/vector v)]
      (vector (p/add v1 v2)))))

(defn difference
  "difference between two positions returning a vector"
  [p1 p2]
  (assert (and (satisfies? p/Position p1)
               (satisfies? p/Position p2)))
  (vector (p/add (p/point p2)
                 (p/negative (:complex p2)))))

(defrecord Orientation [value]
  p/Orientation
  (value [_] value)
  (keyword [_] (if (= value 1)
                 :counter-clockwise
                 :clockwise))

  p/Transformable
  (transform [o transformation]
    (condp instance? transformation
      Reflection (orientation (- value))
      Composition
      (reduce
       (fn [orien trans]
         (p/transform orien trans))
       o
       (:sequence transformation))
      o))

  p/Equality
  (p/equals? [_ o]
    (= value (p/value o))))

(defn orientation
  "constructor function for orientation"
  ([] (orientation 1))
  ([value] (->Orientation value)))

(declare circle)

(defrecord Circle [center radius]
  p/Transformable
  (transform [_ transformation]
    (circle (p/transform center transformation)
            (p/transform radius transformation))))

(defrecord GeneralizedCircle [p1 p2 p3])

(defn circle
  "create circle at center position
  with given radius vector"
  [center radius]
  (->Circle center radius))

(declare polygon)

(defrecord Polygon [positions]
  p/Transformable
  (transform [_ transformation]
    (polygon (map #(p/transform % transformation) positions))))

(defn polygon [positions]
  (->Polygon positions))

;; general equation of a line is
;; az + a-bar*z-bar + b = 0

(defn bar [z] (p/conjugate z))

(defrecord Line-General [a b c])

(declare line-segment)
(declare param-line)

(defrecord LineSegment [p1 p2]
  p/Transformable
  (transform [l transformation]
    (apply line-segment (map #(p/transform % transformation) [p1 p2])))

  p/Parameterized
  (value-for [_ parameter]
    (let [z1 (p/point p1)
          z2 (p/point p2)
          l (param-line z1 z2)]
      (position (l parameter)))))

(defn line-segment
  "create a line segment given two complex numbers"
  [z1 z2]
  (->LineSegment z1 z2))

(defn line-test [{:keys [a b c]}]
  (fn [z] (n/add
           (n/multiply a z)
           (n/multiply b (bar z))
           c)))

(defn on-line [line point]
  (p/equals? n/zero ((line-test line) point)))

(defn line
  "returns line through two points"
  [z w]
  (let [a (n/minus (bar z) (bar w))
        b (n/minus w z)
        c (n/minus (p/multiply z (bar w))
                   (p/multiply (bar z) w))]
    (->Line-General a b c)))

(defn param-line
  "returns a parameterized line"
  [z w]
  (fn [t]
    (p/add (p/multiply z (- 1 t))
           (p/multiply w t))))

(defn steps
  "n steps from 0 to 1"
  [n]
  (let [step (/ n)
        f #(+ % step)]
    (take (inc n) (iterate f 0))))

(defn perp-line
  "returns perpendicular bisector of line through two points"
  [z w]
  (let [a (n/minus (bar w) (bar z))
        b (n/minus w z)
        c (n/minus (p/multiply z (bar z))
                   (p/multiply w (bar w)))]
    (->Line-General a b c)))

(defn circumcenter
  "returns the circumcenter of triangle abc"
  [a b c]
  (let [ab (n/minus a b)
        bc (n/minus b c)
        ca (n/minus c a)
        numer (n/add (p/multiply bc (n/length-sq a))
                     (p/multiply ca (n/length-sq b))
                     (p/multiply ab (n/length-sq c)))
        denom (n/add (p/multiply bc (bar a))
                     (p/multiply ca (bar b))
                     (p/multiply ab (bar c)))]
    (n/divide numer denom)))

(defn circumcircle
  "returns the circumcircle of three given positions"
  [a b c]
  (let [p (circumcenter a b c)
        r (n/minus p a)]
    (circle (position p) (vector r))))

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
            w (p/angle->complex angle)]
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
